namespace FunProbe

open B2R2
open System.Collections.Generic

module BDPSolverHelper =

  let buildTree opts (g: FactorGraph) (edges: HashSet<int * int>) =
    let tree = TreeFactorGraph.init ()
    g.Nodes
    |> Seq.iter (fun (KeyValue (_, node)) ->
      let factor = Node.factor node
      tree.AddShallowFactor factor
      )
    edges
    |> Seq.iter (fun (sid, did) ->
      let addr1 = Node.addr g.Nodes[sid]
      let addr2 = Node.addr g.Nodes[did]
      let f = Factor.init opts ([|addr1; addr2|], TailCallCond)
      tree.AddDeepFactor f
      )
    tree

  let doKruskal (edges: (int * (int * int)) []) (uf: int []) (ufSet: Dictionary<int, Set<int>>) =
    let treeEdges = HashSet<int * int> ()
    Array.iter (fun (_, (v, w)) ->
      let vid = uf[v]
      let wid = uf[w]
      if vid = wid then ()
      else
        treeEdges.Add (v, w) |> ignore
        let vSet = ufSet[vid]
        let wSet = ufSet[wid]
        let aid, bid, aSet, bSet =
          if vid < wid then vid, wid, vSet, wSet else wid, vid, wSet, vSet
        ufSet.Remove bid |> ignore
        let newSet = Set.union aSet bSet
        ufSet[aid] <- newSet
        Set.iter (fun i -> uf[i] <- aid) bSet
      ) edges
    treeEdges

  let addShallowConstraint opts (mgr: ConstraintManager) (tree: FactorGraph) probs idx addr =
    let consts = mgr.ShallowConstraints[addr]
    let w =
      if Array.forall not consts then
        let f = SolverUtils.initBasic opts addr [] |> List.head
        tree.AddShallowFactor f
        -1
      else
        if consts[0] then
          let f = SolverUtils.initFunc opts addr
          tree.AddShallowFactor f
        else
          let _, factors =
            consts[1..]
            |> Array.fold (SolverUtils.combineShallows opts addr) (1, [])
          List.iter (fun f -> tree.AddShallowFactor f) factors
        consts
        |> Array.fold (fun (idx, acc) v ->
            if v then
              let acc =
                if List.contains idx [0; 3; 4; 8; 9; 11; 13; 14] then acc + 1
                else acc - 1
              idx + 1, acc
            else idx + 1, acc) (0, 0)
        |> snd
    (probs: int [])[idx] <- w

  let addDeepConstraint opts (tree: FactorGraph) (addrs: int []) (sidx, didx) =
    let addr = addrs[sidx]
    let addr_ = addrs[didx]
    let f = SolverUtils.initTailCall opts addr addr_
    tree.AddDeepFactor f

  let buildGraph opts (mgr: ConstraintManager) idxs =
    let tree = TreeFactorGraph.init ()
    let addrs = List.toArray idxs
    let idxMap = Dictionary ()
    Array.iteri (fun i idx -> idxMap[idx] <- i) addrs
    let probs = Array.create (Array.length addrs) 0
    Array.iteri (addShallowConstraint opts mgr tree probs) addrs

    let nEdges =
      Array.fold (fun cnt addr ->
        match mgr.DeepConstraints[addr] with
        | Range (sAddr, eAddr) -> cnt + eAddr - sAddr + 1
        | Targets targets -> cnt + targets.Count
        | _ -> cnt) 0 addrs
    let edges = Array.zeroCreate nEdges
    Array.foldi (fun eidx idx addr ->
      match mgr.DeepConstraints[addr] with
      | Range (sAddr, eAddr) ->
        [ sAddr .. eAddr ]
        |> List.fold (fun eidx addr_ ->
          let idx_ = idxMap[addr_]
          let weight = - probs[idx] - probs[idx_]
          edges[eidx] <- weight, (idx_, idx)
          eidx + 1) eidx
      | Targets targets ->
        targets
        |> Seq.fold (fun eidx addr_ ->
          let idx_ = idxMap[addr_]
          let weight = - probs[idx] - probs[idx_]
          edges[eidx] <- weight, (idx_, idx)
          eidx + 1) eidx
      | _ -> eidx
      ) 0 addrs
    |> ignore

    let uf = Array.init (Array.length addrs) (fun i -> i)
    let ufSet = Dictionary<int, Set<int>> ()
    for i in 0 .. Array.length addrs - 1 do
      ufSet[i] <- Set.singleton i
    done
    let edges = Array.sortBy (fun (w, _) -> w) edges
    let treeEdges = doKruskal edges uf ufSet
    Seq.iter (addDeepConstraint opts tree addrs) treeEdges
    tree

// Bogus Dependency Pruning Solver
type BDPSolver (opts, mgr, idxs) =
  inherit Solver ()

  let tree = BDPSolverHelper.buildGraph opts mgr idxs
  let bp = ExactBeliefPropagation.init opts tree

  override __.Graph = tree

  override __.Belief addr =
    bp.Belief addr

  static member init opts mgr idxs =
    BDPSolver (opts, mgr, idxs) :> Solver
