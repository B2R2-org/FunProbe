namespace FunProbe

open B2R2
open System.Collections.Generic

module LBPSolverHelper =

  let rec consume addrs idxMap edges num pushed fArr sArr vStack pStack =
    if (pStack: Stack<int>).Count = 0 then num
    else
      let idx = (pStack: Stack<int>).Pop ()
      let isDone =
        (edges: (int * int) list [])[idx]
        |> List.fold (fun b (idx_, _) ->
          let b =
            if (fArr: int [])[idx_] = -1 then false
            else b
          if (pushed: HashSet<int>).Contains idx_ then ()
          else (vStack: Stack<int>).Push idx_
          b) true
      if isDone then
        (sArr: int [])[idx] <- num
        let num = num + 1
        consume addrs idxMap edges num pushed fArr sArr vStack pStack
      else
        pStack.Push idx
        num

  let rec dfs addrs idxMap edges num pushed fArr sArr vStack pStack =
    if (vStack: Stack<int>).Count = 0 then num
    else
      let idx = vStack.Pop ()
      if (fArr: int [])[idx] <> -1 then
        dfs addrs idxMap edges num pushed fArr sArr vStack pStack
      else
        fArr[idx] <- num
        let num = num + 1
        (pStack: Stack<int>).Push idx
        let num = consume addrs idxMap edges num pushed fArr sArr vStack pStack
        dfs addrs idxMap edges num pushed fArr sArr vStack pStack

  let topologicalSort addrs idxMap edges =
    let numNodes = Array.length addrs
    let pushed = HashSet ()
    let fArr = Array.create numNodes -1
    let sArr = Array.create numNodes -1
    let vStack = Stack ()
    let pStack = Stack ()
    let lnum =
      addrs
      |> Array.fold (fun (num, idx) _ ->
        assert (vStack.Count = 0 && pStack.Count = 0)
        if sArr[idx] = -1 then
          vStack.Push idx
          dfs addrs idxMap edges num pushed fArr sArr vStack pStack, idx + 1
        else num, idx + 1
        ) (0, 0)
      |> snd
    assert (lnum = 2 * numNodes - 1)
    sArr

  let getEdges addrs (edges: (int * int) list []) (numMap: int []) =
    let newEdges = HashSet<int * int> ()
    addrs
    |> Array.iteri (fun idx _ ->
      edges[idx]
      |> List.iter (fun (idx_, _) ->
        if numMap[idx] > numMap[idx_] then newEdges.Add (idx, idx_) |> ignore
        elif numMap[idx] < numMap[idx_] then newEdges.Add (idx_, idx) |> ignore
        else ()
        )
      )
    newEdges

  let addShallowConstraint opts (mgr: ConstraintManager) (tree: FactorGraph) idx =
    let consts = mgr.ShallowConstraints[idx]
    if Array.forall not consts then
      let f = SolverUtils.initBasic opts idx [] |> List.head
      tree.AddShallowFactor f
    else
      if consts[0] then
        let f = SolverUtils.initFunc opts idx
        tree.AddShallowFactor f
      else
        let _, factors =
          consts[1..]
          |> Array.fold (SolverUtils.combineShallows opts idx) (1, [])
        List.iter (fun f -> tree.AddShallowFactor f) factors

  let addDeepConstraint opts (loop: FactorGraph) (addrs: int []) (sidx, didx) =
    let addr = addrs[sidx]
    let addr_ = addrs[didx]
    let f = SolverUtils.initTailCall opts addr addr_
    loop.AddDeepFactor f

  let buildEdges (mgr: ConstraintManager) addrs idxMap (edges: (int * int) list []) =
    addrs
    |> Array.foldi (fun eid idx addr ->
      match mgr.DeepConstraints[addr] with
      | Range (sAddr, eAddr) ->
        let mutable cnt = eid
        for addr_ in sAddr .. eAddr do
          let idx_ = (idxMap: Dictionary<int, int>)[addr_]
          edges[idx_] <- (idx, cnt) :: edges[idx_]
          cnt <- cnt + 1
        done
        cnt
      | Targets targets ->
        let mutable cnt = eid
        for addr_ in targets do
          let idx_ = (idxMap: Dictionary<int, int>)[addr_]
          edges[idx_] <- (idx, cnt) :: edges[idx_]
          cnt <- cnt + 1
        done
        cnt
      | _ -> eid
      ) 0
    |> ignore

  let buildGraph opts mgr idxs =
    let addrs = List.toArray idxs
    let idxMap = Dictionary ()
    Array.iteri (fun i idx -> idxMap[idx] <- i) addrs
    let edges = Array.create (Array.length addrs) []
    buildEdges mgr addrs idxMap edges
    let numMap = topologicalSort addrs idxMap edges
    let edges = getEdges addrs edges numMap
    let loop = LoopFactorGraph.init ()
    List.iter (addShallowConstraint opts mgr loop) idxs
    Seq.iter (addDeepConstraint opts loop addrs) edges
    loop

type LBPSolver (opts, mgr, idxs) =
  inherit Solver ()

  let loop = LBPSolverHelper.buildGraph opts mgr idxs
  let bp = LoopyBeliefPropagation.init opts loop 10

  override __.Graph = loop

  override __.Belief addr =
    bp.Belief addr

  static member init opts mgr idxs =
    LBPSolver (opts, mgr, idxs) :> Solver
