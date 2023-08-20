module FunProbe.ConstraintSolver

open B2R2
open System.Collections.Generic

let rec dfs edges (comps: int []) cid (pushed: HashSet<int>) (stack: Stack<int>) =
  while stack.Count <> 0 do
    let idx = stack.Pop ()
    if comps[idx] <> -1 then ()
    else
      comps[idx] <- cid
      for idx_ in (edges: List<int> [])[idx] do
        if comps[idx_] = -1 && not <| pushed.Contains idx_ then
          stack.Push idx_
          pushed.Add idx_ |> ignore
        else ()
      done

let computeConnectedComponents (edges: List<int> []) =
  let comps = Array.create edges.Length -1
  let nComps =
    edges
    |> Array.fold (fun (idx, cid) _ ->
      if comps[idx] = -1 then
        let stack = Stack ()
        let pushed = HashSet<int> ()
        stack.Push idx
        pushed.Add idx |> ignore
        dfs edges comps cid pushed stack
        idx + 1, cid + 1
      else
        idx + 1, cid) (0, 0)
    |> snd
  comps, nComps

let buildEdges (mgr: ConstraintManager) =
  let edges = Array.zeroCreate (Array.length mgr.ShallowConstraints)
  for i in 0 .. Array.length mgr.ShallowConstraints - 1 do
    edges[i] <- List ()
  done
  (mgr: ConstraintManager).DeepConstraints
  |> Array.iteri (fun idx1 elem ->
    match elem with
    | Range (sidx, eidx) ->
      for idx2 in sidx .. eidx do
        edges[idx1].Add idx2
        edges[idx2].Add idx1
      done
    | Targets targets ->
      for idx2 in targets do
        edges[idx1].Add idx2
        edges[idx2].Add idx1
      done
    | _ -> ()
    )
  edges

let computeComponents mgr =
  buildEdges mgr
  |> computeConnectedComponents

let solveOneByOne (opts: CmdOpts) core mgr solutions =
  let comps, nComps = computeComponents mgr
  let compsArr = Array.create nComps []
  let probMap = Dictionary<Addr, bool [] * float> ()
  Array.iteri (fun idx cid ->
    compsArr[cid] <- idx :: compsArr[cid]) comps
  [ 0 .. nComps - 1 ]
  |> List.iter (fun cid ->
    let idxs = compsArr[cid]
    let nV = List.length idxs
    let nE =
      List.fold (fun acc idx ->
        match mgr.DeepConstraints[idx] with
        | Range (sidx, eidx) ->
          acc + eidx - sidx + 1
        | Targets targets ->
          acc + targets.Count
        | _ ->
          acc) 0 idxs
    let hasLoop = nV <> nE + 1
    let solver =
      if hasLoop then
        if opts.Solver = "lbp" then LBPSolver.init opts mgr idxs
        elif opts.Solver = "bdp" then BDPSolver.init opts mgr idxs
        else Utils.futureFeature ()
      else TreeSolver.init opts mgr idxs
    idxs
    |> List.iter (fun idx ->
      let res = solver.Belief idx
      let struct (p, _, _, _) = Factor.dist res
      let addr = core.BasicBlocks[idx].BBLAddr
      let shallows = mgr.ShallowConstraints[idx]
      probMap[addr] <- (shallows, p)
      if p > 0.5 then
        let addr = core.BasicBlocks[idx].BBLAddr
        (solutions: HashSet<Addr>).Add addr |> ignore
      )
    )
  probMap

let solve opts core mgr =
  let solutions = HashSet<Addr> ()
  let probMap = solveOneByOne opts core mgr solutions
  solutions, probMap
