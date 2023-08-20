namespace FunProbe

type LoopyBeliefPropagation (opts, fg: FactorGraph, tlimit: int) as __ =
  inherit Solver ()

  let mutable cnt = 0
  let nEdges =
    fg.Nodes
    |> Seq.fold (fun cnt (KeyValue (idx, _)) ->
      cnt + List.length fg.Succs[idx]) 0
  let mutable msg: Factor [] = Array.create nEdges Factor.dummy
  let mutable edgeMap = Array.zeroCreate nEdges
  let mutable converged = false
  do
    __.Initialize ()

  override __.Graph = fg

  member private __.Initialize () =
    fg.Nodes
    |> Seq.iter (fun (KeyValue (vid, node)) ->
      fg.IterSuccessors node (fun succ eid ->
        let f =
          if Node.isFactor node then Factor.init opts ([| Node.addr succ |], OneFactor)
          else Factor.init opts ([| Node.addr node |], OneFactor)
        msg[eid] <- f
        edgeMap[eid] <- vid, Node.id succ
        )
      )

  member __.ComputeF2V (fNode: Node) (vNode: Node) =
    let factor, marginalVars =
      fg.FoldPredecessors fNode (fun (factor, marginalVars) v eid ->
        if Node.addr v <> Node.addr vNode then
          let factor = Factor.product factor msg[eid]
          let marginalVars = (Node.addr v) :: marginalVars
          factor, marginalVars
        else factor, marginalVars
        ) (Node.factor fNode, [])
      |> fg.FoldSuccessors fNode (fun (factor, marginalVars) v eid ->
        if Node.addr v <> Node.addr vNode then
          let factor = Factor.product factor msg[eid]
          let marginalVars = (Node.addr v) :: marginalVars
          factor, marginalVars
        else factor, marginalVars
        )

    factor
    |> Factor.marginalizeWith marginalVars
    |> Factor.normalize

  member __.ComputeV2F (vNode: Node) (fNode: Node) =
    fg.FoldPredecessors vNode (fun factor v eid ->
      if Node.factor v <> Node.factor fNode then
        Factor.product factor msg[eid]
      else factor) (Factor.init opts ([|Node.addr vNode|], OneFactor))
    |> fg.FoldSuccessors vNode (fun factor v eid ->
      if Node.factor v <> Node.factor fNode then
        Factor.product factor msg[eid]
      else factor)

  member __.IsConverged (oldMsg: Factor []) (newMsg: Factor []) =
    let _, res, maxDiff =
      oldMsg
      |> Array.fold (fun (eid, acc, maxDiff) oldFactor ->
        if not acc then eid + 1, acc, maxDiff
        else
          let newFactor = newMsg[eid]
          let oldDist = Factor.dist oldFactor
          let newDist = Factor.dist newFactor
          let oldDim = Factor.dim oldFactor
          let newDim = Factor.dim newFactor
          let diff = diffFactors oldDist newDist oldDim newDim
          eid + 1, diff < 0.0001, max maxDiff diff
        ) (0, true, 0.0)
    res

  member __.Loop () =
    let newMsg: Factor [] = Array.copy msg
    let isConverged =
      [0 .. tlimit - 1]
      |> List.fold (fun acc i ->
        if cnt <= i && not acc then
          edgeMap
          |> Array.iteri (fun eid (sid, did) ->
            let src = fg.Nodes[sid]
            let dst = fg.Nodes[did]
            if Node.isFactor src then
              newMsg[eid] <- __.ComputeF2V src dst
            else
              newMsg[eid] <- __.ComputeV2F src dst
            )
          let acc = __.IsConverged msg newMsg
          msg <- newMsg
          cnt <- cnt + 1
          acc
        else acc) converged
    converged <- isConverged

  override __.Belief name =
    __.Loop ()
    let vNode = fg.FindVariableNode name

    fg.FoldPredecessors vNode (fun factor v eid ->
      Factor.product factor msg[eid]
      ) (Factor.init opts ([|Node.addr vNode|], OneFactor))
    |> fg.FoldSuccessors vNode (fun factor v eid ->
      Factor.product factor msg[eid]
      )

  static member init opts fg tlimit =
    LoopyBeliefPropagation (opts, fg, tlimit) :> Solver
