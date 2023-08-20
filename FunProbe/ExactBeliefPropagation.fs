namespace FunProbe

type ExactBeliefPropagation (opts, fg: FactorGraph) as __ =
  inherit Solver ()

  let nEdges =
    fg.Nodes
    |> Seq.fold (fun cnt (KeyValue (idx, _)) ->
      cnt + List.length fg.Succs[idx]) 0
  let msg = Array.create nEdges Factor.dummy

  override __.Graph = fg

  member __.GetF2V (fNode: Node) (vNode: Node) eid =
    let f = msg[eid]
    if Factor.isDummy f then
      let f = __.ComputeF2V fNode vNode
      msg[eid] <- f
      f
    else f

  member __.GetV2F (vNode: Node) (fNode: Node) eid =
    let f = msg[eid]
    if Factor.isDummy f then
      let f = __.ComputeV2F vNode fNode
      msg[eid] <- f
      f
    else f

  member __.ComputeF2V (fNode: Node) (vNode: Node) =
    let factor, marginalVars =
      fg.FoldPredecessors fNode (fun (factor, marginalVars) v eid ->
        if Node.addr v <> Node.addr vNode then
          let f = __.GetV2F v fNode eid
          let factor = Factor.product factor f
          let marginalVars = (Node.addr v) :: marginalVars
          factor, marginalVars
        else factor, marginalVars
        ) (Node.factor fNode, [])
      |> fg.FoldSuccessors fNode (fun (factor, marginalVars) v eid ->
        if Node.addr v <> Node.addr vNode then
          let f = __.GetV2F v fNode eid
          let factor = Factor.product factor f
          let marginalVars = (Node.addr v) :: marginalVars
          factor, marginalVars
        else factor, marginalVars
        )

    factor
    |> Factor.marginalizeWith marginalVars
    |> Factor.normalize

  member __.ComputeV2F (vNode: Node) (fNode: Node) =
    let incomings =
      fg.FoldPredecessors vNode (fun incomings v eid ->
        if Node.factor v <> Node.factor fNode then
          let f = __.GetF2V v vNode eid
          f :: incomings
        else incomings) []
      |> fg.FoldSuccessors vNode (fun incomings v eid ->
        if Node.factor v <> Node.factor fNode then
          let f = __.GetF2V v vNode eid
          f :: incomings
        else incomings)
    let incomings = List.rev incomings

    if List.isEmpty incomings then
      Factor.init opts ([|Node.addr vNode|], OneFactor)
    else
      Factor.jointDistribution incomings
      |> Factor.normalize

  override __.Belief addr =
    let vNode = fg.FindVariableNode addr

    let incomings =
      fg.FoldPredecessors vNode (fun incomings v eid ->
        let f = __.GetF2V v vNode eid
        f :: incomings) []
      |> fg.FoldSuccessors vNode (fun incomings v eid ->
        let f = __.GetF2V v vNode eid
        f :: incomings)

    Factor.jointDistribution incomings
    |> Factor.normalize

  static member init opts fg =
    ExactBeliefPropagation (opts, fg) :> Solver
