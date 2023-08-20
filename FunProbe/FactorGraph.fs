namespace FunProbe

open B2R2

open System.Collections.Generic

(* id * addr * factor *)
type Node = int * int * Factor

module Node =

  let initVar i a = i, a, Factor.dummy

  let initFactor i a f = i, a, f

  let inline id (i, _, _) = i

  let inline addr (_, a, _) =
    a

  let inline isFactor (_, _, f) = Factor.isDummy f |> not

  let inline factor (_, _, f) =
    if Factor.isDummy f then Utils.impossible () else f

  let dummy = -1, 0UL, Factor.dummy

  let isShallow (_, _, f) =
    Factor.isDummy f |> not && Factor.dim f = 1

  let isDeep (_, _, f) =
    Factor.isDummy f |> not && Factor.dim f = 2

  let isVar node = not <| isFactor node

[<AbstractClass>]
type FactorGraph () =

  abstract FoldPredecessors: Node -> ('a -> Node -> int -> 'a) -> 'a -> 'a

  abstract FoldSuccessors: Node -> ('a -> Node -> int -> 'a) -> 'a -> 'a

  abstract IterPredecessors: Node -> (Node -> int -> unit) -> unit

  abstract IterSuccessors: Node -> (Node -> int -> unit) -> unit

  abstract FindVariableNode: int -> Node

  abstract AddShallowFactor: Factor -> unit

  abstract AddDeepFactor: Factor -> unit

  abstract Nodes: Dictionary<int, Node>

  abstract Preds: Dictionary<int, (int * int) list>

  abstract Succs: Dictionary<int, (int * int) list>

  abstract NumNodes: int

  abstract NumEdges: int

type TreeFactorGraph () =
  inherit FactorGraph ()

  let mutable ncnt = 0
  let mutable ecnt = 0
  let nodes = Dictionary<int, Node> ()
  let preds = Dictionary<int, (int * int) list> ()
  let succs = Dictionary<int, (int * int) list> ()

  let vNodes = Dictionary<int, int> ()

  override __.NumNodes = ncnt

  override __.NumEdges = ecnt

  member private __.AssignNID () =
    let id = ncnt
    ncnt <- ncnt + 1
    id

  member private __.AssignEID () =
    let id = ecnt
    ecnt <- ecnt + 1
    id

  member private __.AddNode (n: Node) =
    let nid = Node.id n
    nodes[nid] <- n
    preds[nid] <- []
    succs[nid] <- []

  member __.AddEdge (src: Node) (dst: Node) =
    let eid = __.AssignEID ()
    let sid = Node.id src
    let did = Node.id dst
    succs[sid] <- (did, eid) :: succs[sid]
    preds[did] <- (sid, eid) :: preds[did]

  member private __.AddVariableNode addr =
    if vNodes.ContainsKey addr then nodes[vNodes[addr]]
    else
      let id = __.AssignNID ()
      let n = Node.initVar id addr
      __.AddNode n
      vNodes[addr] <- id
      n

  member private __.AddFactorNode f =
    let id = __.AssignNID ()
    let n = Node.initFactor id 0 f
    __.AddNode n
    n

  override __.AddShallowFactor (f: Factor) =
    let vAddr = (Factor.vars f)[0]
    let vNode = __.AddVariableNode vAddr
    let fNode = __.AddFactorNode f

    __.AddEdge fNode vNode

  override __.AddDeepFactor (f: Factor) =
    let vars = Factor.vars f
    let srcNode = nodes[vNodes[vars[0]]]
    let dstNode = nodes[vNodes[vars[1]]]
    let fNode = __.AddFactorNode f

    __.AddEdge srcNode fNode
    __.AddEdge fNode dstNode

  override __.FoldPredecessors (v: Node) f acc =
    List.fold (fun acc (id, eid) ->
      let w = nodes[id]
      f acc w eid
      ) acc preds[Node.id v]

  override __.FoldSuccessors (v: Node) f acc =
    List.fold (fun acc (id, eid) ->
      let w = nodes[id]
      f acc w eid
      ) acc succs[Node.id v]

  override __.IterPredecessors _ _ = Utils.impossible ()

  override __.IterSuccessors _ _ = Utils.impossible ()

  override __.FindVariableNode addr =
    nodes[vNodes[addr]]

  override __.Nodes = nodes

  member __.Edges = Utils.impossible ()

  member __.EdgesByNode = Utils.impossible ()

  override __.Preds = Utils.impossible ()

  override __.Succs = succs

  static member init () = TreeFactorGraph () :> FactorGraph

type LoopyFactorGraph () =
  inherit FactorGraph ()

  let mutable ncnt = 0
  let mutable ecnt = 0

  let nodes = Dictionary<int, Node> ()
  let vNodes = Dictionary<int, int> ()
  let preds = Dictionary<int, (int * int) list> ()
  let succs = Dictionary<int, (int * int) list> ()

  override __.NumNodes = ncnt

  override __.NumEdges = ecnt

  member private __.AssignNID () =
    let id = ncnt
    ncnt <- ncnt + 1
    id

  member private __.AssignEID () =
    let id = ecnt
    ecnt <- ecnt + 1
    id

  member private __.AddNode (n: Node) =
    let nid = Node.id n
    nodes[nid] <- n
    preds[nid] <- []
    succs[nid] <- []
    vNodes[Node.addr n] <- nid

  member __.AddEdge (src: Node) (dst: Node) =
    let eid = __.AssignEID ()
    let sid = Node.id src
    let did = Node.id dst
    preds[did] <- (sid, eid) :: preds[did]
    succs[sid] <- (did, eid) :: succs[sid]

  override __.AddShallowFactor (f: Factor) =
    let id = __.AssignNID ()
    let node = Node.initFactor id ((Factor.vars f)[0]) f

    __.AddNode node

  override __.AddDeepFactor (f: Factor) =
    let vars = Factor.vars f
    let srcNode = nodes[vNodes[vars[0]]]
    let dstNode = nodes[vNodes[vars[1]]]

    __.AddEdge srcNode dstNode

  override __.FindVariableNode id =
    nodes[vNodes[id]]

  override __.FoldPredecessors (v: Node) f acc =
    List.fold (fun acc (id, eid) ->
      let w = nodes[id]
      f acc w eid
      ) acc preds[Node.id v]

  override __.IterPredecessors (v: Node) f =
    Utils.impossible ()

  override __.FoldSuccessors (v: Node) f acc =
    List.fold (fun acc (id, eid) ->
      let w = nodes[id]
      f acc w eid
      ) acc succs[Node.id v]

  override __.IterSuccessors (v: Node) f =
    List.iter (fun (id, eid) ->
      let w = nodes[id]
      f w eid) succs[Node.id v]

  override __.Nodes = nodes

  override __.Preds = preds

  override __.Succs = succs

  static member init () = LoopyFactorGraph () :> FactorGraph

type LoopFactorGraph () =
  inherit FactorGraph ()

  let mutable ncnt = 0
  let mutable ecnt = 0
  let nodes = Dictionary<int, Node> ()
  let preds = Dictionary<int, (int * int) list> ()
  let succs = Dictionary<int, (int * int) list> ()

  let vNodes = Dictionary<int, int> ()

  override __.NumNodes = ncnt

  override __.NumEdges = ecnt

  member private __.AssignNID () =
    let id = ncnt
    ncnt <- ncnt + 1
    id

  member private __.AssignEID () =
    let id = ecnt
    ecnt <- ecnt + 1
    id

  member private __.AddNode (n: Node) =
    let nid = Node.id n
    nodes[nid] <- n
    preds[nid] <- []
    succs[nid] <- []

  member __.AddEdge (src: Node) (dst: Node) =
    let eid = __.AssignEID ()
    let sid = Node.id src
    let did = Node.id dst
    preds[did] <- (sid, eid) :: preds[did]
    succs[sid] <- (did, eid) :: succs[sid]

  member private __.AddVariableNode addr =
    if vNodes.ContainsKey addr then nodes[vNodes[addr]]
    else
      let id = __.AssignNID ()
      let n = Node.initVar id addr
      __.AddNode n
      vNodes[addr] <- id
      n

  member private __.AddFactorNode f =
    let id = __.AssignNID ()
    let n = Node.initFactor id 0 f
    __.AddNode n
    n

  override __.AddShallowFactor (f: Factor) =
    let vAddr = (Factor.vars f)[0]
    let vNode = __.AddVariableNode vAddr

    let fNode = __.AddFactorNode f

    __.AddEdge fNode vNode

  override __.AddDeepFactor (f: Factor) =
    let vars = Factor.vars f
    let srcNode = nodes[vNodes[vars[0]]]
    let dstNode = nodes[vNodes[vars[1]]]

    let fNode = __.AddFactorNode f

    __.AddEdge srcNode fNode
    __.AddEdge fNode dstNode

  override __.FoldPredecessors (v: Node) f acc =
    List.fold (fun acc (id, eid) ->
      let w = nodes[id]
      f acc w eid
      ) acc preds[Node.id v]

  override __.FoldSuccessors (v: Node) f acc =
    List.fold (fun acc (id, eid) ->
      let w = nodes[id]
      f acc w eid
      ) acc succs[Node.id v]

  override __.IterPredecessors (v: Node) f =
    List.iter (fun (id, eid) ->
      let w = nodes[id]
      f w eid) preds[Node.id v]

  override __.IterSuccessors (v: Node) f =
    List.iter (fun (id, eid) ->
      let w = nodes[id]
      f w eid) succs[Node.id v]

  override __.FindVariableNode addr =
    nodes[vNodes[addr]]

  override __.Nodes = nodes

  member __.Edges = Utils.impossible ()

  member __.EdgesByNode = Utils.impossible ()

  override __.Preds = Utils.impossible ()

  override __.Succs = succs

  static member init () = LoopFactorGraph () :> FactorGraph
