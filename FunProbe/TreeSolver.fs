namespace FunProbe

open B2R2

module TreeSolverHelper =

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

  let addDeepConstraint opts (mgr: ConstraintManager) (tree: FactorGraph) idx =
    match mgr.DeepConstraints[idx] with
    | Range (sidx, eidx) ->
      for idx_ in sidx .. eidx do
        let f = SolverUtils.initTailCall opts idx_ idx
        tree.AddDeepFactor f
      done
    | Targets targets ->
      for idx_ in targets do
        let f = SolverUtils.initTailCall opts idx_ idx
        tree.AddDeepFactor f
      done
    | _ -> ()

  let buildGraph opts (mgr: ConstraintManager) idxs =
    let tree = TreeFactorGraph.init ()
    List.iter (addShallowConstraint opts mgr tree) idxs
    List.iter (addDeepConstraint opts mgr tree) idxs
    tree

type TreeSolver (opts, mgr, idxs) =
  inherit Solver ()

  let tree = TreeSolverHelper.buildGraph opts mgr idxs
  let bp = ExactBeliefPropagation.init opts tree

  override __.Graph = tree

  override __.Belief addr =
    bp.Belief addr

  static member init opts mgr idxs =
    TreeSolver (opts, mgr, idxs) :> Solver
