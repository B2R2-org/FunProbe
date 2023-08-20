namespace FunProbe

open B2R2

type FactorKind =
  | OneFactor
  | FuncCond
  | BasicCond
  | NopCond
  | CallCond
  | UnreachableCond
  | TailCallCond
  | CondTailCallCond
  | FakeCallCond
  | SurroundCond
  | ExceptionCond
  | RelocCond
  | ColdCond
  | CallJmpCond
  | JmpNopCond
  | ConstCond
  | AddrCond
  | Custom1D of float

type Tensor = (struct (float * float * float * float))

[<AutoOpen>]
module FactorHelper =

  let inline init1DTensor p =
    struct (p, 1.0 - p, 0.0, 0.0), 1

  let inline initAlmostSure1D () =
    init1DTensor 0.999

  let inline initSymmetric2D p =
    struct (p, 1.0 - p, 1.0 - p, p), 2

  let initNeutral1D () =
    init1DTensor 0.5

  let initPositive1D (opts: CmdOpts) =
    init1DTensor opts.PositiveProb

  let initNegative1D (opts: CmdOpts) =
    init1DTensor opts.NegativeProb

  let initPositive2D (opts: CmdOpts) =
    initSymmetric2D opts.PositiveProb

  let initTensor opts = function
    | OneFactor -> initNeutral1D ()
    | FuncCond -> initAlmostSure1D ()
    | BasicCond -> initNegative1D opts
    | NopCond -> initNegative1D opts
    | CallCond -> initPositive1D opts
    | UnreachableCond -> initPositive1D opts
    | TailCallCond -> initPositive2D opts
    | CondTailCallCond -> initNegative1D opts
    | FakeCallCond -> initNegative1D opts
    | SurroundCond -> initNegative1D opts
    | ExceptionCond -> initPositive1D opts
    | RelocCond -> initPositive1D opts
    | ColdCond -> initNegative1D opts
    | CallJmpCond -> initPositive1D opts
    | JmpNopCond -> initNegative1D opts
    | ConstCond -> initPositive1D opts
    | AddrCond -> initPositive1D opts
    | Custom1D p -> init1DTensor p

  let assignNewIndType1 vars shared =
    let inds = Array.create (Array.length vars) 0
    Array.fold (fun (idx1, idx2, cur) addr ->
      if Set.contains addr shared then
        inds[idx2] <- cur
        idx1, idx2 + 1, cur + 1
      else
        inds[idx1] <- cur
        idx1 + 1, idx2, cur + 1
      ) (0, Array.length vars - Set.count shared, 0) vars
    |> ignore
    Array.toList inds

  let assignNewIndType2 vars shared =
    let inds = Array.create (Array.length vars) 0
    Array.fold (fun (idx1, idx2, cur) addr ->
      if Set.contains addr shared then
        inds[idx1] <- cur
        idx1 + 1, idx2, cur + 1
      else
        inds[idx2] <- cur
        idx1, idx2 + 1, cur + 1
      ) (0, Array.length vars - Set.count shared, 0) vars
    |> ignore
    Array.toList inds

  let permuteAxis (inds: int list) dist =
    let struct (xx, xy, yx, yy) = dist
    if inds[0] = 0 then dist
    else struct (xx, yx, xy, yy)

  let sumFactor (struct (xx, xy, yx, yy)) =
    xx + xy + yx + yy

  let diffFactors (dist1: Tensor) (dist2: Tensor) dim1 dim2 =
    let struct (xx1, xy1, yx1, yy1) = dist1
    let struct (xx2, xy2, yx2, yy2) = dist2
    assert (dim1 = dim2)
    if dim1 = 1 then
      List.max [ abs (xx1 - xx2); abs (xy1 - xy2) ]
    else
      List.max [ abs (xx1 - xx2); abs (xy1 - xy2); abs (yx1 - yx2); abs (yy1 - yy2) ]

  let multFactors dist1 dist2 f1Dim f2Dim =
    let struct (xx1, xy1, yx1, yy1) = dist1
    let struct (xx2, xy2, yx2, yy2) = dist2
    if f1Dim = 1 && f2Dim = 1 then
      struct (xx1 * xx2, xy1 * xy2, 0.0, 0.0), 1
    elif f1Dim = 2 && f2Dim = 1 then
      struct (xx1 * xx2, xy1 * xy2, yx1 * xx2, yy1 * xy2), 2
    elif f1Dim = 1 && f2Dim = 2 then
      struct (xx1 * xx2, xy1 * xy2, xx1 * yx2, xy1 * yy2), 2
    elif f1Dim = 2 && f2Dim = 2 then
      struct (xx1 * xx2, xy1 * xy2, yx1 * yx2, yy1 * yy2), 2
    else
      Utils.impossible ()

  let divConstFactor (struct (xx, xy, yx, yy)) div =
    struct (xx / div, xy / div, yx / div, yy / div)

  let sumAxis ax (struct (xx, xy, yx, yy)) =
    if ax = 0 then
      struct (xx + yx, xy + yy, 0.0, 0.0)
    else
      struct (xx + xy, yx + yy, 0.0, 0.0)

type Factor = int [] * Tensor * int

module Factor =

  let inline vars ((vs, _, _): Factor) = vs

  let inline indexOf addr ((vs, _, _): Factor) =
    if vs[0] = addr then 0 else 1

  let inline dist ((_, dist, _): Factor) = dist

  let inline dim ((_, _, dim): Factor) = dim

  let init opts (vs, kind: FactorKind) =
    let dist, dim = initTensor opts kind
    (vs, dist, dim)

  let productFast (f1: Factor) (f2: Factor) =
    let newDist, newDim = multFactors (dist f1) (dist f2) (dim f1) (dim f2)
    let newVars = vars f1

    newVars, newDist, newDim

  let normalize (f: Factor) =
    let d = dist f
    let tensor = divConstFactor d (sumFactor d)
    vars f, tensor, dim f

  let product (f1: Factor) (f2: Factor) =
    let f1VSet = Set.ofArray <| vars f1
    let f2VSet = Set.ofArray <| vars f2
    let shared = Set.intersect f1VSet f2VSet

    let newF1Ind = assignNewIndType1 (vars f1) shared
    let newF2Ind = assignNewIndType2 (vars f2) shared

    let newF1Dist = permuteAxis newF1Ind (dist f1)
    let newF2Dist = permuteAxis newF2Ind (dist f2)
    let newDist, newDim = multFactors newF1Dist newF2Dist (dim f1) (dim f2)

    let newF1Vars = Array.permute (fun idx -> newF1Ind[idx]) <| vars f1
    let newF2Vars = Array.permute (fun idx -> newF2Ind[idx]) <| vars f2
    let newVars = Array.concat [newF1Vars; newF2Vars[shared.Count ..]]

    (newVars, newDist, newDim) |> normalize

  let marginalizeWith vs (f: Factor) =
    let newDist =
      List.fold (fun dist v ->
        let idx = indexOf v f
        sumAxis idx dist
        ) (dist f) vs
    let newVars = Array.filter (fun v -> not <| List.contains v vs) <| vars f
    newVars, newDist, 1

  let jointDistribution fs =
    List.reduce product fs

  let jointDistributionFast fs =
    List.reduce productFast fs

  let dummy = [||], struct (0.0, 0.0, 0.0, 0.0), 0

  let inline isDummy (_, _, dim) = dim = 0
