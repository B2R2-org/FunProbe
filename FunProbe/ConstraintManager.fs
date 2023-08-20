namespace FunProbe

open B2R2
open System.Collections.Generic

type DeepElem =
  | Unknown
  | Range of int * int
  | Targets of HashSet<int>

type ConstraintManager (nBBL: int) =

  let shallowConstraints: bool [] [] = Array.zeroCreate nBBL
  let _ =
    [0 .. nBBL - 1]
    |> List.iter (fun idx -> shallowConstraints[idx] <- Array.create 15 false)
  let deepConstraints: DeepElem [] = Array.create nBBL Unknown
  let deepPositives: HashSet<int> = HashSet ()

  member __.buildFuncCond idx =
    shallowConstraints[idx][0] <- true

  member __.buildBasicCond idx =
    shallowConstraints[idx][1] <- true

  member __.buildNopCond idx =
    shallowConstraints[idx][2] <- true

  member __.buildCallCond idx =
    shallowConstraints[idx][3] <- true

  member __.buildUnreachableCond idx =
    shallowConstraints[idx][4] <- true

  member __.buildTailCallCondRange idx sidx eidx =
    deepConstraints[idx] <- Range (sidx, eidx)

  member __.buildTailCallCondTarget idx targets =
    deepConstraints[idx] <- Targets targets

  member __.buildCondTailCallCond idx =
    shallowConstraints[idx][5] <- true

  member __.buildFakeCallCond idx =
    shallowConstraints[idx][6] <- true

  member __.buildSurroundCond idx =
    shallowConstraints[idx][7] <- true

  member __.buildExceptionCond idx =
    shallowConstraints[idx][8] <- true

  member __.buildRelocCond idx =
    shallowConstraints[idx][9] <- true

  member __.buildColdCond idx =
    shallowConstraints[idx][10] <- true

  member __.buildCallJmpCond idx =
    shallowConstraints[idx][11] <- true

  member __.buildJmpNopCond idx =
    shallowConstraints[idx][12] <- true

  member __.buildConstCond idx =
    shallowConstraints[idx][13] <- true

  member __.buildAddrCond idx =
    shallowConstraints[idx][14] <- true

  member __.ShallowConstraints = shallowConstraints

  member __.DeepConstraints = deepConstraints

  member __.DeepPositives = deepPositives
