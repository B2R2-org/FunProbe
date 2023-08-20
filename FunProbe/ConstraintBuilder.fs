module FunProbe.ConstraintBuilder

open B2R2

open System.Collections.Generic

let rec dfsConnectedComponents core id (compMap: int []) = function
  | [] -> ()
  | idx :: idxs ->
    if compMap[idx] <> -1 then
      dfsConnectedComponents core id compMap idxs
    else
      compMap[idx] <- id
      let idxs =
        core.Preds[idx]
        |> List.fold (fun idxs p ->
          if compMap[p] <> -1 then idxs
          else p :: idxs) idxs
      let idxs =
        core.Succs[idx]
        |> List.fold (fun idxs s ->
          if compMap[s] <> -1 then idxs
          else s :: idxs) idxs
      dfsConnectedComponents core id compMap idxs

let rec computeConnectedComponents core (compMap: int []) =
  Array.fold (fun (idx, id) _ ->
    if compMap[idx] = -1 then
      dfsConnectedComponents core id compMap [idx]
      idx + 1, id + 1
    else idx + 1, id
    ) (0, 0) core.BasicBlocks
  |> ignore

let resolveTarget core idx target =
  match core.AddrMap.TryGetValue target with
  | true, targetIdx ->
    if List.isEmpty core.Succs[idx] then None
    elif List.contains targetIdx core.Succs[idx] then Some targetIdx
    else
      let succs = core.Succs[idx] |> Set.ofList
      let succs = succs |> Set.filter (fun a -> core.BasicBlocks[a].BBLAddr > target)
      Set.minElement succs |> Some
  | _ -> None

let buildNopCond (mgr: ConstraintManager) bbl =
  if bbl.BBLIsNop || bbl.BBLIsPadding then mgr.buildNopCond bbl.BBLIdx

let buildCallCond core (mgr: ConstraintManager) bbl =
  let addr = bbl.BBLAddr
  let idx = bbl.BBLIdx
  if core.CallTargets.Contains addr then mgr.buildCallCond idx

let buildFakeCallCond core (mgr: ConstraintManager) bbl =
  if not bbl.BBLIsNop && not bbl.BBLIsPadding then
    let addr = bbl.BBLAddr
    let idx = bbl.BBLIdx
    if core.CallTargets.Contains addr then
      let prevAddr =
        match core.Handle.ISA.Arch with
        | Arch.MIPS32 | Arch.MIPS64 ->
          let pAddr = core.PrevInsAddrs[addr]
          if pAddr = 0UL then 0UL
          else core.PrevInsAddrs[pAddr]
        | _ -> core.PrevInsAddrs[addr]
      if prevAddr <> 0UL then
        let prevIns = core.Instrs[prevAddr]
        if prevIns.IsCall () then
          match prevIns.DirectBranchTarget () with
          | true, target ->
            if target = addr then mgr.buildFakeCallCond idx
          | _ -> ()
        else ()

let buildBasicCond (mgr: ConstraintManager) bbl =
  mgr.buildBasicCond bbl.BBLIdx

let buildTailCallCond core (mgr: ConstraintManager) info tree (bbl: BasicBlock) =
  if not bbl.BBLIsNop && not bbl.BBLIsPadding then
    let last = core.Instrs[bbl.BBLLastAddr]
    if last.IsCall () then ()
    elif last.IsCondBranch () then ()
    else
      match last.DirectBranchTarget () with
      | true, target ->
        if bbl.BBLAddr > core.EntryPoint && core.EntryPoint > target then ()
        else
          let idx = bbl.BBLIdx
          match resolveTarget core idx target with
          | Some target ->
            if abs (idx - target) < 2 then ()
            else
              let smaller, larger =
                if idx < target then idx, target else target, idx
              let hasCallTarget =
                [ smaller + 1 .. larger - 1 ]
                |> List.fold (fun acc idx ->
                  let bAddr = core.BasicBlocks[idx].BBLAddr
                  acc || core.CallTargets.Contains bAddr) false
              if hasCallTarget then
                let targets = HashSet ()
                for idx in smaller + 1 .. larger - 1 do
                  let block = core.BasicBlocks[idx]
                  let bAddr = block.BBLAddr
                  let bIdx = block.BBLIdx
                  if core.CallTargets.Contains bAddr then targets.Add bIdx |> ignore
                done
                mgr.buildTailCallCondTarget target targets
              else
                let children = Map.find idx tree
                if List.contains target children then ()
                else
                  mgr.buildTailCallCondRange target (smaller + 1) (larger - 1)
          | _ -> ()
      | _ -> ()

let buildCondTailCallCond core (mgr: ConstraintManager) (bbl: BasicBlock) =
  if not bbl.BBLIsNop && not bbl.BBLIsPadding then
    let last = core.Instrs[bbl.BBLLastAddr]
    if last.IsCondBranch () then
      match last.DirectBranchTarget () with
      | true, target ->
        let idx = bbl.BBLIdx
        match resolveTarget core idx target with
        | Some target ->
          mgr.buildCondTailCallCond target
        | _ -> ()
      | _ -> ()
    else ()

let buildUnreachableCond core (mgr: ConstraintManager) bbl =
  let idx = bbl.BBLIdx
  if List.isEmpty core.Preds[idx] then
    if core.JumpTargets.Contains bbl.BBLAddr |> not then
      mgr.buildUnreachableCond idx

let buildSurroundCond core (mgr: ConstraintManager) (comps: int []) bbl =
  let idx = bbl.BBLIdx
  if List.isEmpty core.Preds[idx] then
    let before = if idx = 0 then None else Some (idx - 1)
    let after = if idx = Array.length core.BasicBlocks - 1 then None else Some (idx + 1)
    match before, after with
    | Some before, Some after ->
      if comps[before] = comps[after] then mgr.buildSurroundCond idx
    | _ -> ()

let buildColdCond core (mgr: ConstraintManager) bbl =
  if not bbl.BBLIsNop && not bbl.BBLIsPadding then
    let last = core.Instrs[bbl.BBLLastAddr]
    if last.IsBranch () && not <| last.IsCall () && not <| last.IsRET () then
      match last.DirectBranchTarget () with
      | true, target ->
        let idx = bbl.BBLIdx
        if bbl.BBLAddr > core.EntryPoint && core.EntryPoint > target then
          match resolveTarget core idx target with
          | Some target ->
            mgr.buildColdCond target
          | _ -> ()
        else ()
      | _ -> ()
    else ()

let buildCallJmpCond core (mgr: ConstraintManager) bbl =
  if not bbl.BBLIsNop && not bbl.BBLIsPadding then
    let bblAddr = bbl.BBLAddr
    if core.CallTargets.Contains bblAddr then
      if bbl.BBLLastAddr = bblAddr then
        let last = core.Instrs[bbl.BBLLastAddr]
        if last.IsBranch () && not <| last.IsCall () && not <| last.IsRET () then
          match last.DirectBranchTarget () with
          | true, target ->
            let idx = bbl.BBLIdx
            match resolveTarget core idx target with
            | Some target ->
              mgr.buildCallJmpCond target
            | None -> ()
          | _ -> ()
        else ()
      else ()
    else ()
  else ()

let buildJmpNopCond core (mgr: ConstraintManager) bbl =
  if not bbl.BBLIsNop && not bbl.BBLIsPadding then
    let bblAddr = bbl.BBLAddr
    if not <| core.CallTargets.Contains bblAddr then
      if bbl.BBLLastAddr = bblAddr then
        let last = core.Instrs[bbl.BBLLastAddr]
        if last.IsBranch () && not <| last.IsCall () && not <| last.IsRET () then
          if bbl.BBLIdx < Array.length core.BasicBlocks - 2 then
            let nextbbl = core.BasicBlocks[bbl.BBLIdx + 1]
            let nnextbbl = core.BasicBlocks[bbl.BBLIdx + 2]
            if nextbbl.BBLIsNop || nextbbl.BBLIsPadding then
              match last.DirectBranchTarget () with
              | true, target ->
                let idx = bbl.BBLIdx
                match resolveTarget core idx target with
                | Some target ->
                  if nnextbbl.BBLIdx = target then
                    mgr.buildJmpNopCond bbl.BBLIdx
                | None -> ()
              | _ -> ()
            else
              match last.DirectBranchTarget () with
              | true, target ->
                let idx = bbl.BBLIdx
                match resolveTarget core idx target with
                | Some target ->
                  if nextbbl.BBLIdx = target then
                    mgr.buildJmpNopCond bbl.BBLIdx
                | None -> ()
              | _ -> ()
          else ()
        else ()
      else ()
    else ()
  else ()

let build core =
  let mgr = ConstraintManager (Array.length core.BasicBlocks)

  (* Function Constraints *)
  core.InitArray
  |> Seq.iter (fun addr ->
    let idx = core.AddrMap[addr]
    mgr.buildFuncCond idx)
  core.FiniArray
  |> Seq.iter (fun addr ->
    let idx = core.AddrMap[addr]
    mgr.buildFuncCond idx)
  core.DynamicSymbols
  |> Seq.iter (fun addr ->
    let idx = core.AddrMap[addr]
    mgr.buildFuncCond idx)
  (* Exception Constraints *)
  core.ExceptionInfo
  |> Seq.iter (fun addr ->
    let idx = core.AddrMap[addr]
    mgr.buildExceptionCond idx
    )
  (* Relocation Constraints *)
  core.RelocationSymbols
  |> Seq.iter (fun addr ->
    let idx = core.AddrMap[addr]
    mgr.buildRelocCond idx
    )
  (* Constant Constraints *)
  core.Constants
  |> Seq.iter (fun addr ->
    let idx = core.AddrMap[addr]
    mgr.buildConstCond idx
    )
  core.Addrs
  |> Seq.iter (fun addr ->
    let idx = core.AddrMap[addr]
    mgr.buildAddrCond idx
    )
  let comps = Array.create (Array.length core.BasicBlocks) -1
  computeConnectedComponents core comps
  (* Others *)
  core.BasicBlocks
  |> Array.iter (fun bbl ->
    buildNopCond mgr bbl
    buildCallCond core mgr bbl
    buildFakeCallCond core mgr bbl
    buildBasicCond mgr bbl
    buildCondTailCallCond core mgr bbl
    buildUnreachableCond core mgr bbl
    buildSurroundCond core mgr comps bbl
    buildColdCond core mgr bbl
    buildCallJmpCond core mgr bbl
    buildJmpNopCond core mgr bbl
    )
  let info, tree = Dominator.dominatorTree core
  core.BasicBlocks
  |> Array.iter (fun bbl ->
    buildTailCallCond core mgr info tree bbl
    )
  mgr
