module FunProbe.BasicBlockBuilder

open B2R2
open B2R2.FrontEnd.BinLifter
open B2R2.FrontEnd.BinInterface

open System.Collections.Generic

let completeBlock core num acc addr size lastAddr isNopBlk =
  core.AddrMap[addr] <- num
  (acc: Dictionary<Addr, BasicBlock>)[addr] <- BasicBlock.init num addr size lastAddr isNopBlk false
  num + 1

let addPaddingBlock core num acc addr size =
  core.AddrMap[addr] <- num
  (acc: Dictionary<Addr, BasicBlock>)[addr] <- BasicBlock.init num addr size 0UL false true
  num + 1

let rec buildBasicBlockLoop core num acc curBlk size prevAddr isNopBlk = function
  | [] -> completeBlock core num acc curBlk size prevAddr isNopBlk
  | addr :: addrs ->
    let ins = core.Instrs[addr]
    let prevIns = core.Instrs[prevAddr]
    if prevAddr + uint64 prevIns.Length <> addr then
      handlePaddingBlock core num acc curBlk size isNopBlk addr ins prevAddr prevIns addrs
    elif core.References.ContainsKey addr then
      handleReferenced core num acc curBlk size prevAddr isNopBlk addr ins addrs
    elif prevIns.IsBBLEnd () then
      handleBBLEnd core num acc curBlk size prevAddr isNopBlk addr ins addrs
    elif isNopBlk then
      handleNopBlk core num acc curBlk size prevAddr addr ins addrs
    else
      handleFallThrough core num acc curBlk size isNopBlk addr ins addrs

and initBlockAndLoop core num acc addr ins addrs =
  let curBlk, size, isNopBlk =
    if NopOracle.isNop core.Handle ins then addr, uint64 ins.Length, true
    else addr, uint64 ins.Length, false
  buildBasicBlockLoop core num acc curBlk size addr isNopBlk addrs

and increaseSizeAndLoop core num acc curBlk size prevAddr isNopBlk (ins: Instruction) addrs =
  buildBasicBlockLoop core num acc curBlk (size + uint64 ins.Length) prevAddr isNopBlk addrs

and handlePaddingBlock core num acc curBlk size isNopBlk addr ins prevAddr prevIns addrs =
  let num = completeBlock core num acc curBlk size prevAddr isNopBlk
  let nextAddr = prevAddr + uint64 (prevIns: Instruction).Length
  let num = addPaddingBlock core num acc nextAddr (addr - nextAddr)
  initBlockAndLoop core num acc addr ins addrs

and handleReferenced core num acc curBlk size prevAddr isNopBlk addr ins addrs =
  let num = completeBlock core num acc curBlk size prevAddr isNopBlk
  initBlockAndLoop core num acc addr ins addrs

and handleBBLEnd core num acc curBlk size prevAddr isNopBlk addr (ins: Instruction) addrs =
  match core.Handle.ISA.Arch with
  | Arch.MIPS32 | Arch.MIPS64 ->
    (* MIPS specific *)
    match addrs with
    | addr :: addrs ->
      let num = completeBlock core num acc curBlk (size + uint64 ins.Length) prevAddr isNopBlk
      let ins = core.Instrs[addr]
      initBlockAndLoop core num acc addr ins addrs
    | [] ->
      completeBlock core num acc curBlk (size + uint64 ins.Length) prevAddr isNopBlk
  | _ ->
    let num = completeBlock core num acc curBlk size prevAddr isNopBlk
    initBlockAndLoop core num acc addr ins addrs

and handleNopBlk core num acc curBlk size prevAddr addr ins addrs =
  if NopOracle.isNop core.Handle ins then
    increaseSizeAndLoop core num acc curBlk size addr true ins addrs
  else
    let num = completeBlock core num acc curBlk size prevAddr true
    initBlockAndLoop core num acc addr ins addrs

and handleFallThrough core num acc curBlk size isNopBlk addr ins addrs =
  increaseSizeAndLoop core num acc curBlk size addr isNopBlk ins addrs

let buildBasicBlocks core =
  let acc = Dictionary ()
  match core.Instrs.Keys |> Seq.toList with
  | [] -> ()
  | addr :: addrs ->
    let ins = core.Instrs[addr]
    let isNop = NopOracle.isNop core.Handle ins
    (* Start with the second instruction *)
    let num = buildBasicBlockLoop core 0 acc addr (uint64 ins.Length) addr isNop addrs
    core.BasicBlocks <- Array.zeroCreate num
    core.Preds <- Array.create num []
    core.Succs <- Array.create num []
    acc
    |> Seq.iter (fun (KeyValue (_, bbl)) ->
      core.BasicBlocks[bbl.BBLIdx] <- bbl
      )

let addEdge core srcidx dst =
  //assert (core.BasicBlocks.ContainsKey src)
  match core.AddrMap.TryGetValue dst with
  | true, dstidx ->
    core.Succs[srcidx] <- dstidx :: core.Succs[srcidx]
    core.Preds[dstidx] <- srcidx :: core.Preds[dstidx]
  | _ -> ()

let handleNopFallThroughEdge core addr nextAddr =
  addEdge core addr nextAddr

let handleCallEdge core blkAddr ftAddr (lastIns: Instruction) =
  match lastIns.DirectBranchTarget () with
  | true, target ->
    if NonReturningAnalyzer.isNoRetCall core lastIns.Address target then ()
    else addEdge core blkAddr ftAddr
  | _ ->
    if NonReturningAnalyzer.isNoRetIndCall core lastIns.Address then ()
    else addEdge core blkAddr ftAddr (* Indirect call *)

let handleConditionalJumpEdge core blkAddr lastAddr ftAddr (lastIns: Instruction) =
  addEdge core blkAddr ftAddr (* Fall-through *)
  match lastIns.DirectBranchTarget () with
  | true, target -> addEdge core blkAddr target
  | _ ->
    (* Indirect Conditional Jump *)
    match core.JumpTables.TryGetValue lastAddr with
    | true, (_, targets) ->
      Set.iter (addEdge core blkAddr) targets
    | _ -> ()

let getReturnTarget core addr =
  let ins = core.Instrs[addr] :?> MIPS.MIPSInstruction
  match ins.Info.Opcode, ins.Info.Operands with
  | MIPS.Opcode.JR, MIPS.OneOperand (MIPS.OpReg r) ->
    let prevAddr = core.PrevInsAddrs[addr]
    let pprevAddr = core.PrevInsAddrs[prevAddr]
    let prevIns = core.Instrs[prevAddr] :?> MIPS.MIPSInstruction
    let pprevIns = core.Instrs[pprevAddr] :?> MIPS.MIPSInstruction
    match prevIns.Info.Opcode, prevIns.Info.Operands with
    | MIPS.Opcode.DADDIU, MIPS.ThreeOperands (MIPS.OpReg r1, MIPS.OpReg r2, MIPS.OpImm imm1) ->
      if r1 = r2 then
        match pprevIns.Info.Opcode, pprevIns.Info.Operands with
        | MIPS.Opcode.LD, MIPS.TwoOperands (MIPS.OpReg r3, MIPS.OpMem (_, MIPS.Imm imm2, _)) ->
          if r1 = r3 then
            match BinHandle.TryReadUInt (core.Handle, core.GOT + uint64 imm2, WordSize.toByteWidth core.Handle.ISA.WordSize) with
            | Error _ -> None
            | Ok bAddr ->
              Some <| bAddr + uint64 imm1
          else None
        | _ -> None
      else None
    | _ -> None
  | _ -> None

let handleMIPSRetEdge core blkAddr lastAddr =
  match core.Handle.ISA.Arch with
  | Arch.MIPS32 | Arch.MIPS64 ->
    match getReturnTarget core lastAddr with
    | None -> ()
    | Some target ->
      addEdge core blkAddr target
  | _ -> ()

let handleUnconditionalJumpEdge core blkAddr lastAddr (lastIns: Instruction) =
  match lastIns.DirectBranchTarget () with
  | true, target -> addEdge core blkAddr target
  | _ -> (* Indirect jump *)
    match core.JumpTables.TryGetValue lastAddr with
    | true, (_, targets) -> Set.iter (addEdge core blkAddr) targets
    | _ ->
      match core.Handle.ISA.Arch with
      | Arch.MIPS32 | Arch.MIPS64 ->
        match NonReturningAnalyzer.getIndCallTarget core lastAddr with
        | Some target -> addEdge core blkAddr target
        | _ -> handleMIPSRetEdge core blkAddr lastAddr
      | _ -> ()

let handleFallThroughEdge core addr nextAddr =
  addEdge core addr nextAddr

let handleExceptionEdge core addr nextAddr =
  addEdge core addr nextAddr

let buildEdges core =
  core.BasicBlocks
  |> Array.iter (fun bbl ->
    let addr = bbl.BBLAddr
    let idx = bbl.BBLIdx
    let ftAddr = addr + bbl.BBLSize
    if bbl.BBLIsPadding then ()
    elif bbl.BBLIsNop then handleNopFallThroughEdge core idx ftAddr
    else
      let lastAddr = bbl.BBLLastAddr
      let lastIns = core.Instrs[lastAddr]
      if lastIns.IsBBLEnd () then
        if lastIns.IsExit () then ()
        elif lastIns.IsInterrupt () then
          handleFallThroughEdge core idx ftAddr
        elif lastIns.IsCondBranch () then
          handleConditionalJumpEdge core idx lastAddr ftAddr lastIns
        elif lastIns.IsRET () then ()
        elif lastIns.IsCall () then
          handleCallEdge core idx ftAddr lastIns
        else (* Unconditional Jump *)
          handleUnconditionalJumpEdge core idx lastAddr lastIns
      else
        (* Fall-through *)
        handleFallThroughEdge core idx ftAddr
    if core.LandingPads.ContainsKey addr then
      handleExceptionEdge core idx core.LandingPads[addr]
    )

let nopToPad core =
  core.BasicBlocks
  |> Array.fold (fun acc bbl ->
    let idx = bbl.BBLIdx
    if bbl.BBLIsNop then
      if List.isEmpty core.Preds[idx] then idx :: acc
      else acc
    else acc) []
  |> List.iter (fun idx ->
    let bbl = core.BasicBlocks[idx]
    let bbl = { bbl with BBLIsNop = false; BBLIsPadding = true }
    core.BasicBlocks[idx] <- bbl
    core.Succs[idx]
    |> List.iter (fun s ->
      core.Preds[s] <- List.filter (fun x -> x <> idx) core.Preds[s])
    core.Succs[idx] <- []
    )

let run core =
  buildBasicBlocks core
  buildEdges core
  nopToPad core
