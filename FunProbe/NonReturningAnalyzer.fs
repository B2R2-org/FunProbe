module FunProbe.NonReturningAnalyzer

open B2R2
open B2R2.FrontEnd.BinLifter
open B2R2.FrontEnd.BinFile
open B2R2.FrontEnd.BinInterface

/// TODO: Fill this
let nonReturningFuncNames = [
  "abort"
  "assert_fail"
  "assert_rtn"
  "cexit"
  "cxa_bad_cast"
  "cxa_call_unexpected"
  "cxa_terminate"
  "cxa_throw"
  "c_exit"
  "exit"
  "fortify_fail"
  "gfortran_stop_numeric"
  "longjmp"
  "longjmp_chk"
  "panic"
  "pthread_exit"
  "reboot"
  "siglongjmp"
  "stack_chk_fail"
  "Unwind_Resume"
  "ZN10__cxxabiv111__terminateEPFvvE"
  "ZSt9terminatev"
  "ZSt20__throw_length_errorPKc"
  ]

let condNonReturningFuncNames = [
  "error"
  "error_at_line"
]

let loadGOTMap path =
  System.IO.File.ReadAllLines path
  |> Array.fold (fun map line ->
    let tokens = line.Split " "
    let addr = System.Convert.ToUInt64 (tokens[0], 16)
    let name = tokens[1]
    Map.add addr name map
    ) Map.empty

let initExternalNonReturningFuncs opts core =
  let gotMap =
    if opts.MIPSGOTPath <> "" then
      loadGOTMap opts.MIPSGOTPath
    else Map.empty
  let hdl = core.Handle
  hdl.FileInfo.GetLinkageTableEntries ()
  |> Seq.iter (fun entry ->
    if List.contains (entry.FuncName.TrimStart '_') nonReturningFuncNames then
      core.NoRets.Add entry.TrampolineAddress |> ignore
    elif List.contains (entry.FuncName.TrimStart '_') condNonReturningFuncNames then
      core.CondNoRets.Add entry.TrampolineAddress |> ignore
    else ())
  hdl.FileInfo.GetDynamicSymbols ()
  |> Seq.iter (fun symb ->
    if symb.Address <> 0UL then
      if List.contains (symb.Name.TrimStart '_') nonReturningFuncNames then
        core.NoRets.Add symb.Address |> ignore
      elif List.contains (symb.Name.TrimStart '_') condNonReturningFuncNames then
        core.CondNoRets.Add symb.Address |> ignore
      else ()
    else ()
    )
  gotMap
  |> Map.iter (fun addr name ->
    //printfn "reloc %A" symb
    if addr <> 0UL then
      if List.contains (name.TrimStart '_') nonReturningFuncNames then
        core.NoRets.Add addr |> ignore
      elif List.contains (name.TrimStart '_') condNonReturningFuncNames then
        core.CondNoRets.Add addr |> ignore
      else ()
    else ()
    )

let collectCallTargets core =
  core.Instrs
  |> Seq.iter (fun (KeyValue (_, instr)) ->
    if instr.IsCall () then
      match instr.DirectBranchTarget () with
      | true, target when Core.isInText target core -> (* XXX: Consider .text section only *)
        core.CallTargets.Add target |> ignore
      | _ -> ()
    else ())

let rec findFirstBranchIns core addr =
  let ins = core.Instrs[addr]
  if ins.IsBBLEnd () then ins
  else
    let prevAddr = core.PrevInsAddrs[addr]
    findFirstBranchIns core prevAddr

let isIntelNotNoRetHeuristic core addr =
  match core.Handle.ISA.Arch with
  | Arch.IntelX86 ->
    let ins = core.Instrs[addr] :?> Intel.IntelInstruction
    match ins.Opcode with
    | Intel.Opcode.POP -> true
    | _ -> false
  | _ -> false

let collectInternalNonReturningFuncs core =
  core.CallTargets
  |> Seq.iter (fun addr ->
    let prevAddr = core.PrevInsAddrs[addr]
    if prevAddr = 0UL then ()
    else
      let brIns = findFirstBranchIns core prevAddr
      if brIns.IsCall () then
        match brIns.DirectBranchTarget () with
        | true, target ->
          if not <| core.CondNoRets.Contains target then
            if not <| isIntelNotNoRetHeuristic core addr then
              core.NoRets.Add target |> ignore
            else ()
        | _ -> ()
      else ())
  core.Instrs
  |> Seq.iter (fun (KeyValue (addr, instr)) ->
    if not <| core.Instrs.ContainsKey (addr + uint64 instr.Length) then
      let brIns = findFirstBranchIns core addr
      if brIns.IsCall () then
        match brIns.DirectBranchTarget () with
        | true, target ->
          if not <| core.CondNoRets.Contains target then
            core.NoRets.Add target |> ignore
        | _ -> ()
      else ()
    else ())

let run opts core =
  initExternalNonReturningFuncs opts core
  collectCallTargets core
  //collectInternalNonReturningFuncs core

let checkCondNoRetArg core addr =
  match core.Handle.ISA.Arch with
  | Arch.MIPS32 | Arch.MIPS64 ->
    let prevAddr = core.PrevInsAddrs[addr]
    let pprevAddr = core.PrevInsAddrs[prevAddr]
    let ins = core.Instrs[pprevAddr] :?> MIPS.MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | MIPS.Opcode.OR, MIPS.ThreeOperands (_, _, MIPS.OpReg r) ->
      if MIPS.Register.R0 = r then false
      else
        match ins.Immediate () with
        | true, n -> uint64 n = 1UL
        | _ -> false
    | _ ->
      match ins.Immediate () with
      | true, n -> uint64 n = 1UL
      | _ -> false
  | _ ->
    let prevAddr = core.PrevInsAddrs[addr]
    let ins = core.Instrs[prevAddr]
    match ins.Immediate () with
    | true, n -> uint64 n = 1UL
    | _ -> false

let isNoRetCall core lastAddr target =
  if core.NoRets.Contains target then true
  elif core.CondNoRets.Contains target then
    checkCondNoRetArg core lastAddr
  else false

let getIndCallTarget core addr =
  let ins = core.Instrs[addr] :?> MIPS.MIPSInstruction
  match ins.Info.Opcode, ins.Info.Operands with
  | MIPS.Opcode.JALR, MIPS.OneOperand (MIPS.OpReg r) ->
    let prevAddr = core.PrevInsAddrs[addr]
    let prevIns = core.Instrs[prevAddr] :?> MIPS.MIPSInstruction
    match prevIns.Info.Opcode, prevIns.Info.Operands with
    | MIPS.Opcode.LW, MIPS.TwoOperands (MIPS.OpReg r_, MIPS.OpMem (_, MIPS.Imm off, _))
    | MIPS.Opcode.LD, MIPS.TwoOperands (MIPS.OpReg r_, MIPS.OpMem (_, MIPS.Imm off, _)) ->
      if r = r_ then
        let addr = core.GOT + uint64 off
        Some addr
      else None
    | _ -> None
  | _ -> None

let isNoRetIndCall core lastAddr =
  match core.Handle.ISA.Arch with
  | Arch.MIPS32 | Arch.MIPS64 ->
    match getIndCallTarget core lastAddr with
    | None -> false
    | Some target ->
      let wordSize = WordSize.toByteWidth core.Handle.ISA.WordSize
      if core.Handle.FileInfo.IsValidAddr target then
        if core.NoRets.Contains target then isNoRetCall core lastAddr target
        else
          match BinHandle.TryReadUInt (core.Handle, target, wordSize) with
          | Ok target -> isNoRetCall core lastAddr target
          | _ -> false
      else false
  | _ -> false