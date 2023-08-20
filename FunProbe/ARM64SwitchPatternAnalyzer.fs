module FunProbe.ARM64SwitchPatternAnalyzer

open B2R2
open B2R2.FrontEnd.BinLifter
open B2R2.FrontEnd.BinLifter.ARM64
open B2R2.FrontEnd.BinInterface

type PatternMatcher = {
  St: int
  Target: Register
  Ref: Register
  TableAddr: Addr
  RefAddr: Addr
  EntrySize: int
}

let maskPage value =
  value &&& 0xFFFFFFFFFFFFF000UL

let rec collectAddrs core nBlks addr addrs =
  let prevAddr = core.PrevInsAddrs[addr]
  let prevIns = core.Instrs[prevAddr]
  if prevIns.IsBBLEnd () then
    if nBlks = 0 then List.rev addrs
    else
      if prevIns.IsCondBranch () then
        collectAddrs core (nBlks - 1) prevAddr (prevAddr :: addrs)
      else
        if core.References.ContainsKey addr && core.References[addr].Count > 0 then
          let prevAddr = core.References[addr][0]
          collectAddrs core (nBlks - 1) prevAddr (prevAddr :: addrs)
        else []
  else collectAddrs core nBlks prevAddr (prevAddr :: addrs)

let rec findTableBaseAArch64ELFPattern1 core st = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> ARM64Instruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.ADD,
      FourOperands (OprRegister r1, OprRegister r2, OprRegister r3, Shift _)
    | Opcode.ADD,
      FourOperands (OprRegister r1, OprRegister r2, OprRegister r3, ExtReg _) ->
      if st.St = 0 then
        if st.Target = r1 then
          let st = { st with St = 1; Target = r3; Ref = r2 }
          findTableBaseAArch64ELFPattern1 core st addrs
        else findTableBaseAArch64ELFPattern1 core st addrs
      else findTableBaseAArch64ELFPattern1 core st addrs
    | Opcode.LDRB,
      TwoOperands (OprRegister r1, Memory (BaseMode (RegOffset (r2, _, _)))) ->
      if st.St = 1 then
        let rBay = core.Handle.RegisterBay
        let r1id = Register.toRegID r1
        let rid = Register.toRegID st.Target
        if Array.contains rid (rBay.GetRegisterAliases r1id) then
          let st = { st with St = 2; Target = r2; EntrySize = 1 }
          findTableBaseAArch64ELFPattern1 core st addrs
        else findTableBaseAArch64ELFPattern1 core st addrs
      else findTableBaseAArch64ELFPattern1 core st addrs
    | Opcode.LDRH,
      TwoOperands (OprRegister r1, Memory (BaseMode (RegOffset (r2, _, _)))) ->
      if st.St = 1 then
        let rBay = core.Handle.RegisterBay
        let r1id = Register.toRegID r1
        let rid = Register.toRegID st.Target
        if Array.contains rid (rBay.GetRegisterAliases r1id) then
          let st = { st with St = 2; Target = r2; EntrySize = 2 }
          findTableBaseAArch64ELFPattern1 core st addrs
        else findTableBaseAArch64ELFPattern1 core st addrs
      else findTableBaseAArch64ELFPattern1 core st addrs
    | Opcode.LDR,
      TwoOperands (OprRegister r1, Memory (BaseMode (RegOffset (r2, _, _)))) ->
      if st.St = 1 then
        let rBay = core.Handle.RegisterBay
        let r1id = Register.toRegID r1
        let rid = Register.toRegID st.Target
        if Array.contains rid (rBay.GetRegisterAliases r1id) then
          let st = { st with St = 2; Target = r2; EntrySize = 4 }
          findTableBaseAArch64ELFPattern1 core st addrs
        else findTableBaseAArch64ELFPattern1 core st addrs
      else findTableBaseAArch64ELFPattern1 core st addrs
    | Opcode.ADR,
      TwoOperands (OprRegister r1, Memory (LiteralMode (ImmOffset (Lbl imm)))) ->
      if st.St >= 1 && st.St <= 4 then
        if st.Ref = r1 then
          let st = { st with RefAddr = addr + uint64 imm }
          findTableBaseAArch64ELFPattern1 core st addrs
        else findTableBaseAArch64ELFPattern1 core st addrs
      else findTableBaseAArch64ELFPattern1 core st addrs
    | Opcode.ADD,
      FourOperands (OprRegister r1, OprRegister r2, Immediate imm, _) ->
      if st.St = 2 then
        if st.Target = r1 && r1 = r2 then
          let st = { st with St = 3; TableAddr = uint64 imm }
          findTableBaseAArch64ELFPattern1 core st addrs
        else findTableBaseAArch64ELFPattern1 core st addrs
      else findTableBaseAArch64ELFPattern1 core st addrs
    | Opcode.ADRP,
      TwoOperands (OprRegister r1, Memory (LiteralMode (ImmOffset (Lbl off)))) ->
      if st.St = 3 then
        if st.Target = r1 then
          let page = maskPage addr
          let st = { st with St = 4; TableAddr = page + uint64 off + st.TableAddr }
          findTableBaseAArch64ELFPattern1 core st addrs
        else findTableBaseAArch64ELFPattern1 core st addrs
      else findTableBaseAArch64ELFPattern1 core st addrs
    | Opcode.BHI,
      _
    | Opcode.BLS,
      _ ->
      if st.St = 4 then
        let st = { st with St = 5 }
        findTableBaseAArch64ELFPattern1 core st addrs
      else findTableBaseAArch64ELFPattern1 core st addrs
    | Opcode.CMP,
      ThreeOperands (OprRegister _, Immediate size, _) ->
      if st.St = 5 then
        Some (st.TableAddr, int size + 1, st.EntrySize, st.RefAddr)
      else findTableBaseAArch64ELFPattern1 core st addrs
    | _ -> findTableBaseAArch64ELFPattern1 core st addrs

let rec findRegDef core reg target addr =
  if addr = 0UL then None
  elif core.CallTargets.Contains addr then None
  else
    let ins = core.Instrs[addr] :?> ARM64Instruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.ADD,
      FourOperands (OprRegister r1, OprRegister r2, Immediate imm, _) ->
      if reg = r1 then
        let prevAddr = core.PrevInsAddrs[addr]
        findRegDef core r2 (uint64 imm) prevAddr
      else
        let prevAddr = core.PrevInsAddrs[addr]
        findRegDef core reg target prevAddr
    | Opcode.ADRP,
      TwoOperands (OprRegister r1, Memory (LiteralMode (ImmOffset (Lbl off)))) ->
      if reg = r1 && target <> 0UL then
        let page = maskPage addr
        Some (page + uint64 off + target)
      else
        let prevAddr = core.PrevInsAddrs[addr]
        findRegDef core reg target prevAddr
    | _ ->
      let prevAddr = core.PrevInsAddrs[addr]
      findRegDef core reg target prevAddr

let rec findMemDef core reg off target addr =
  if addr = 0UL then None
  elif core.CallTargets.Contains addr then None
  else
    let ins = core.Instrs[addr] :?> ARM64Instruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.STR,
      TwoOperands (OprRegister r1, Memory (BaseMode (ImmOffset (BaseOffset (r2, Some imm)))))
    | Opcode.STP,
      ThreeOperands (OprRegister r1, OprRegister _, Memory (BaseMode (ImmOffset (BaseOffset (r2, Some imm))))) ->
      if reg = r2 && off = imm then
        let prevAddr = core.PrevInsAddrs[addr]
        findRegDef core r1 0UL prevAddr
      else
        let prevAddr = core.PrevInsAddrs[addr]
        findMemDef core reg off target prevAddr
    | _ ->
      let prevAddr = core.PrevInsAddrs[addr]
      findMemDef core reg off target prevAddr

let rec findTableBaseAArch64ELFPattern2 core st = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> ARM64Instruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.ADD,
      FourOperands (OprRegister r1, OprRegister r2, OprRegister r3, Shift _)
    | Opcode.ADD,
      FourOperands (OprRegister r1, OprRegister r2, OprRegister r3, ExtReg _) ->
      if st.St = 0 then
        if st.Target = r1 then
          let st = { st with St = 1; Target = r3; Ref = r2 }
          findTableBaseAArch64ELFPattern2 core st addrs
        else findTableBaseAArch64ELFPattern2 core st addrs
      else findTableBaseAArch64ELFPattern2 core st addrs
    | Opcode.LDRB,
      TwoOperands (OprRegister r1, Memory (BaseMode (RegOffset (r2, _, _)))) ->
      if st.St = 1 then
        let rBay = core.Handle.RegisterBay
        let r1id = Register.toRegID r1
        let rid = Register.toRegID st.Target
        if Array.contains rid (rBay.GetRegisterAliases r1id) then
          let st = { st with St = 2; Target = r2; EntrySize = 1 }
          findTableBaseAArch64ELFPattern2 core st addrs
        else findTableBaseAArch64ELFPattern2 core st addrs
      else findTableBaseAArch64ELFPattern2 core st addrs
    | Opcode.LDRH,
      TwoOperands (OprRegister r1, Memory (BaseMode (RegOffset (r2, _, _)))) ->
      if st.St = 1 then
        let rBay = core.Handle.RegisterBay
        let r1id = Register.toRegID r1
        let rid = Register.toRegID st.Target
        if Array.contains rid (rBay.GetRegisterAliases r1id) then
          let st = { st with St = 2; Target = r2; EntrySize = 2 }
          findTableBaseAArch64ELFPattern2 core st addrs
        else findTableBaseAArch64ELFPattern2 core st addrs
      else findTableBaseAArch64ELFPattern2 core st addrs
    | Opcode.LDR,
      TwoOperands (OprRegister r1, Memory (BaseMode (RegOffset (r2, _, _)))) ->
      if st.St = 1 then
        let rBay = core.Handle.RegisterBay
        let r1id = Register.toRegID r1
        let rid = Register.toRegID st.Target
        if Array.contains rid (rBay.GetRegisterAliases r1id) then
          let st = { st with St = 2; Target = r2; EntrySize = 4 }
          findTableBaseAArch64ELFPattern2 core st addrs
        else findTableBaseAArch64ELFPattern2 core st addrs
      else findTableBaseAArch64ELFPattern2 core st addrs
    | Opcode.ADR,
      TwoOperands (OprRegister r1, Memory (LiteralMode (ImmOffset (Lbl imm)))) ->
      if st.St >= 1 && st.St <= 2 then
        if st.Ref = r1 then
          let st = { st with RefAddr = addr + uint64 imm }
          findTableBaseAArch64ELFPattern2 core st addrs
        else findTableBaseAArch64ELFPattern2 core st addrs
      else findTableBaseAArch64ELFPattern2 core st addrs
    | Opcode.BHI,
      _
    | Opcode.BLS,
      _ ->
      if st.St = 2 then
        let st = { st with St = 3 }
        findTableBaseAArch64ELFPattern2 core st addrs
      else findTableBaseAArch64ELFPattern2 core st addrs
    | Opcode.CMP,
      ThreeOperands (OprRegister _, Immediate size, _) ->
      if st.St = 3 then
        match findRegDef core st.Target 0UL addr with
        | Some c ->
          Some (c, int size + 1, st.EntrySize, st.RefAddr)
        | None -> None
      else findTableBaseAArch64ELFPattern2 core st addrs
    | _ -> findTableBaseAArch64ELFPattern2 core st addrs

let rec findTableBaseAArch64ELFPattern3 core st = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> ARM64Instruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.ADD,
      FourOperands (OprRegister r1, OprRegister r2, OprRegister r3, Shift _)
    | Opcode.ADD,
      FourOperands (OprRegister r1, OprRegister r2, OprRegister r3, ExtReg _) ->
      if st.St = 0 then
        if st.Target = r1 then
          let st = { st with St = 1; Target = r3; Ref = r2 }
          findTableBaseAArch64ELFPattern3 core st addrs
        else findTableBaseAArch64ELFPattern3 core st addrs
      else findTableBaseAArch64ELFPattern3 core st addrs
    | Opcode.LDRB,
      TwoOperands (OprRegister r1, Memory (BaseMode (RegOffset (r2, _, _)))) ->
      if st.St = 1 then
        let rBay = core.Handle.RegisterBay
        let r1id = Register.toRegID r1
        let rid = Register.toRegID st.Target
        if Array.contains rid (rBay.GetRegisterAliases r1id) then
          let st = { st with St = 2; Target = r2; EntrySize = 1 }
          findTableBaseAArch64ELFPattern3 core st addrs
        else findTableBaseAArch64ELFPattern3 core st addrs
      else findTableBaseAArch64ELFPattern3 core st addrs
    | Opcode.LDRH,
      TwoOperands (OprRegister r1, Memory (BaseMode (RegOffset (r2, _, _)))) ->
      if st.St = 1 then
        let rBay = core.Handle.RegisterBay
        let r1id = Register.toRegID r1
        let rid = Register.toRegID st.Target
        if Array.contains rid (rBay.GetRegisterAliases r1id) then
          let st = { st with St = 2; Target = r2; EntrySize = 2 }
          findTableBaseAArch64ELFPattern3 core st addrs
        else findTableBaseAArch64ELFPattern3 core st addrs
      else findTableBaseAArch64ELFPattern3 core st addrs
    | Opcode.LDR,
      TwoOperands (OprRegister r1, Memory (BaseMode (RegOffset (r2, _, _)))) ->
      if st.St = 1 then
        let rBay = core.Handle.RegisterBay
        let r1id = Register.toRegID r1
        let rid = Register.toRegID st.Target
        if Array.contains rid (rBay.GetRegisterAliases r1id) then
          let st = { st with St = 2; Target = r2; EntrySize = 4 }
          findTableBaseAArch64ELFPattern3 core st addrs
        else findTableBaseAArch64ELFPattern3 core st addrs
      else findTableBaseAArch64ELFPattern3 core st addrs
    | Opcode.ADR,
      TwoOperands (OprRegister r1, Memory (LiteralMode (ImmOffset (Lbl imm)))) ->
      if st.St >= 1 && st.St <= 3 then
        if st.Ref = r1 then
          let st = { st with RefAddr = addr + uint64 imm }
          findTableBaseAArch64ELFPattern3 core st addrs
        else findTableBaseAArch64ELFPattern3 core st addrs
      else findTableBaseAArch64ELFPattern3 core st addrs
    | Opcode.LDR,
      TwoOperands (OprRegister r1, Memory (BaseMode (ImmOffset (BaseOffset (r2, Some imm))))) ->
      if st.St = 2 then
        if st.Target = r1 then
          match findMemDef core r2 imm 0UL addr with
          | Some c ->
            let st = { st with St = 3; TableAddr = c }
            findTableBaseAArch64ELFPattern3 core st addrs
          | _ ->
            findTableBaseAArch64ELFPattern3 core st addrs
        else findTableBaseAArch64ELFPattern3 core st addrs
      else findTableBaseAArch64ELFPattern3 core st addrs
    | Opcode.BHI,
      _
    | Opcode.BLS,
      _ ->
      if st.St = 3 then
        let st = { st with St = 4 }
        findTableBaseAArch64ELFPattern3 core st addrs
      else findTableBaseAArch64ELFPattern3 core st addrs
    | Opcode.CMP,
      ThreeOperands (OprRegister _, Immediate size, _) ->
      if st.St = 4 then
        Some (st.TableAddr, int size + 1, st.EntrySize, st.RefAddr)
      else findTableBaseAArch64ELFPattern3 core st addrs
    | _ -> findTableBaseAArch64ELFPattern3 core st addrs

let findTableBaseAArch64ELF core addr (ins: Instruction) =
  let ins = ins :?> ARM64Instruction
  match ins.Info.Operands with
  | OneOperand (OprRegister reg) ->
    let addrs = collectAddrs core 1 addr [addr]
    let st = { St = 0; Target = reg; Ref = reg; TableAddr = 0UL; RefAddr = 0UL; EntrySize = 0 }
    None
    |> Option.orElseWith (fun _ ->
      findTableBaseAArch64ELFPattern1 core st addrs)
    |> Option.orElseWith (fun _ ->
      findTableBaseAArch64ELFPattern3 core st addrs)
    |> Option.orElseWith (fun _ ->
      findTableBaseAArch64ELFPattern2 core st addrs)
  | _ -> None

let readTableEntryAArch64ELF core refAddr (cur: Addr) size =
  let entry = BinHandle.ReadInt (core.Handle, cur, size) |> uint64
  refAddr + entry * 4UL

let rec readTableEntriesAArch64ELF core tbl cur tblSize size refAddr entries =
  if tbl + uint64 tblSize * uint64 size = cur then entries
  else
    let target = readTableEntryAArch64ELF core refAddr cur size
    let entries = Set.add target entries
    let cur = cur + uint64 size
    readTableEntriesAArch64ELF core tbl cur tblSize size refAddr entries
