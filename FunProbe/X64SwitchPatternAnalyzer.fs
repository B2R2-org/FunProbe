module FunProbe.X64SwitchPatternAnalyzer

open B2R2
open B2R2.FrontEnd.BinLifter
open B2R2.FrontEnd.BinLifter.Intel
open B2R2.FrontEnd.BinInterface

let mask32bit value =
  value &&& 0xFFFFFFFFUL

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

type RegJmpPatternMatcher = {
  Target: Register
  Index: Register
  TableAddr: Addr
  NumEntries: int
  EntrySize: int
}

let rec checkNecessaryInstructionsRegJmpNOPIE core cur (st: RegJmpPatternMatcher) newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> IntelInstruction
    match ins.Opcode, ins.Operands with
    | Opcode.JMPNear, OneOperand (OprReg _) ->
      if cur = 0 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        checkNecessaryInstructionsRegJmpNOPIE core 1 st newAddrs addrs
      else None
    | Opcode.JA, _
    | Opcode.JZ, _
    | Opcode.JBE, _ ->
      if cur = 1 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        checkNecessaryInstructionsRegJmpNOPIE core 2 st newAddrs addrs
      else None
    | Opcode.CMP,
      TwoOperands (OprReg _, OprImm (imm, sz))
    | Opcode.CMP,
      TwoOperands (OprMem _, OprImm (imm, sz))
    | Opcode.SUB,
      TwoOperands (OprReg _, OprImm (imm, sz)) ->
      if cur = 2 then
        let imm =
          if sz = 8<rt> then uint64 imm &&& 0xFFUL
          elif sz = 16<rt> then uint64 imm &&& 0xFFFFUL
          elif sz = 32<rt> then uint64 imm &&& 0xFFFFFFFFUL
          elif sz = 64<rt> then uint64 imm
          else Utils.impossible ()
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        let st = { st with NumEntries = int imm + 1 }
        Some (st, newAddrs)
      else None
    | _ -> checkNecessaryInstructionsRegJmpNOPIE core cur st newAddrs addrs

let rec checkTableAndIndexRegJmpNOPIE core st = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> IntelInstruction
    match ins.Opcode, ins.Operands with
    | Opcode.MOV, TwoOperands (OprReg r1, OprMem (None, Some (_, Scale.X8), Some disp, _)) ->
      if st.Target = r1 then
        Some (uint64 disp, st.NumEntries, 8, 0UL)
      else checkTableAndIndexRegJmpNOPIE core st addrs
    | _ -> checkTableAndIndexRegJmpNOPIE core st addrs

let rec checkTableLoadNOPIE core st newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> IntelInstruction
    match ins.Opcode, ins.Operands with
    | Opcode.MOV,
      TwoOperands (OprReg r1, OprMem (Some r2, None, None, _)) ->
      if st.Target = r1 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        let st = { st with Target = r2; EntrySize = 8 }
        Some (st, newAddrs)
      else checkTableLoadNOPIE core st newAddrs addrs
    | _ -> checkTableLoadNOPIE core st newAddrs addrs

let rec checkAdditionAndTable core st = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> IntelInstruction
    match ins.Opcode, ins.Operands with
    | Opcode.ADD,
      TwoOperands (OprReg r1, OprImm (imm, _)) ->
      if st.Target = r1 then
        Some (uint64 imm, st.NumEntries, st.EntrySize, 0UL)
      else checkAdditionAndTable core st addrs
    | _ -> checkAdditionAndTable core st addrs

type MemJmpPatternMatcher = {
  TableAddr: Addr
  NumEntries: int
  EntrySize: int
}

let rec checkNecessaryInstructionsMemJmpNOPIE core cur (st: MemJmpPatternMatcher) newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> IntelInstruction
    match ins.Opcode, ins.Operands with
    | Opcode.JMPNear, OneOperand (OprMem (None, Some (_, Scale.X8), Some disp, _)) ->
      if cur = 0 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        let st = { st with TableAddr = uint64 disp; EntrySize = 8 }
        checkNecessaryInstructionsMemJmpNOPIE core 1 st newAddrs addrs
      else None
    | Opcode.JA, _
    | Opcode.JZ, _
    | Opcode.JBE, _ ->
      if cur = 1 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        checkNecessaryInstructionsMemJmpNOPIE core 2 st newAddrs addrs
      else None
    | Opcode.CMP,
      TwoOperands (OprReg _, OprImm (imm, sz))
    | Opcode.CMP,
      TwoOperands (OprMem _, OprImm (imm, sz))
    | Opcode.SUB,
      TwoOperands (OprReg _, OprImm (imm, sz)) ->
      if cur = 2 then
        let imm =
          if sz = 8<rt> then uint64 imm &&& 0xFFUL
          elif sz = 16<rt> then uint64 imm &&& 0xFFFFUL
          elif sz = 32<rt> then uint64 imm &&& 0xFFFFFFFFUL
          elif sz = 64<rt> then uint64 imm
          else Utils.impossible ()
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        let st = { st with NumEntries = int imm + 1 }
        Some (st, newAddrs)
      else checkNecessaryInstructionsMemJmpNOPIE core cur st newAddrs addrs
    | _ -> checkNecessaryInstructionsMemJmpNOPIE core cur st newAddrs addrs

let findTableBaseX64ELFNOPIE core addr (ins: Instruction) =
  let ins = ins :?> IntelInstruction
  match ins.Operands with
  | OneOperand (OprReg reg) ->
    let addrs = collectAddrs core 1 addr [addr]
    let st = { Target = reg; Index = reg; TableAddr = 0UL; NumEntries = 0; EntrySize = 0 }
    match checkNecessaryInstructionsRegJmpNOPIE core 0 st addrs addrs with
    | Some (st, addrs) ->
      match checkTableAndIndexRegJmpNOPIE core st addrs with
      | Some v ->
        //printfn "Looks like jump table"
        Some v
      | None ->
        match checkTableLoadNOPIE core st addrs addrs with
        | Some (st, addrs) ->
          match checkAdditionAndTable core st addrs with
          | Some v ->
            //printfn "Looks like jump table"
            Some v
          | _ ->
            //printfn "Looks like jump table"
            None
        | _ -> None
    | _ -> None
  | OneOperand (OprMem (None, Some (_, Scale.X8), Some disp, _)) ->
    let addrs = collectAddrs core 1 addr [addr]
    let st = { TableAddr = 0UL; NumEntries = 0; EntrySize = 0 }
    match checkNecessaryInstructionsMemJmpNOPIE core 0 st addrs addrs with
    | Some (st, _) ->
      //printfn "Looks like jump table"
      Some (st.TableAddr, st.NumEntries, st.EntrySize, 0UL)
    | _ -> None
  | _ -> None

let readTableEntryX64ELFNOPIE core (cur: Addr) size =
  BinHandle.ReadUInt (core.Handle, cur, size)

let rec readTableEntriesX64ELFNOPIE core tbl cur tblSize size entries =
  if tbl + uint64 tblSize * uint64 size = cur then entries
  else
    let target = readTableEntryX64ELFNOPIE core cur size
    let entries = Set.add target entries
    let cur = cur + uint64 size
    readTableEntriesX64ELFNOPIE core tbl cur tblSize size entries

let rec getUpperAddrs core addr addrs =
  let addrs = addr :: addrs
  if core.CallTargets.Contains addr then addrs
  else
    let prevAddr = core.PrevInsAddrs[addr]
    if prevAddr = 0UL then addrs
    else getUpperAddrs core prevAddr addrs

let rec getLowerAddrs core addr addrs =
  if core.CallTargets.Contains addr then addrs
  elif core.Instrs.ContainsKey addr |> not then addrs
  else
    let addrs = addr :: addrs
    let nextAddr = addr + 4UL
    getLowerAddrs core nextAddr addrs

let rec getAddrs core addr =
  getUpperAddrs core addr [addr]
  |> getLowerAddrs core addr
  |> List.rev

let verifyTableEntryX64ELFPIE core tbl (cur: Addr) size =
  match BinHandle.TryReadUInt (core.Handle, cur, size) with
  | Ok target -> Some ((target + tbl) |> mask32bit)
  | _ -> None

let rec verifyTableEntriesX64ELFPIE core tbl cur tblSize size entries =
  if tbl + uint64 tblSize * uint64 size = cur then Some entries
  else
    match verifyTableEntryX64ELFPIE core tbl cur size with
    | Some target ->
      let entries = Set.add target entries
      let cur = cur + uint64 size
      verifyTableEntriesX64ELFPIE core tbl cur tblSize size entries
    | None -> None

let readTableEntryX64ELFPIE core tbl (cur: Addr) size =
  BinHandle.ReadUInt (core.Handle, cur, size) + tbl |> mask32bit

let rec readTableEntriesX64ELFPIE core tbl cur tblSize size entries =
  if tbl + uint64 tblSize * uint64 size = cur then entries
  else
    let target = readTableEntryX64ELFPIE core tbl cur size
    let entries = Set.add target entries
    let cur = cur + uint64 size
    readTableEntriesX64ELFPIE core tbl cur tblSize size entries

let verifyTable core tbl tblSize size =
  if core.Handle.FileInfo.IsValidAddr tbl then
    match verifyTableEntriesX64ELFPIE core tbl tbl tblSize size Set.empty with
    | Some entries ->
      Set.forall (fun a -> core.Instrs.ContainsKey a) entries
    | None -> false
  else false

let rec checkNecessaryInstructionsPIE core cur (st: RegJmpPatternMatcher) newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> IntelInstruction
    match ins.Opcode, ins.Operands with
    | Opcode.JMPNear, OneOperand (OprReg _) ->
      if cur = 0 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        checkNecessaryInstructionsPIE core 1 st newAddrs addrs
      else None
    | Opcode.ADD, TwoOperands (OprReg r1, OprReg _) ->
      if cur = 1 then
        if st.Target = r1 then
          let newAddrs = List.filter (fun a -> a <> addr) newAddrs
          checkNecessaryInstructionsPIE core 2 st newAddrs addrs
        else checkNecessaryInstructionsPIE core cur st newAddrs addrs
      else checkNecessaryInstructionsPIE core cur st newAddrs addrs
    | Opcode.JA, _
    | Opcode.JZ, _
    | Opcode.JBE, _ ->
      if cur = 2 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        checkNecessaryInstructionsPIE core 3 st newAddrs addrs
      else None
    | Opcode.CMP,
      TwoOperands (OprReg _, OprImm (imm, sz))
    | Opcode.CMP,
      TwoOperands (OprMem _, OprImm (imm, sz))
    | Opcode.SUB,
      TwoOperands (OprReg _, OprImm (imm, sz)) ->
      if cur = 3 then
        let imm =
          if sz = 8<rt> then uint64 imm &&& 0xFFUL
          elif sz = 16<rt> then uint64 imm &&& 0xFFFFUL
          elif sz = 32<rt> then uint64 imm &&& 0xFFFFFFFFUL
          elif sz = 64<rt> then uint64 imm
          else Utils.impossible ()
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        let st = { st with NumEntries = int imm + 1 }
        Some (st, newAddrs)
      else None
    | _ -> checkNecessaryInstructionsPIE core cur st newAddrs addrs

let rec checkTableLoadAndIndexPIE core st newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> IntelInstruction
    match ins.Opcode, ins.Operands with
    | Opcode.MOVSXD, TwoOperands (OprReg r1, OprMem (Some r2, Some (r3, _), _, _)) ->
      if st.Target = r1 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        let st = { st with Target = r2; Index = r3; EntrySize = 4 }
        Some (st, newAddrs)
      else checkTableLoadAndIndexPIE core st newAddrs addrs
    | Opcode.MOV, TwoOperands (OprReg r1, OprMem (Some r2, Some (r3, _), _, _)) ->
      let rBay = core.Handle.RegisterBay
      let r1id = Register.toRegID r1
      let targetid = Register.toRegID st.Target
      if Array.contains targetid (rBay.GetRegisterAliases r1id) then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        let st = { st with Target = r2; Index = r3; EntrySize = 4 }
        Some (st, newAddrs)
      else checkTableLoadAndIndexPIE core st newAddrs addrs
    | _ -> checkTableLoadAndIndexPIE core st newAddrs addrs

let rec checkTableAddr core st prevAddr = function
  | [] ->
    let addrs = getAddrs core prevAddr
    followRegAndFind core st addrs
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> IntelInstruction
    match ins.Opcode, ins.Operands with
    | Opcode.LEA, TwoOperands (OprReg r1, OprMem (Some r2, None, Some disp, _)) ->
      let rBay = core.Handle.RegisterBay
      let r2id = Register.toRegID r2
      if st.Target = r1 && r2id = rBay.ProgramCounter then
        Some (addr + uint64 ins.Length + uint64 disp, st.NumEntries, st.EntrySize, 0UL)
      else checkTableAddr core st addr addrs
    | _ -> checkTableAddr core st addr addrs

and followRegAndFind core st = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> IntelInstruction
    match ins.Opcode, ins.Operands with
    | Opcode.LEA, TwoOperands (OprReg r1, OprMem (Some r2, None, Some disp, _)) ->
      let rBay = core.Handle.RegisterBay
      let r2id = Register.toRegID r2
      if st.Target = r1 && r2id = rBay.ProgramCounter then
        if verifyTable core (addr + uint64 ins.Length + uint64 disp) st.NumEntries st.EntrySize then
          Some (addr + uint64 ins.Length + uint64 disp, st.NumEntries, st.EntrySize, 0UL)
        else None
      else followRegAndFind core st addrs
    | _ -> followRegAndFind core st addrs

let findTableBaseX64ELFPIE core addr (ins: Instruction) =
  let ins = ins :?> IntelInstruction
  match ins.Operands with
  | OneOperand (OprReg reg) ->
    let addrs = collectAddrs core 1 addr [addr]
    let st = { Target = reg; Index = reg; TableAddr = 0UL; NumEntries = 0; EntrySize = 0 }
    match checkNecessaryInstructionsPIE core 0 st addrs addrs with
    | Some (st, addrs) ->
      match checkTableLoadAndIndexPIE core st addrs addrs with
      | Some (st, addrs) ->
        //printfn "Looks like jump table"
        checkTableAddr core st addr addrs
      | None ->
        //printfn "Looks like jump table"
        None
    | _ -> None
  | _ -> None
