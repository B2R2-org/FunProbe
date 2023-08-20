module FunProbe.ARM32SwitchPatternAnalyzer

open B2R2
open B2R2.FrontEnd.BinLifter
open B2R2.FrontEnd.BinLifter.ARM32
open B2R2.FrontEnd.BinInterface

let mask32bit value =
  value &&& 0xFFFFFFFFUL

let rec findTableBase core addr =
  if core.Instrs.ContainsKey addr |> not then None
  else
    let ins = core.Instrs[addr] :?> ARM32Instruction
    match ins.Opcode, ins.Operands with
    | Opcode.ADD,
      ThreeOperands (OprReg _, OprReg Register.PC, OprImm imm) ->
      Some (addr + 8UL + uint64 imm)
    | _ ->
      if core.References.ContainsKey addr then None
      else findTableBase core (addr - 4UL)

let rec getTableSize core tblAddr cur nEntries =
  if core.Instrs.ContainsKey cur then nEntries
  elif cur <> tblAddr && core.DataReferences.Contains cur then nEntries
  else
    let cur = cur + 4UL
    let nEntries = nEntries + 1
    getTableSize core tblAddr cur nEntries

let findTableBaseARMELFPIE core addr (ins: Instruction) =
  let ins = ins :?> ARM32Instruction
  match ins.Opcode, ins.Operands with
  | Opcode.ADD,
    FourOperands (OprReg Register.PC, OprReg r, _, _) ->
    let rBay = core.Handle.RegisterBay
    if rBay.ProgramCounter = Register.toRegID r then
      //let addrs = collectAddrs core 1 addr [addr]
      Some (addr + 4UL, 0, 0, addr + 4UL)
    else
      match findTableBase core (addr - 4UL) with
      | Some tblAddr ->
        let tblSize = getTableSize core tblAddr tblAddr 0
        let entrySize = 4
        Some (tblAddr, tblSize, entrySize, 0UL)
      | None -> None
  | _ -> None

let rec readJumpEncodedTableEntriesARMELFPIE core addr refs entries =
  if Set.contains addr refs then entries
  elif core.Instrs.ContainsKey addr |> not then entries
  else
    let ins = core.Instrs[addr] :?> ARM32Instruction
    match ins.Opcode with
    | Opcode.B ->
      match ins.DirectBranchTarget () with
      | true, target ->
        let refs = Set.add target refs
        let entries = Set.add addr entries
        readJumpEncodedTableEntriesARMELFPIE core (addr + 4UL) refs entries
      | _ -> entries
    | _ -> entries

let readTableEntryARMELFPIE core tblAddr (cur: Addr) size =
  tblAddr + BinHandle.ReadUInt (core.Handle, cur, size) |> mask32bit

let rec readJumpTableEntriesARMELFPIE core tbl cur tblSize size entries =
  if tbl + uint64 tblSize * uint64 size = cur then entries
  else
    let target = readTableEntryARMELFPIE core tbl cur size
    let entries = Set.add target entries
    let cur = cur + uint64 size
    readJumpTableEntriesARMELFPIE core tbl cur tblSize size entries

let readTableEntriesARMELFPIE core tbl cur tblSize size refAddr entries =
  if refAddr <> 0UL then
    readJumpEncodedTableEntriesARMELFPIE core refAddr Set.empty entries
  else readJumpTableEntriesARMELFPIE core tbl cur tblSize size entries

let findTableBaseARMELFNOPIE core addr (ins: Instruction) =
  let ins = ins :?> ARM32Instruction
  match ins.Opcode, ins.Operands with
  | Opcode.LDR,
    TwoOperands (OprReg Register.PC, OprMemory (OffsetMode (RegOffset (r, _, _, _)))) ->
    let rBay = core.Handle.RegisterBay
    if rBay.ProgramCounter = Register.toRegID r then
      let tblAddr = addr + 8UL
      let tblSize = getTableSize core tblAddr (addr + 8UL) 0
      let entrySize = 4
      Some (tblAddr, tblSize, entrySize, 0UL)
    else
      let tblAddr = addr + 4UL
      let tblSize = getTableSize core tblAddr (addr + 4UL) 0
      let entrySize = 4
      Some (tblAddr, tblSize, entrySize, 0UL)
  | _ -> None

let readTableEntryARMELFNOPIE core (cur: Addr) size =
  BinHandle.ReadInt (core.Handle, cur, size) |> uint64

let rec readTableEntriesARMELFNOPIE core tbl cur tblSize size entries =
  if tbl + uint64 tblSize * uint64 size = cur then entries
  else
    let target = readTableEntryARMELFNOPIE core cur size
    let entries = Set.add target entries
    let cur = cur + uint64 size
    readTableEntriesARMELFNOPIE core tbl cur tblSize size entries
