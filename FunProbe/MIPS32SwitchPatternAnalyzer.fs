module FunProbe.MIPS32SwitchPatternAnalyzer

open B2R2
open B2R2.FrontEnd.BinLifter
open B2R2.FrontEnd.BinLifter.MIPS
open B2R2.FrontEnd.BinInterface

type PatternMatcher = {
  Target: Register
  Index: Register
  TableAddr: Addr
  NumEntries: int
  EntrySize: int
}

let mask32bit value =
  value &&& 0xFFFFFFFFUL

let rec collectAddrs core nBlks addr addrs =
  let prevAddr = core.PrevInsAddrs[addr]
  if prevAddr = 0UL then addrs |> List.rev
  else
    let prevIns = core.Instrs[prevAddr]
    if prevIns.IsBBLEnd () && not <| prevIns.IsCall () then
      if nBlks = 0 then addrs |> List.rev
      else
        let nextAddr = prevAddr + uint64 prevIns.Length
        collectAddrs core (nBlks - 1) prevAddr (nextAddr :: prevAddr :: List.tail addrs)
    else collectAddrs core nBlks prevAddr (prevAddr :: addrs)

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

let verifyTableEntryMIPS32ELFNOPIE core (cur: Addr) size =
  BinHandle.TryReadUInt (core.Handle, cur, size)

let rec verifyTableEntriesMIPS32ELFNOPIE core tbl cur tblSize size entries =
  if tbl + uint64 tblSize * uint64 size = cur then Some entries
  else
    match verifyTableEntryMIPS32ELFNOPIE core cur size with
    | Ok target ->
      let entries = Set.add target entries
      let cur = cur + uint64 size
      verifyTableEntriesMIPS32ELFNOPIE core tbl cur tblSize size entries
    | _ -> None

let readTableEntryMIPS32ELFNOPIE core (cur: Addr) size =
  BinHandle.ReadUInt (core.Handle, cur, size)

let rec readTableEntriesMIPS32ELFNOPIE core tbl cur tblSize size entries =
  if tbl + uint64 tblSize * uint64 size = cur then entries
  else
    let target = readTableEntryMIPS32ELFNOPIE core cur size
    let entries = Set.add target entries
    let cur = cur + uint64 size
    readTableEntriesMIPS32ELFNOPIE core tbl cur tblSize size entries

let verifyTableNOPIE core tbl tblSize size =
  if core.Handle.FileInfo.IsValidAddr (mask32bit tbl) then
    match verifyTableEntriesMIPS32ELFNOPIE core tbl tbl tblSize size Set.empty with
    | Some entries ->
      Set.forall (fun a -> core.Instrs.ContainsKey a) entries
    | None -> false
  else false

let rec checkNecessaryInstructionsNOPIE core cur st newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.JR, OneOperand (OpReg reg) ->
      if cur = 0 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        checkNecessaryInstructionsNOPIE core 1 st newAddrs addrs
      else None
    | Opcode.BEQ, _ ->
      if cur = 1 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        checkNecessaryInstructionsNOPIE core 2 st newAddrs addrs
      else None
    | Opcode.SLTIU, ThreeOperands (OpReg _, OpReg _, OpImm imm) ->
      if cur = 2 then
        if imm < 0x1000UL then
          let newAddrs = List.filter (fun a -> a <> addr) newAddrs
          let st = { st with NumEntries = int imm }
          Some (st, newAddrs)
        else checkNecessaryInstructionsNOPIE core cur st newAddrs addrs
      else None
    | _ -> checkNecessaryInstructionsNOPIE core cur st newAddrs addrs

let rec checkTableLoadNOPIE core st newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.LW,
      TwoOperands (OpReg r1, OpMem (r2, Imm imm, _)) ->
      if st.Target = r1 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        let st = { st with Target = r2; TableAddr = uint64 imm; EntrySize = 4 }
        Some (st, newAddrs)
      else checkTableLoadNOPIE core st newAddrs addrs
    | _ -> checkTableLoadNOPIE core st newAddrs addrs

let rec checkAdditionNOPIE core st newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.ADDU,
      ThreeOperands (OpReg r1, OpReg r2, OpReg r3) ->
      if st.Target = r1 then
        let st = { st with Target = r2; Index = r3 }
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        Some (st, newAddrs)
      else checkAdditionNOPIE core st newAddrs addrs
    | _ -> checkAdditionNOPIE core st newAddrs addrs

let rec checkIndexNOPIE core st newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.SLL,
      ThreeOperands (OpReg r1, OpReg _, OpShiftAmount _) ->
      if st.Target = r1 then
        let st = { st with Target = st.Index; Index = st.Target }
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        Some (st, newAddrs)
      elif st.Index = r1 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        Some (st, newAddrs)
      else checkIndexNOPIE core st newAddrs addrs
    | _ -> checkIndexNOPIE core st newAddrs addrs

let rec checkTableAddrNOPIEGuess core st = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.ADDIU,
      ThreeOperands (OpReg r1, OpReg r2, OpImm imm) ->
      if st.Target = r1 && st.TableAddr = 0UL then
        let rBay = core.Handle.RegisterBay
        let r2id = Register.toRegID r2
        if r2id <> rBay.StackPointer.Value then
          let st = { st with Target = r2; TableAddr = uint64 imm }
          checkTableAddrNOPIEGuess core st addrs
        else checkTableAddrNOPIEGuess core st addrs
      else checkTableAddrNOPIEGuess core st addrs
    | Opcode.LUI,
      TwoOperands (OpReg r1, OpImm imm) ->
      if st.Target = r1 && st.TableAddr <> 0UL then
        let imm = (uint64 imm) <<< 16
        if verifyTableNOPIE core (st.TableAddr + imm) st.NumEntries st.EntrySize then
          Some (st.TableAddr + imm, st.NumEntries, st.EntrySize, 0UL)
        else None
      elif st.Target = r1 && st.TableAddr = 0UL then None
      else
        checkTableAddrNOPIEGuess core st addrs
    | _ -> checkTableAddrNOPIEGuess core st addrs

let rec checkTableAddrNOPIE core st prevAddr = function
  | [] ->
    let addrs = getAddrs core prevAddr
    followRegAndFindNOPIE core st addrs
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.ADDIU,
      ThreeOperands (OpReg r1, OpReg r2, OpImm imm) ->
      if st.Target = r1 && st.TableAddr = 0UL then
        let rBay = core.Handle.RegisterBay
        let r2id = Register.toRegID r2
        if r2id <> rBay.StackPointer.Value then
          let st = { st with Target = r2; TableAddr = uint64 imm }
          checkTableAddrNOPIE core st addr addrs
        else checkTableAddrNOPIE core st addr addrs
      else checkTableAddrNOPIE core st addr addrs
    | Opcode.LUI,
      TwoOperands (OpReg r1, OpImm imm) ->
      if st.Target = r1 && st.TableAddr <> 0UL then
        let imm = (uint64 imm) <<< 16
        Some (st.TableAddr + imm, st.NumEntries, st.EntrySize, 0UL)
      elif st.Target = r1 && st.TableAddr = 0UL then
        None
      else
        checkTableAddrNOPIE core st addr addrs
    | Opcode.LW,
      TwoOperands (OpReg r1, OpMem (r2, Imm imm, _)) ->
      if st.Target = r1 then
        let addrs = getAddrs core addr
        followMemAndFindNOPIE core st r2 imm addrs
      else checkTableAddrNOPIE core st addr addrs
    | _ -> checkTableAddrNOPIE core st addr addrs

and followMemAndFindNOPIE core st reg off = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.SW,
      TwoOperands (OpReg r1, OpMem (r2, Imm imm, _)) ->
      if reg = r2 && off = imm then
        let guessAddrs =
          let prevAddr = core.PrevInsAddrs[addr]
          let prevIns = core.Instrs[prevAddr]
          if prevIns.IsBBLEnd () then
            collectAddrs core 1 prevAddr [addr]
          else collectAddrs core 0 addr [addr]
        let st = { st with Target = r1 }
        match checkTableAddrNOPIEGuess core st guessAddrs with
        | Some res -> Some res
        | None ->
          followMemAndFindNOPIE core st reg off addrs
      else
        followMemAndFindNOPIE core st reg off addrs
    | _ ->
      followMemAndFindNOPIE core st reg off addrs

and followRegAndFindNOPIE core st = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.LW,
      TwoOperands (OpReg r1, OpMem (r2, Imm imm, _)) ->
      if st.Target = r1 then
        let memAddrs = getAddrs core addr
        match followMemAndFindNOPIE core st r2 imm memAddrs with
        | Some v -> Some v
        | None -> followRegAndFindNOPIE core st addrs
      else followRegAndFindNOPIE core st addrs
    | Opcode.LUI,
      TwoOperands (OpReg r1, OpImm imm) ->
      if st.Target = r1 && st.TableAddr = 0UL then
        None
      elif st.Target = r1 && st.TableAddr <> 0UL then
        let imm = (uint64 imm) <<< 16
        if verifyTableNOPIE core (imm + st.TableAddr) st.NumEntries st.EntrySize then
          Some (imm + st.TableAddr, st.NumEntries, st.EntrySize, 0UL)
        else None
      else followRegAndFindNOPIE core st addrs
    | Opcode.ADDIU,
      ThreeOperands (OpReg r1, OpReg r2, OpImm imm) ->
      if st.Target = r1 && st.TableAddr = 0UL then
        let rBay = core.Handle.RegisterBay
        let r2id = Register.toRegID r2
        if r2id <> rBay.StackPointer.Value then
          let st = { st with Target = r2; TableAddr = uint64 imm }
          followRegAndFindNOPIE core st addrs
        else followRegAndFindNOPIE core st addrs
      else followRegAndFindNOPIE core st addrs
    | _ -> followRegAndFindNOPIE core st addrs

let findTableBaseMIPS32ELFNOPIE core addr (ins: Instruction) =
  let ins = ins :?> MIPSInstruction
  match ins.Info.Operands with
  | OneOperand (OpReg reg) ->
    let addrs = collectAddrs core 1 addr [addr + 4UL; addr]
    let st = { Target = reg; Index = reg; TableAddr = 0UL; EntrySize = 0; NumEntries = 0 }
    match checkNecessaryInstructionsNOPIE core 0 st addrs addrs with
    | Some (st, addrs) ->
      match checkTableLoadNOPIE core st addrs addrs with
      | Some (st, addrs) ->
        match checkAdditionNOPIE core st addrs addrs with
        | Some (st, addrs) ->
          match checkIndexNOPIE core st addrs addrs with
          | Some (st, addrs) ->
            //printfn "Looks like jump table"
            checkTableAddrNOPIE core st addr addrs
          | _ -> None
        | _ -> None
      | _ -> None
    | _ -> None
  | _ -> None

let tryReadGOTBased hdl (addr: Addr) =
  if hdl.FileInfo.IsValidAddr addr then
    match BinHandle.TryReadUInt (hdl, addr, 4) with
    | Ok imm ->
      if imm &&& 0xFFFFUL = 0x0UL then Some imm else None
    | _ -> None
  else None

let verifyTableEntryMIPS32ELFPIE core (cur: Addr) size =
  match BinHandle.TryReadUInt (core.Handle, cur, size) with
  | Ok target -> Some (mask32bit <| target + core.GOT)
  | Error _ -> None

let rec verifyTableEntriesMIPS32ELFPIE core tbl cur tblSize size entries =
  if tbl + uint64 tblSize * uint64 size = cur then Some entries
  else
    match verifyTableEntryMIPS32ELFPIE core cur size with
    | Some target ->
      let entries = Set.add target entries
      let cur = cur + uint64 size
      verifyTableEntriesMIPS32ELFPIE core tbl cur tblSize size entries
    | None -> None

let readTableEntryMIPS32ELFPIE core (cur: Addr) size =
  BinHandle.ReadUInt (core.Handle, cur, size) + core.GOT
  |> mask32bit

let rec readTableEntriesMIPS32ELFPIE core tbl cur tblSize size entries =
  if tbl + uint64 tblSize * uint64 size = cur then entries
  else
    let target = readTableEntryMIPS32ELFPIE core cur size
    let entries = Set.add target entries
    let cur = cur + uint64 size
    readTableEntriesMIPS32ELFPIE core tbl cur tblSize size entries

let verifyTablePIE core tbl tblSize size =
  if core.Handle.FileInfo.IsValidAddr (mask32bit tbl) then
    match verifyTableEntriesMIPS32ELFPIE core tbl tbl tblSize size Set.empty with
    | Some entries ->
      Set.forall (fun a -> core.Instrs.ContainsKey a) entries
    | None -> false
  else false

let rec checkNecessaryInstructionsPIE core cur st newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.JR, OneOperand (OpReg reg) ->
      if cur = 0 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        checkNecessaryInstructionsPIE core 1 st newAddrs addrs
      else None
    | Opcode.ADDU, ThreeOperands (OpReg r1, OpReg r2, OpReg _) ->
      if cur = 1 then
        if st.Target = r1 then
          let newAddrs = List.filter (fun a -> a <> addr) newAddrs
          let st = { st with Target = r2 }
          checkNecessaryInstructionsPIE core 2 st newAddrs addrs
        else checkNecessaryInstructionsPIE core cur st newAddrs addrs
      else checkNecessaryInstructionsPIE core cur st newAddrs addrs
    | Opcode.BEQ, _ ->
      if cur = 2 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        checkNecessaryInstructionsPIE core 3 st newAddrs addrs
      else None
    | Opcode.SLTIU, ThreeOperands (OpReg _, OpReg _, OpImm imm) ->
      if cur = 3 then
        if imm < 0x1000UL then
          let newAddrs = List.filter (fun a -> a <> addr) newAddrs
          let st = { st with NumEntries = int imm }
          Some (st, newAddrs)
        else checkNecessaryInstructionsPIE core cur st newAddrs addrs
      else None
    | _ -> checkNecessaryInstructionsPIE core cur st newAddrs addrs

let rec checkTableLoadPIE core st newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.LW,
      TwoOperands (OpReg r1, OpMem (r2, Imm imm, _)) ->
      if st.Target = r1 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        let st = { st with Target = r2; TableAddr = uint64 imm; EntrySize = 4 }
        Some (st, newAddrs)
      else checkTableLoadPIE core st newAddrs addrs
    | _ -> checkTableLoadPIE core st newAddrs addrs

let rec checkAdditionPIE core st newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.ADDU,
      ThreeOperands (OpReg r1, OpReg r2, OpReg r3) ->
      if st.Target = r1 then
        let st = { st with Target = r2; Index = r3 }
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        Some (st, newAddrs)
      else checkAdditionPIE core st newAddrs addrs
    | _ -> checkAdditionPIE core st newAddrs addrs

let rec checkIndexPIE core st newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.SLL,
      ThreeOperands (OpReg r1, OpReg _, OpShiftAmount _) ->
      if st.Target = r1 then
        let st = { st with Target = st.Index; Index = st.Target }
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        Some (st, newAddrs)
      elif st.Index = r1 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        Some (st, newAddrs)
      else checkIndexPIE core st newAddrs addrs
    | _ -> checkIndexPIE core st newAddrs addrs

let rec checkTableAddrPIEGuess core st = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.ADDIU,
      ThreeOperands (OpReg r1, OpReg r2, OpImm imm) ->
      if st.Target = r1 && st.TableAddr = 0UL then
        let rBay = core.Handle.RegisterBay
        let r2id = Register.toRegID r2
        if r2id <> rBay.StackPointer.Value then
          let st = { st with Target = r2; TableAddr = uint64 imm }
          checkTableAddrPIEGuess core st addrs
        else checkTableAddrPIEGuess core st addrs
      else checkTableAddrPIEGuess core st addrs
    | Opcode.LW,
      TwoOperands (OpReg r1, OpMem (r2, Imm imm, _)) ->
      if st.Target = r1 && st.TableAddr <> 0UL then
        match tryReadGOTBased core.Handle (core.GOT + uint64 imm) with
        | Some value ->
          if verifyTablePIE core (value + st.TableAddr) st.NumEntries st.EntrySize then
            Some (value + st.TableAddr, st.NumEntries, st.EntrySize, 0UL)
          else None
        | _ -> None
      elif st.Target = r1 && st.TableAddr = 0UL then None
      else
        checkTableAddrPIEGuess core st addrs
    | _ -> checkTableAddrPIEGuess core st addrs

let rec checkTableAddrPIE core st prevAddr = function
  | [] ->
    let addrs = getAddrs core prevAddr
    followRegAndFindPIE core st addrs
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.ADDIU,
      ThreeOperands (OpReg r1, OpReg r2, OpImm imm) ->
      if st.Target = r1 && st.TableAddr = 0UL then
        let rBay = core.Handle.RegisterBay
        let r2id = Register.toRegID r2
        if r2id <> rBay.StackPointer.Value then
          let st = { st with Target = r2; TableAddr = uint64 imm }
          checkTableAddrPIE core st addr addrs
        else checkTableAddrPIE core st addr addrs
      else checkTableAddrPIE core st addr addrs
    | Opcode.LW,
      TwoOperands (OpReg r1, OpMem (r2, Imm imm, _)) ->
      if st.Target = r1 && st.TableAddr <> 0UL then
        match tryReadGOTBased core.Handle (core.GOT + uint64 imm) with
        | Some value ->
          Some (value + st.TableAddr, st.NumEntries, st.EntrySize, 0UL)
        | _ ->
          let addrs = getAddrs core addr
          followMemAndFindPIE core st r2 imm addrs
      elif st.Target = r1 && st.TableAddr = 0UL then
        let addrs = getAddrs core addr
        followMemAndFindPIE core st r2 imm addrs
      else
        checkTableAddrPIE core st addr addrs
    | _ -> checkTableAddrPIE core st addr addrs

and followMemAndFindPIE core st reg off = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.SW,
      TwoOperands (OpReg r1, OpMem (r2, Imm imm, _)) ->
      if reg = r2 && off = imm then
        let guessAddrs =
          let prevAddr = core.PrevInsAddrs[addr]
          let prevIns = core.Instrs[prevAddr]
          if prevIns.IsBBLEnd () then
            collectAddrs core 1 prevAddr [addr]
          else collectAddrs core 0 addr [addr]
        let st = { st with Target = r1 }
        match checkTableAddrPIEGuess core st guessAddrs with
        | Some res -> Some res
        | None ->
          followMemAndFindPIE core st reg off addrs
      else
        followMemAndFindPIE core st reg off addrs
    | _ ->
      followMemAndFindPIE core st reg off addrs

and followRegAndFindPIE core st = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.LW,
      TwoOperands (OpReg r1, OpMem (r2, Imm imm, _)) ->
      if st.Target = r1 && st.TableAddr = 0UL then
        let memAddrs = getAddrs core addr
        match followMemAndFindPIE core st r2 imm memAddrs with
        | Some v -> Some v
        | None -> followRegAndFindPIE core st addrs
      elif st.Target = r1 && st.TableAddr <> 0UL then
        match tryReadGOTBased core.Handle (core.GOT + uint64 imm) with
        | Some value ->
          if verifyTablePIE core (value + st.TableAddr) st.NumEntries st.EntrySize then
            Some (value + st.TableAddr, st.NumEntries, st.EntrySize, 0UL)
          else None
        | _ ->
          None
      else followRegAndFindPIE core st addrs
    | Opcode.ADDIU,
      ThreeOperands (OpReg r1, OpReg r2, OpImm imm) ->
      if st.Target = r1 && st.TableAddr = 0UL then
        let rBay = core.Handle.RegisterBay
        let r2id = Register.toRegID r2
        if r2id <> rBay.StackPointer.Value then
          let st = { st with Target = r2; TableAddr = uint64 imm }
          followRegAndFindPIE core st addrs
        else followRegAndFindPIE core st addrs
      else followRegAndFindPIE core st addrs
    | _ -> followRegAndFindPIE core st addrs

let findTableBaseMIPS32ELFPIE core addr (ins: Instruction) =
  let ins = ins :?> MIPSInstruction
  match ins.Info.Operands with
  | OneOperand (OpReg reg) ->
    let addrs = collectAddrs core 1 addr [addr + 4UL; addr]
    let st = { Target = reg; Index = reg; TableAddr = 0UL; EntrySize = 0; NumEntries = 0 }
    match checkNecessaryInstructionsPIE core 0 st addrs addrs with
    | Some (st, addrs) ->
      match checkTableLoadPIE core st addrs addrs with
      | Some (st, addrs) ->
        match checkAdditionPIE core st addrs addrs with
        | Some (st, addrs) ->
          match checkIndexPIE core st addrs addrs with
          | Some (st, addrs) ->
            //printfn "Looks like jump table"
            checkTableAddrPIE core st addr addrs
          | _ -> None
        | _ -> None
      | _ -> None
    | _ -> None
  | _ -> None
