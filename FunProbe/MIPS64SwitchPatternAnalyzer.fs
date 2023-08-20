module FunProbe.MIPS64SwitchPatternAnalyzer

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

let rec collectAddrs core nBlks addr addrs =
  let prevAddr = core.PrevInsAddrs[addr]
  if prevAddr = 0UL then List.rev addrs
  else
    let pprevAddr = core.PrevInsAddrs[prevAddr]
    if pprevAddr = 0UL then List.rev (prevAddr :: addrs)
    else
      let pprevIns = core.Instrs[pprevAddr]
      if pprevIns.IsBBLEnd () then
        if nBlks = 0 then List.rev (prevAddr :: addrs)
        else
          if pprevIns.IsCondBranch () then
            collectAddrs core (nBlks - 1) pprevAddr (prevAddr :: pprevAddr :: addrs)
          else
            if core.References.ContainsKey addr && core.References[addr].Count > 0 then
              let nAddr = core.References[addr][0]
              collectAddrs core (nBlks - 1) nAddr (nAddr + 4UL :: nAddr :: addrs)
            else List.rev (prevAddr :: addrs)
      elif NopOracle.isNop core.Handle pprevIns then
        if nBlks = 0 then List.rev (prevAddr :: addrs)
        else
          if core.References.ContainsKey prevAddr && core.References[prevAddr].Count > 0 then
            let nAddr = core.References[prevAddr][0]
            collectAddrs core (nBlks - 1) nAddr (prevAddr :: addrs)
          else
            collectAddrs core nBlks prevAddr (prevAddr :: addrs)
      else
        collectAddrs core nBlks prevAddr (prevAddr :: addrs)

(*
let rec collectAddrs core nBlks addr addrs =
  let prevAddr = core.PrevInsAddrs[addr]
  if prevAddr = 0UL then List.rev addrs
  else
    let prevIns = core.Instrs[prevAddr]
    if prevIns.IsBBLEnd () then
      if nBlks = 0 then
        let addrs = List.tail addrs
        if core.Instrs[List.head addrs].IsNop () then
          List.tail addrs |> List.rev
        else addrs |> List.rev
      else
        if prevIns.IsCondBranch () then
          let nextAddr = prevAddr + uint64 prevIns.Length
          collectAddrs core (nBlks - 1) prevAddr (nextAddr :: prevAddr :: List.tail addrs)
        else
          if core.References.ContainsKey addr && core.References[addr].Count > 0 then
            let prevAddr = core.References[addr][0]
            let nextAddr = prevAddr + uint64 prevIns.Length
            collectAddrs core (nBlks - 1) prevAddr (nextAddr :: prevAddr :: List.tail addrs)
          else addrs
    elif NopOracle.isNop core.Handle prevIns then
      if nBlks = 0 then
        let addrs = List.tail addrs
        if core.Instrs[List.head addrs].IsNop () then
          List.tail addrs |> List.rev
        else addrs |> List.rev
      else
        if core.References.ContainsKey addr && core.References[addr].Count > 0 then
          let prevAddr = core.References[addr][0]
          let nextAddr = prevAddr + uint64 prevIns.Length
          collectAddrs core (nBlks - 1) prevAddr (nextAddr :: prevAddr :: List.tail addrs)
        else
          collectAddrs core nBlks prevAddr (prevAddr :: addrs)
    else collectAddrs core nBlks prevAddr (prevAddr :: addrs)
*)

let tryReadGOTBased hdl (addr: Addr) =
  if hdl.FileInfo.IsValidAddr addr then
    match BinHandle.TryReadUInt (hdl, addr, 8) with
    | Ok imm ->
      if imm &&& 0xFFFFUL = 0x0UL then Some imm else None
    | _ -> None
  else None

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

let verifyTableEntryMIPS64ELF core (cur: Addr) size =
  match BinHandle.TryReadUInt (core.Handle, cur, size) with
  | Ok target -> Some (target + core.GOT)
  | Error _ -> None

let rec verifyTableEntriesMIPS64ELF core tbl cur tblSize size entries =
  if tbl + uint64 tblSize * uint64 size = cur then Some entries
  else
    match verifyTableEntryMIPS64ELF core cur size with
    | Some target ->
      let entries = Set.add target entries
      let cur = cur + uint64 size
      verifyTableEntriesMIPS64ELF core tbl cur tblSize size entries
    | None -> None

let readTableEntryMIPS64ELF core (cur: Addr) size =
  BinHandle.ReadUInt (core.Handle, cur, size) + core.GOT

let rec readTableEntriesMIPS64ELF core tbl cur tblSize size entries =
  if tbl + uint64 tblSize * uint64 size = cur then entries
  else
    let target = readTableEntryMIPS64ELF core cur size
    let entries = Set.add target entries
    let cur = cur + uint64 size
    readTableEntriesMIPS64ELF core tbl cur tblSize size entries

let verifyTable core tbl tblSize size =
  if core.Handle.FileInfo.IsValidAddr tbl then
    match verifyTableEntriesMIPS64ELF core tbl tbl tblSize size Set.empty with
    | Some entries ->
      Set.forall (fun a -> core.Instrs.ContainsKey a) entries
    | _ -> false
  else false

let rec checkNecessaryInstructions core cur st newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.JR, OneOperand (OpReg reg) ->
      if cur = 0 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        checkNecessaryInstructions core 1 st newAddrs addrs
      else None
    | Opcode.DADDU, ThreeOperands (OpReg r1, OpReg r2, OpReg _) ->
      if cur = 1 then
        if st.Target = r1 then
          let newAddrs = List.filter (fun a -> a <> addr) newAddrs
          let st = { st with Target = r2 }
          checkNecessaryInstructions core 2 st newAddrs addrs
        else checkNecessaryInstructions core cur st newAddrs addrs
      else checkNecessaryInstructions core cur st newAddrs addrs
    | Opcode.BEQ, _ ->
      if cur = 2 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        checkNecessaryInstructions core 3 st newAddrs addrs
      else checkNecessaryInstructions core cur st newAddrs addrs
    | Opcode.SLTIU, ThreeOperands (OpReg _, OpReg _, OpImm imm) ->
      if cur = 3 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        let st = { st with NumEntries = int imm }
        Some (st, newAddrs)
      else None
    | _ -> checkNecessaryInstructions core cur st newAddrs addrs

let rec checkTableLoad core st newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.LD,
      TwoOperands (OpReg r1, OpMem (r2, Imm imm, _)) ->
      if st.Target = r1 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        let st = { st with Target = r2; TableAddr = uint64 imm; EntrySize = 8 }
        Some (st, newAddrs)
      else checkTableLoad core st newAddrs addrs
    | _ -> checkTableLoad core st newAddrs addrs

let rec checkAddition core st newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.DADDU,
      ThreeOperands (OpReg r1, OpReg r2, OpReg r3) ->
      if st.Target = r1 then
        let st = { st with Target = r2; Index = r3 }
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        Some (st, newAddrs)
      else checkAddition core st newAddrs addrs
    | _ -> checkAddition core st newAddrs addrs

let rec checkIndex core st newAddrs = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.DSLL,
      ThreeOperands (OpReg r1, OpReg _, OpShiftAmount _) ->
      if st.Target = r1 then
        let st = { st with Target = st.Index; Index = st.Target }
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        Some (st, newAddrs)
      elif st.Index = r1 then
        let newAddrs = List.filter (fun a -> a <> addr) newAddrs
        Some (st, newAddrs)
      else checkIndex core st newAddrs addrs
    | _ -> checkIndex core st newAddrs addrs

let rec checkTableAddrGuess core st = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.DADDIU,
      ThreeOperands (OpReg r1, OpReg r2, OpImm imm) ->
      if st.Target = r1 && st.TableAddr = 0UL then
        let rBay = core.Handle.RegisterBay
        let r2id = Register.toRegID r2
        if r2id <> rBay.StackPointer.Value then
          let st = { st with Target = r2; TableAddr = uint64 imm }
          checkTableAddrGuess core st addrs
        else checkTableAddrGuess core st addrs
      else checkTableAddrGuess core st addrs
    | Opcode.LD,
      TwoOperands (OpReg r1, OpMem (r2, Imm imm, _)) ->
      if st.Target = r1 && st.TableAddr <> 0UL then
        match tryReadGOTBased core.Handle (core.GOT + uint64 imm) with
        | Some value ->
          if verifyTable core (value + st.TableAddr) st.NumEntries st.EntrySize then
            Some (value + st.TableAddr, st.NumEntries, st.EntrySize, 0UL)
          else None
        | _ -> None
      elif st.Target = r1 && st.TableAddr = 0UL then None
      else checkTableAddrGuess core st addrs
    | _ -> checkTableAddrGuess core st addrs

let rec checkTableAddr core st prevAddr = function
  | [] ->
    let addrs = getAddrs core prevAddr
    followRegAndFind core st addrs
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.DADDIU,
      ThreeOperands (OpReg r1, OpReg r2, OpImm imm) ->
      if st.Target = r1 && st.TableAddr = 0UL then
        let rBay = core.Handle.RegisterBay
        let r2id = Register.toRegID r2
        if r2id <> rBay.StackPointer.Value then
          let st = { st with Target = r2; TableAddr = uint64 imm }
          checkTableAddr core st addr addrs
        else checkTableAddr core st addr addrs
      else checkTableAddr core st addr addrs
    | Opcode.LD,
      TwoOperands (OpReg r1, OpMem (r2, Imm imm, _)) ->
      if st.Target = r1 && st.TableAddr <> 0UL then
        match tryReadGOTBased core.Handle (core.GOT + uint64 imm) with
        | Some value ->
          Some (value + st.TableAddr, st.NumEntries, st.EntrySize, 0UL)
        | _ ->
          let addrs = getAddrs core addr
          followMemAndFind core st r2 imm addrs
      elif st.Target = r1 && st.TableAddr = 0UL then
        let addrs = getAddrs core addr
        followMemAndFind core st r2 imm addrs
      else checkTableAddr core st addr addrs
    | _ -> checkTableAddr core st addr addrs

and followMemAndFind core st reg off = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.SD,
      TwoOperands (OpReg r1, OpMem (r2, Imm imm, _)) ->
      if reg = r2 && off = imm then
        let guessAddrs =
          let prevAddr = core.PrevInsAddrs[addr]
          let prevIns = core.Instrs[prevAddr]
          if prevIns.IsBBLEnd () then
            collectAddrs core 1 prevAddr [addr]
          else collectAddrs core 1 addr [addr]
        let st = { st with Target = r1 }
        match checkTableAddrGuess core st guessAddrs with
        | Some res -> Some res
        | None ->
          followMemAndFind core st reg off addrs
      else
        followMemAndFind core st reg off addrs
    | _ ->
      followMemAndFind core st reg off addrs

and followRegAndFind core st = function
  | [] -> None
  | addr :: addrs ->
    let ins = core.Instrs[addr] :?> MIPSInstruction
    match ins.Info.Opcode, ins.Info.Operands with
    | Opcode.DADDIU,
      ThreeOperands (OpReg r1, OpReg r2, OpImm imm) ->
      if st.Target = r1 && st.TableAddr = 0UL then
        let rBay = core.Handle.RegisterBay
        let r2id = Register.toRegID r2
        if r2id <> rBay.StackPointer.Value then
          let st = { st with Target = r2; TableAddr = uint64 imm }
          followRegAndFind core st addrs
        else followRegAndFind core st addrs
      else followRegAndFind core st addrs
    | Opcode.LD,
      TwoOperands (OpReg r1, OpMem (r2, Imm imm, _)) ->
      if st.Target = r1 && st.TableAddr = 0UL then
        let memAddrs = getAddrs core addr
        match followMemAndFind core st r2 imm memAddrs with
        | Some v -> Some v
        | None -> followRegAndFind core st addrs
      elif st.Target = r1 && st.TableAddr <> 0UL then
        match tryReadGOTBased core.Handle (core.GOT + uint64 imm) with
        | Some value ->
          if verifyTable core (value + st.TableAddr) st.NumEntries st.EntrySize then
            Some (value + st.TableAddr, st.NumEntries, st.EntrySize, 0UL)
          else None
        | _ -> None
      else followRegAndFind core st addrs
    | _ -> followRegAndFind core st addrs

let findTableBaseMIPS64ELF core addr (ins: Instruction) =
  let ins = ins :?> MIPSInstruction
  match ins.Info.Operands with
  | OneOperand (OpReg reg) ->
    let addrs = collectAddrs core 2 addr [addr + 4UL; addr]
    let st = { Target = reg; Index = reg; TableAddr = 0UL; EntrySize = 0; NumEntries = 0 }
    match checkNecessaryInstructions core 0 st addrs addrs with
    | Some (st, addrs) ->
      match checkTableLoad core st addrs addrs with
      | Some (st, addrs) ->
        match checkAddition core st addrs addrs with
        | Some (st, addrs) ->
          match checkIndex core st addrs addrs with
          | Some (st, addrs) ->
            //printfn "Looks like jump table"
            checkTableAddr core st addr addrs
          | _ -> None
        | _ -> None
      | _ -> None
    | _ -> None
  | _ -> None
