module FunProbe.DataSeparator

open B2R2
open B2R2.FrontEnd.BinLifter.ARM32
open B2R2.FrontEnd.BinInterface

open System.Collections.Generic

let rec findAddrUsage core addr reg =
  if core.Instrs.ContainsKey addr |> not then 1
  else
    let ins = core.Instrs[addr] :?> ARM32Instruction
    if ins.IsBBLEnd () then
      1
    else
      match ins.Opcode, ins.Operands with
      | Opcode.LDR,
        TwoOperands (OprReg _, OprMemory (OffsetMode (ImmOffset (r, _, _)))) when reg = r ->
        1
      | Opcode.LDRD,
        ThreeOperands (OprReg _, OprReg _, OprMemory (OffsetMode (ImmOffset (r, _, _)))) when reg = r ->
        2
      | Opcode.VLD1,
        TwoOperands (OprSIMD _, OprMemory (OffsetMode (AlignOffset (r, None, _)))) when reg = r ->
        4
      | Opcode.VLD1,
        TwoOperands (OprSIMD _, OprMemory (OffsetMode (AlignOffset (r, Some size, _)))) when reg = r ->
        int (size / 32L)
      | _ -> findAddrUsage core (addr + 4UL) reg

let toReg = function
  | Vector r -> r
  | Scalar (r, _) -> r

let getSIMDSize = function
  | SFReg r ->
    let bw = toReg r |> Register.toRegType |> RegType.toByteWidth
    bw / 4
  | OneReg r ->
    let bw = toReg r |> Register.toRegType |> RegType.toByteWidth
    bw / 4
  | TwoRegs (r1, r2) ->
    let bw1 = toReg r1 |> Register.toRegType |> RegType.toByteWidth
    let bw2 = toReg r2 |> Register.toRegType |> RegType.toByteWidth
    (bw1 + bw2) / 4
  | ThreeRegs (r1, r2, r3) ->
    let bw1 = toReg r1 |> Register.toRegType |> RegType.toByteWidth
    let bw2 = toReg r2 |> Register.toRegType |> RegType.toByteWidth
    let bw3 = toReg r3 |> Register.toRegType |> RegType.toByteWidth
    (bw1 + bw2 + bw3) / 4
  | FourRegs (r1, r2, r3, r4) ->
    let bw1 = toReg r1 |> Register.toRegType |> RegType.toByteWidth
    let bw2 = toReg r2 |> Register.toRegType |> RegType.toByteWidth
    let bw3 = toReg r3 |> Register.toRegType |> RegType.toByteWidth
    let bw4 = toReg r4 |> Register.toRegType |> RegType.toByteWidth
    (bw1 + bw2 + bw3 + bw4) / 4

let findConstDataInCode core (toRemove: HashSet<Addr>) sAddr eAddr =
  let rBay = core.Handle.RegisterBay
  core.Instrs
  |> Seq.iter (fun (KeyValue (addr, ins)) ->
    if toRemove.Contains addr |> not then
      let ins = ins :?> ARM32Instruction
      match ins.Opcode, ins.Operands with
      (* Constant Load Pattern *)
      | Opcode.LDR,
        TwoOperands (OprReg r, OprMemory (LiteralMode imm)) ->
        let dataAddr = addr + 8UL + uint64 imm
        if sAddr <= dataAddr && dataAddr < eAddr then
          core.DataReferences.Add dataAddr |> ignore
          toRemove.Add dataAddr |> ignore
        else ()
      | Opcode.VLDR,
        TwoOperands (OprSIMD s, OprMemory (OffsetMode (ImmOffset (Register.PC, _, Some imm)))) ->
        let dataAddr = addr + 8UL + uint64 imm
        if sAddr <= dataAddr && dataAddr < eAddr then
          core.DataReferences.Add dataAddr |> ignore
          let size = getSIMDSize s
          [ dataAddr .. 4UL .. dataAddr + (uint64 <| 4 * (size - 1)) ]
          |> List.iter (fun data -> toRemove.Add data |> ignore)
        else ()
      (* Address Computation Pattern *)
      | Opcode.ADD,
        ThreeOperands (OprReg rt, OprReg r, OprImm imm) ->
        if rBay.ProgramCounter = Register.toRegID r then
          let dataAddr = addr + 8UL + uint64 imm
          if sAddr <= dataAddr && dataAddr < eAddr then
            core.DataReferences.Add dataAddr |> ignore
            let removeSize = findAddrUsage core (addr + 4UL) rt
            //printfn "%x %x %x" addr dataAddr removeSize
            [ dataAddr .. 4UL .. dataAddr + (uint64 <| 4 * (removeSize - 1)) ]
            |> List.iter (fun data -> toRemove.Add data |> ignore)
          else ()
        else ()
      | Opcode.SUB,
        ThreeOperands (OprReg rt, OprReg r, OprImm imm) ->
        if rBay.ProgramCounter = Register.toRegID r then
          let dataAddr = addr + 8UL - uint64 imm
          if sAddr <= dataAddr && dataAddr < eAddr then
            core.DataReferences.Add dataAddr |> ignore
            let removeSize = findAddrUsage core (addr + 4UL) rt
            //printfn "%x %x %x" addr dataAddr removeSize
            [ dataAddr .. 4UL .. dataAddr + (uint64 <| 4 * (removeSize - 1)) ]
            |> List.iter (fun data -> toRemove.Add data |> ignore)
          else ()
        else ()
      | _ -> ()
    else ()
    )

let rec findTableBase core (referenced: HashSet<Addr>) addr =
  if core.Instrs.ContainsKey addr |> not then None
  else
    let ins = core.Instrs[addr] :?> ARM32Instruction
    match ins.Opcode, ins.Operands with
    | Opcode.ADD,
      ThreeOperands (OprReg _, OprReg Register.PC, OprImm imm) ->
      Some (addr + 8UL + uint64 imm)
    | _ ->
      if referenced.Contains addr then None
      else findTableBase core referenced (addr - 4UL)

let computeJumpReferences core sAddr eAddr =
  let referenced = HashSet<Addr> ()
  core.Instrs
  |> Seq.iter (fun (KeyValue (addr, ins)) ->
    match ins.DirectBranchTarget () with
    | true, v when sAddr <= v && v < eAddr ->
      referenced.Add v |> ignore
    | _ -> ())
  referenced

let rec recoverRanges core tblAddr (cur: Addr) referenced recovered =
  if Set.contains cur recovered then cur
  elif (referenced: HashSet<Addr>).Contains cur then cur
  elif core.References.ContainsKey cur then cur
  else
    let v = BinHandle.ReadUInt (core.Handle, cur, 4)
    if v % 4UL <> 0UL then cur
    else
      let target =
        if core.Handle.FileInfo.IsRelocatable then (tblAddr + v) &&& 0xFFFFFFFFUL
        else v
      let recovered = Set.add target recovered
      referenced.Add target |> ignore
      recoverRanges core tblAddr (cur + 4UL) referenced recovered

let findJumpTableInCode core referenced (toRemove: HashSet<Addr>) sAddr eAddr =
  let rBay = core.Handle.RegisterBay
  core.Instrs
  |> Seq.iter (fun (KeyValue (addr, ins)) ->
    let ins = ins :?> ARM32Instruction
    match ins.Opcode, ins.Operands with
    | Opcode.ADD,
      FourOperands (OprReg r1, OprReg r2, OprReg _, OprShift _) ->
      if rBay.ProgramCounter = Register.toRegID r1 then
        if rBay.ProgramCounter = Register.toRegID r2 then
          if toRemove.Contains (addr + 4UL) then
            let tbl = addr + 4UL
            //printfn "%x %A" addr ins.Operands
            core.DataReferences.Add tbl |> ignore
            let last = recoverRanges core tbl tbl referenced Set.empty
            [ tbl .. 4UL .. last - 4UL ]
            |> List.iter (fun entry -> toRemove.Add entry |> ignore)
            //printfn "%x -- %x" tbl last
        else
          match findTableBase core referenced (addr - 4UL) with
          | None -> ()
          | Some tblAddr ->
            let last = recoverRanges core tblAddr tblAddr referenced Set.empty
            core.DataReferences.Add tblAddr |> ignore
            [ tblAddr .. 4UL .. last - 4UL ]
            |> List.iter (fun entry -> toRemove.Add entry |> ignore)
            //printfn "%x -- %x" tblAddr last
      else ()
    | Opcode.LDR,
      TwoOperands (OprReg Register.PC, OprMemory (OffsetMode (RegOffset (r, _, _, _)))) ->
      if rBay.ProgramCounter = Register.toRegID r then
        let tbl = addr + 8UL
        core.DataReferences.Add tbl |> ignore
        let last = recoverRanges core tbl tbl referenced Set.empty
        [ tbl .. 4UL .. last - 4UL ]
        |> List.iter (fun entry -> toRemove.Add entry |> ignore)
        //printfn "%x -- %x" tbl last
      else
        let tbl = addr + 4UL
        core.DataReferences.Add tbl |> ignore
        let last = recoverRanges core tbl tbl referenced Set.empty
        [ tbl .. 4UL .. last - 4UL ]
        |> List.iter (fun entry -> toRemove.Add entry |> ignore)
        //printfn "%x -- %x" tbl last
    | _ -> ()
    )

let rec findNextAddr core eAddr addr =
  if addr = eAddr then None
  elif core.Instrs.ContainsKey addr then Some addr
  else findNextAddr core eAddr (addr + 4UL)

let removeInstruction core (toRemove: HashSet<Addr>) eAddr =
  toRemove
  |> Seq.iter (fun addr ->
    if core.Instrs.ContainsKey addr then
      match findNextAddr core eAddr (addr + 4UL) with
      | Some nextAddr ->
        let prevAddr = core.PrevInsAddrs[addr]
        if core.PrevInsAddrs[nextAddr] <> addr then Utils.impossible ()
        core.PrevInsAddrs[nextAddr] <- prevAddr
      | None -> ()
      core.Instrs.Remove addr |> ignore
      core.PrevInsAddrs.Remove addr |> ignore
      core.Stmts.Remove addr |> ignore
    else ()
    )

let removeDataInCode core sAddr eAddr =
  let toRemove = HashSet<Addr> ()
  findConstDataInCode core toRemove sAddr eAddr
  let referenced = computeJumpReferences core sAddr eAddr
  findJumpTableInCode core referenced toRemove sAddr eAddr
  removeInstruction core toRemove eAddr

let run core =
  match core.Handle.ISA.Arch with
  | Arch. ARMv7 ->
    core.Handle.FileInfo.GetTextSections ()
    |> Seq.iter (fun sec ->
      let sAddr = sec.Address
      let eAddr = sAddr + sec.Size
      removeDataInCode core sAddr eAddr)
  | _ -> ()