module FunProbe.Disassembler

open B2R2
open B2R2.FrontEnd.BinInterface

open System.Collections.Generic

let rec linearSweep core prevAddr addr eAddr =
  let hdl = core.Handle
  if addr >= eAddr then ()
  else
    let addr, prevAddr =
      match BinHandle.TryParseInstr (hdl, addr=addr) with
      | Ok ins ->
        core.Instrs[addr] <- ins
        core.PrevInsAddrs[addr] <- prevAddr
        try
          let stmts = BinHandle.LiftInstr hdl ins
          core.Stmts[addr] <- stmts
        with
          | _ -> core.Stmts[addr] <- [||]
        addr + uint64 ins.Length, addr
      | Error _ ->
        match hdl.ISA.Arch with
        | Arch.IntelX86
        | Arch.IntelX64 -> addr + 1UL, prevAddr
        | _ -> addr + 4UL, prevAddr
    linearSweep core prevAddr addr eAddr

let rec findNextAddr core eAddr addr =
  if addr = eAddr then None
  elif core.Instrs.ContainsKey addr then Some addr
  else findNextAddr core eAddr (addr + 4UL)

let rec recoverRanges hdl tblAddr (cur: Addr) referenced recovered =
  if Set.contains cur recovered then cur
  elif (referenced: HashSet<Addr>).Contains cur then cur
  else
    let v = BinHandle.ReadUInt (hdl, cur, 4)
    if v % 4UL <> 0UL then cur
    else
      let target = (tblAddr + v) &&& 0xFFFFFFFFUL
      let recovered = Set.add target recovered
      recoverRanges hdl tblAddr (cur + 4UL) referenced recovered

let run core =
  core.Handle.FileInfo.GetTextSections ()
  |> Seq.iter (fun sec ->
    let sAddr = sec.Address
    let eAddr = sAddr + sec.Size
    linearSweep core 0UL sAddr eAddr)
  core.Handle.FileInfo.GetSections ".init"
  |> Seq.iter (fun sec ->
    let sAddr = sec.Address
    let eAddr = sAddr + sec.Size
    linearSweep core 0UL sAddr eAddr)
  core.Handle.FileInfo.GetSections ".fini"
  |> Seq.iter (fun sec ->
    let sAddr = sec.Address
    let eAddr = sAddr + sec.Size
    linearSweep core 0UL sAddr eAddr)