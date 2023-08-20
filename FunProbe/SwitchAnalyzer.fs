namespace FunProbe

open B2R2
open B2R2.FrontEnd.BinInterface

type TableAccessPattern =
  | TableAccess of Addr
  | NotTableAccess
  | UnknownAccess

module SwitchAnalyzer =

  let findTableBase core addr ins =
    let fi = core.Handle.FileInfo
    match fi.ISA.Arch, fi.FileFormat, fi.IsRelocatable with
    | Arch.IntelX86, FileFormat.ELFBinary, false ->
      X86SwitchPatternAnalyzer.findTableBaseX86ELFNOPIE core addr ins
    | Arch.IntelX86, FileFormat.ELFBinary, true ->
      X86SwitchPatternAnalyzer.findTableBaseX86ELFPIE core addr ins
    | Arch.IntelX64, FileFormat.ELFBinary, false ->
      X64SwitchPatternAnalyzer.findTableBaseX64ELFNOPIE core addr ins
    | Arch.IntelX64, FileFormat.ELFBinary, true ->
      X64SwitchPatternAnalyzer.findTableBaseX64ELFPIE core addr ins
    | Arch.ARMv7, FileFormat.ELFBinary, false ->
      ARM32SwitchPatternAnalyzer.findTableBaseARMELFNOPIE core addr ins
    | Arch.ARMv7, FileFormat.ELFBinary, true ->
      ARM32SwitchPatternAnalyzer.findTableBaseARMELFPIE core addr ins
    | Arch.AARCH64, FileFormat.ELFBinary, _ ->
      ARM64SwitchPatternAnalyzer.findTableBaseAArch64ELF core addr ins
    | Arch.MIPS32, FileFormat.ELFBinary, false ->
      MIPS32SwitchPatternAnalyzer.findTableBaseMIPS32ELFNOPIE core addr ins
    | Arch.MIPS32, FileFormat.ELFBinary, true ->
      MIPS32SwitchPatternAnalyzer.findTableBaseMIPS32ELFPIE core addr ins
    | Arch.MIPS64, FileFormat.ELFBinary, _ ->
      MIPS64SwitchPatternAnalyzer.findTableBaseMIPS64ELF core addr ins
    | _ -> None

  let maskAddr core addr =
    match core.Handle.ISA.WordSize with
    | WordSize.Bit32 -> 0xFFFFFFFFUL &&& addr
    | _ -> addr

  let recoverTableBase core =
    core.Instrs
    |> Seq.iter (fun (KeyValue (addr, ins)) ->
      if ins.IsIndirectBranch () && not <| ins.IsCall () && not <| ins.IsRET () then
        //printfn "indjmp %x %A" addr <| ins.Disasm ()
        match findTableBase core addr ins with
        | Some (baseAddr, tblSize, size, refAddr) ->
          let baseAddr = maskAddr core baseAddr
          let refAddr = maskAddr core refAddr
          if core.Handle.FileInfo.IsValidAddr baseAddr then
            //printfn "Resolved Jump Table: %x => (%x)%x (%d)" addr refAddr baseAddr tblSize
            core.ResolvedJmpTbls[baseAddr] <- addr, tblSize, size, refAddr
          else ()
        | None -> ()
      else ())

  let readTableEntries core tbl tblSize size refAddr =
    try
      let fi = core.Handle.FileInfo
      match fi.ISA.Arch, fi.FileFormat, fi.IsRelocatable with
      | Arch.IntelX86, FileFormat.ELFBinary, false ->
        X86SwitchPatternAnalyzer.readTableEntriesX86ELFNOPIE core tbl tbl tblSize size Set.empty
      | Arch.IntelX86, FileFormat.ELFBinary, true ->
        X86SwitchPatternAnalyzer.readTableEntriesX86ELFPIE core tbl tbl tblSize size Set.empty
      | Arch.IntelX64, FileFormat.ELFBinary, false ->
        X64SwitchPatternAnalyzer.readTableEntriesX64ELFNOPIE core tbl tbl tblSize size Set.empty
      | Arch.IntelX64, FileFormat.ELFBinary, true ->
        X64SwitchPatternAnalyzer.readTableEntriesX64ELFPIE core tbl tbl tblSize size Set.empty
      | Arch.ARMv7, FileFormat.ELFBinary, false ->
        ARM32SwitchPatternAnalyzer.readTableEntriesARMELFNOPIE core tbl tbl tblSize size Set.empty
      | Arch.ARMv7, FileFormat.ELFBinary, true ->
        ARM32SwitchPatternAnalyzer.readTableEntriesARMELFPIE core tbl tbl tblSize size refAddr Set.empty
      | Arch.AARCH64, FileFormat.ELFBinary, _ ->
        ARM64SwitchPatternAnalyzer.readTableEntriesAArch64ELF core tbl tbl tblSize size refAddr Set.empty
      | Arch.MIPS32, FileFormat.ELFBinary, false ->
        MIPS32SwitchPatternAnalyzer.readTableEntriesMIPS32ELFNOPIE core tbl tbl tblSize size Set.empty
      | Arch.MIPS32, FileFormat.ELFBinary, true ->
        MIPS32SwitchPatternAnalyzer.readTableEntriesMIPS32ELFPIE core tbl tbl tblSize size Set.empty
      | Arch.MIPS64, FileFormat.ELFBinary, _ ->
        MIPS64SwitchPatternAnalyzer.readTableEntriesMIPS64ELF core tbl tbl tblSize size Set.empty
      | _ -> Set.empty
    with
      | _ -> Set.empty

  let recoverTableEntries core =
    core.ResolvedJmpTbls
    |> Seq.iter (fun (KeyValue (tblAddr, (insAddr, tblSize, size, refAddr))) ->
      let entries = readTableEntries core tblAddr tblSize size refAddr
      //printfn "Recovered targets (%x)" tblAddr
      //Set.iter (printfn "%x") entries
      core.JumpTables[insAddr] <- tblAddr, entries)

  let analyze core =
    recoverTableBase core
    recoverTableEntries core
