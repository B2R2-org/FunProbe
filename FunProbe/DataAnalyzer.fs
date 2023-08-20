module FunProbe.DataAnalyzer

open B2R2
open B2R2.FrontEnd.BinFile
open B2R2.FrontEnd.BinFile.ELF
open B2R2.FrontEnd.BinFile.ELF.ExceptionHeaderEncoding
open B2R2.FrontEnd.BinLifter.Intel
open B2R2.FrontEnd.BinInterface

open System
open System.Collections.Generic

let rec parseFuncPointerSection core (addrs: HashSet<Addr>) (addr: Addr) eAddr =
  let hdl = core.Handle
  if addr = eAddr then ()
  else
    let wordSize = WordSize.toByteWidth hdl.ISA.WordSize
    let target = BinHandle.ReadUInt (hdl, addr, wordSize)
    if core.References.ContainsKey target |> not then
      core.References[target] <- List ()
    addrs.Add target |> ignore
    parseFuncPointerSection core addrs (addr + uint64 wordSize) eAddr

let parseInitArray core =
  core.Handle.FileInfo.GetSections ".init_array"
  |> Seq.iter (fun s ->
    let sAddr = s.Address
    let eAddr = sAddr + s.Size
    parseFuncPointerSection core core.InitArray sAddr eAddr)

let parseFiniArray core =
  core.Handle.FileInfo.GetSections ".fini_array"
  |> Seq.iter (fun s ->
    let sAddr = s.Address
    let eAddr = sAddr + s.Size
    parseFuncPointerSection core core.FiniArray sAddr eAddr)

let parseExceptionHandlingNormal core =
  let text = core.Handle.FileInfo.GetSections ".text" |> Seq.item 0
  core.Handle.FileInfo.ExceptionTable
  |> ARMap.iter (fun range pads ->
    if text.Address <= range.Min && range.Min < text.Address + text.Size then
      if core.References.ContainsKey range.Min |> not then
        core.References[range.Min] <- List ()
      core.ExceptionInfo.Add range.Min |> ignore
      ARMap.iter (fun tRange catch ->
        if catch <> 0UL then
          core.References[tRange.Min] <- List ()
          core.References[catch] <- List ()
          core.LandingPads[tRange.Min] <- catch
        else ()
        ) pads
    else ()
    )

let parseLSDAHeader cls (span: ByteSpan) reader sAddr offset =
  let b = span[offset]
  let offset = offset + 1
  let struct (lpv, lpapp) = parseEncoding b
  let struct (lpstart, offset) =
    if lpv = ExceptionHeaderValue.DW_EH_PE_omit then struct (None, offset)
    else
      let struct (cv, offset) = computeValue cls span reader lpv offset
      struct (Some (sAddr + uint64 offset + cv), offset)
  let b = span[offset]
  let offset = offset + 1
  let struct (ttv, ttapp) = parseEncoding b
  let struct (ttbase, offset) =
    if ttv = ExceptionHeaderValue.DW_EH_PE_omit then struct (None, offset)
    else
      let cv, offset = parseULEB128 span offset
      struct (Some (sAddr + uint64 offset + cv), offset)
  let b = span[offset]
  let offset = offset + 1
  let struct (csv, csapp) = parseEncoding b
  let cstsz, offset = parseULEB128 span offset
  { LPValueEncoding = lpv
    LPAppEncoding = lpapp
    LPStart = lpstart
    TTValueEncoding = ttv
    TTAppEncoding = ttapp
    TTBase = ttbase
    CallSiteValueEncoding = csv
    CallSiteAppEncoding = csapp
    CallSiteTableSize = cstsz }, offset

let rec parseCallSiteTable acc cls span reader offset csv =
  (* We found that GCC sometimes produces a wrong callsite table length, and the
     length can be off by one. So we minus one here. This is conservative
     anyways, because callsite entry can only be larger than three bytes. *)
  if offset >= (span: ByteSpan).Length - 3 then
    List.rev acc
  else
    let struct (start, offset) = computeValue cls span reader csv offset
    let struct (length, offset) = computeValue cls span reader csv offset
    let struct (landingPad, offset) = computeValue cls span reader csv offset
    let actionOffset, offset = parseULEB128 span offset
    let acc =
      if start = 0UL && length = 0UL && landingPad = 0UL && actionOffset = 0UL
      then acc (* This can appear due to the miscalculation issue above. *)
      else { Position = start
             Length = length
             LandingPad = landingPad
             ActionOffset = int actionOffset
             ActionTypeFilters = [] } :: acc
    parseCallSiteTable acc cls span reader offset csv

let parseTable hdl (rdr: IBinReader) tabAddr (tabBytes: ByteSpan) funcAddr off =
  let wordSize = hdl.ISA.WordSize
  let zWord = rdr.ReadUInt32 (tabBytes, off) |> uint64
  if zWord &&& 0xF0000000UL = 0x80000000UL then ARMap.empty
  else
    let fWord = rdr.ReadUInt32 (tabBytes, off + 4) |> uint64
    let cnt = (fWord &&& 0xFF000000UL) >>> 24 |> int
    let hdr, offset = parseLSDAHeader wordSize tabBytes rdr tabAddr (off + 8 + cnt * 4)
    let subspn = tabBytes.Slice (offset, int hdr.CallSiteTableSize)
    let encoding = hdr.CallSiteValueEncoding
    let callsites = parseCallSiteTable [] wordSize subspn rdr 0 encoding
    List.fold (fun pads cs ->
      if cs.LandingPad <> 0UL then
        let sAddr = funcAddr + cs.Position
        let eAddr = sAddr + cs.Length - 1UL
        let range = AddrRange (sAddr, eAddr)
        let landingpad = funcAddr + cs.LandingPad
        ARMap.add range landingpad pads
      else pads
      ) ARMap.empty callsites

let rec parseIndexTable hdl (rdr: IBinReader) idxAddr tabAddr (idxBytes: ByteSpan) (tabBytes: ByteSpan) map (off: int) size =
  if off = size then map
  else
    let fWord = rdr.ReadUInt32 (idxBytes, off) |> uint64
    let sWord = rdr.ReadUInt32 (idxBytes, off + 4) |> uint64
    let funcAddr = (fWord + idxAddr + uint64 off) &&& 0x7FFFFFFFUL
    if sWord = 1UL then
      let map = Map.add funcAddr ARMap.empty map
      parseIndexTable hdl rdr idxAddr tabAddr idxBytes tabBytes map (off + 8) size
    elif sWord &&& 0xF0000000UL = 0x80000000UL then
      let map = Map.add funcAddr ARMap.empty map
      parseIndexTable hdl rdr idxAddr tabAddr idxBytes tabBytes map (off + 8) size
    else
      let sAddr = (sWord + idxAddr + uint64 (off + 4)) &&& 0x7FFFFFFFUL
      let tabOff = int <| sAddr - tabAddr
      let pad = parseTable hdl rdr tabAddr tabBytes funcAddr tabOff
      let map = Map.add funcAddr pad map
      parseIndexTable hdl rdr idxAddr tabAddr idxBytes tabBytes map (off + 8) size

let parseExceptionHandlingARM core =
  let hdl = core.Handle
  let idxSecs = hdl.FileInfo.GetSections ".ARM.exidx"
  if Seq.length idxSecs = 0 then ()
  else
    let idxSec = Seq.item 0 idxSecs
    let idxBytes = BinHandle.ReadBytes (hdl, idxSec.Address, int idxSec.Size)
    let idxBytes = ByteSpan (idxBytes)
    let tabSecs = hdl.FileInfo.GetSections ".ARM.extab"
    let tabAddr, tabBytes =
      if Seq.length tabSecs = 0 then 0UL, [||]
      else
        let tabSec = Seq.item 0 tabSecs
        tabSec.Address, BinHandle.ReadBytes (hdl, tabSec.Address, int tabSec.Size)
    let tabBytes = ByteSpan (tabBytes)
    let rdr = BinReader.binReaderLE
    let eh = parseIndexTable hdl rdr idxSec.Address tabAddr idxBytes tabBytes Map.empty 0 (int idxSec.Size)
    let text = core.Handle.FileInfo.GetSections ".text" |> Seq.item 0
    eh
    |> Map.iter (fun addr pads ->
      if text.Address <= addr && addr < text.Address + text.Size then
        if core.References.ContainsKey addr |> not then
          core.References[addr] <- List ()
        core.ExceptionInfo.Add addr |> ignore
        ARMap.iter (fun tRange catch ->
          if catch <> 0UL then
            core.References[tRange.Min] <- List ()
            core.References[catch] <- List ()
            core.LandingPads[tRange.Min] <- catch
          else ()
          ) pads
      else ()
      )

let parseExceptionHandling core =
  match core.Handle.ISA.Arch with
  | Arch.ARMv7 -> parseExceptionHandlingARM core
  | _ -> parseExceptionHandlingNormal core

let parseMIPSDynamicSymbol core =
  let text = core.Handle.FileInfo.GetSections ".text" |> Seq.item 0
  core.Handle.FileInfo.GetDynamicSymbols ()
  |> Seq.iter (fun symb ->
    if symb.Kind = SymbolKind.SymFunctionType && symb.Address <> 0UL then
      if text.Address <= symb.Address && symb.Address < text.Address + text.Size then
        if core.References.ContainsKey symb.Address |> not then
          core.References[symb.Address] <- List ()
        core.DynamicSymbols.Add symb.Address |> ignore
      else ()
    else ()
    )

let parseRelocationSymbols core =
  let text = core.Handle.FileInfo.GetSections ".text" |> Seq.item 0
  core.Handle.FileInfo.GetRelocationSymbols ()
  |> Seq.iter (fun symb ->
    if text.Address <= symb.Address && symb.Address < text.Address + text.Size then
      if core.References.ContainsKey symb.Address |> not then
        core.References[symb.Address] <- List ()
      core.RelocationSymbols.Add symb.Address |> ignore
    else ()
    )

let findPointersX86PIE core =
  let rodata = core.Handle.FileInfo.GetSections ".rodata" |> Seq.item 0
  core.Instrs
  |> Seq.fold (fun candidates (KeyValue (addr, ins)) ->
    let ins = ins :?> IntelInstruction
    match ins.Opcode, ins.Operands with
    | Opcode.MOV, TwoOperands (OprReg _, OprMem (Some _, Some (_, Scale.X4), Some disp, _))
    | Opcode.ADD, TwoOperands (OprReg _, OprMem (Some _, Some (_, Scale.X4), Some disp, _)) ->
      let addr = core.GOT + uint64 disp
      if rodata.Address <= addr && addr < rodata.Address + rodata.Size then
        Set.add addr candidates
      else candidates
    | _ -> candidates
    ) Set.empty
  |> Set.toList

let mask32bit value =
  value &&& 0xFFFFFFFFUL

let loadTableX86PIE core _tbl (addr: Addr) =
  let text = core.Handle.FileInfo.GetSections ".text" |> Seq.item 0
  match BinHandle.TryReadUInt (core.Handle, addr, 4) with
  | Ok target ->
    let addr = (core.GOT + target) |> mask32bit
    if text.Address <= addr && addr < text.Address + text.Size then Some addr
    else None
  | _ -> None

let findPointersX86NOPIE core =
  let rodata = core.Handle.FileInfo.GetSections ".rodata" |> Seq.item 0
  core.Instrs
  |> Seq.fold (fun candidates (KeyValue (addr, ins)) ->
    let ins = ins :?> IntelInstruction
    match ins.Opcode, ins.Operands with
    | Opcode.MOV, TwoOperands (OprReg _, OprMem (None, Some (_, Scale.X4), Some disp, _))
    | Opcode.JMPNear, OneOperand (OprMem (None, Some (_, Scale.X4), Some disp, _)) ->
      let addr = uint64 disp
      if rodata.Address <= addr && addr < rodata.Address + rodata.Size then
        Set.add addr candidates
      else candidates
    | _ -> candidates
    ) Set.empty
  |> Set.toList

let loadTableX86NOPIE core _tbl (addr: Addr) =
  let text = core.Handle.FileInfo.GetSections ".text" |> Seq.item 0
  match BinHandle.TryReadUInt (core.Handle, addr, 4) with
  | Ok addr ->
    if text.Address <= addr && addr < text.Address + text.Size then Some addr
    else None
  | _ -> None

let findPointersX64PIE core =
  let rodata = core.Handle.FileInfo.GetSections ".rodata" |> Seq.item 0
  core.Instrs
  |> Seq.fold (fun candidates (KeyValue (addr, ins)) ->
    let ins = ins :?> IntelInstruction
    match ins.Opcode, ins.Operands with
    | Opcode.LEA, TwoOperands (OprReg _, OprMem (Some _, None, Some disp, _)) ->
      let addr = uint64 disp
      if rodata.Address <= addr && addr < rodata.Address + rodata.Size then
        Set.add addr candidates
      else candidates
    | _ -> candidates
    ) Set.empty
  |> Set.toList

let loadTableX64PIE core tbl (addr: Addr) =
  let text = core.Handle.FileInfo.GetSections ".text" |> Seq.item 0
  match BinHandle.TryReadUInt (core.Handle, addr, 4) with
  | Ok addr ->
    let addr = tbl + addr |> mask32bit
    if text.Address <= addr && addr < text.Address + text.Size then Some addr
    else None
  | _ -> None

let findPointersX64NOPIE core =
  let rodata = core.Handle.FileInfo.GetSections ".rodata" |> Seq.item 0
  core.Instrs
  |> Seq.fold (fun candidates (KeyValue (addr, ins)) ->
    let ins = ins :?> IntelInstruction
    match ins.Opcode, ins.Operands with
    | Opcode.JMPNear, OneOperand (OprMem (None, Some (_, Scale.X8), Some disp, _))
    | Opcode.MOV, TwoOperands (OprReg _, OprMem (None, Some (_, Scale.X8), Some disp, _)) ->
      let addr = uint64 disp
      if rodata.Address <= addr && addr < rodata.Address + rodata.Size then
        Set.add addr candidates
      else candidates
    | _ -> candidates
    ) Set.empty
  |> Set.toList

let loadTableX64NOPIE core _tbl (addr: Addr) =
  let text = core.Handle.FileInfo.GetSections ".text" |> Seq.item 0
  match BinHandle.TryReadUInt (core.Handle, addr, 8) with
  | Ok addr ->
    if text.Address <= addr && addr < text.Address + text.Size then Some addr
    else None
  | _ -> None

let rec readAll core (reader: Addr -> Addr -> Addr option) tbl addrs addr size =
  match reader tbl addr with
  | Some target ->
    if core.Instrs.ContainsKey target then
      if core.CallTargets.Contains target then Set.empty
      else
        let addrs = Set.add target addrs
        let addr = addr + uint64 size
        readAll core reader tbl addrs addr size
    else addrs
  | _ -> addrs

let rec readUntil core (reader: Addr -> Addr -> Addr option) tbl addrs addr size eAddr =
  if addr = eAddr then addrs
  else
    match reader tbl addr with
    | Some target ->
      if core.Instrs.ContainsKey target then
        if core.CallTargets.Contains target then Set.empty
        else
          let addrs = Set.add target addrs
          let addr = addr + uint64 size
          readAll core reader tbl addrs addr size
      else addrs
    | _ -> addrs

let rec detectJumpTableEntries core loader size = function
  | [] -> ()
  | [ addr ] ->
    let addrs = readAll core loader addr Set.empty addr size
    Set.iter (fun addr -> core.JumpTargets.Add addr |> ignore) addrs
  | addr1 :: ((addr2 :: _) as candidates) ->
    let addrs = readUntil core loader addr1 Set.empty addr1 size addr2
    Set.iter (fun addr -> core.JumpTargets.Add addr |> ignore) addrs
    detectJumpTableEntries core loader size candidates

let detectJumpTables core =
  match core.Handle.ISA.Arch with
  | Arch.IntelX86 ->
    if core.Handle.FileInfo.IsRelocatable then
      let candidates = findPointersX86PIE core
      let loader = loadTableX86PIE core
      detectJumpTableEntries core loader 4 candidates
    else
      let candidates = findPointersX86NOPIE core
      let loader = loadTableX86NOPIE core
      detectJumpTableEntries core loader 4 candidates
  | Arch.IntelX64 ->
    if core.Handle.FileInfo.IsRelocatable then
      let candidates = findPointersX64PIE core
      let loader = loadTableX64PIE core
      detectJumpTableEntries core loader 4 candidates
    else
      let candidates = findPointersX64NOPIE core
      let loader = loadTableX64NOPIE core
      detectJumpTableEntries core loader 8 candidates
  | _ -> ()

let collectCodePointers core (sec: Section) =
  let hdl = core.Handle
  let text = hdl.FileInfo.GetSections ".text" |> Seq.item 0
  let bytes = BinHandle.ReadBytes (hdl, sec.Address, int sec.Size)
  let wordSize = WordSize.toByteWidth hdl.ISA.WordSize
  [ 0 .. wordSize .. Array.length bytes - wordSize ]
  |> List.iter (fun idx ->
    let value =
      if wordSize = 4 then BitConverter.ToUInt32 bytes[idx .. idx + wordSize] |> uint64
      else BitConverter.ToUInt64 bytes[idx .. idx + wordSize] |> uint64
    if text.Address <= value && value < text.Address + text.Size then
      if core.AddrMap.ContainsKey value then
        core.Constants.Add value |> ignore
    else ()
    )

let harvestCodePointers core =
  core.Handle.FileInfo.GetSections ()
  |> Seq.iter (fun sec ->
    if sec.Name.Contains ".data" then collectCodePointers core sec
    else ()
    )

let analyze core =
  parseInitArray core
  parseFiniArray core
  parseExceptionHandling core
  parseMIPSDynamicSymbol core
  parseRelocationSymbols core
