namespace FunProbe

open B2R2
open B2R2.BinIR.LowUIR
open B2R2.FrontEnd.BinLifter
open B2R2.FrontEnd.BinFile
open B2R2.FrontEnd.BinInterface
open System.Collections.Generic

type InstrMap = SortedList<Addr, Instruction>
type StmtMap = Dictionary<Addr, Stmt []>

type BasicBlock = {
  BBLIdx: int
  BBLAddr: Addr
  BBLSize: uint64
  BBLLastAddr: Addr
  BBLIsNop: bool
  BBLIsPadding: bool
}
with
  static member init idx addr size last isNop isPad =
    { BBLIdx = idx
      BBLAddr = addr
      BBLSize = size
      BBLLastAddr = last
      BBLIsNop = isNop
      BBLIsPadding = isPad }

type Core = {
  Handle: BinHandle
  EntryPoint: Addr
  Text: Section
  GOT: Addr
  Instrs: InstrMap
  /// For fast lookup
  PrevInsAddrs: Dictionary<Addr, Addr>
  Stmts: StmtMap
  ResolvedJmpTbls: SortedList<Addr, Addr * int * int * Addr>
  JumpTables: SortedList<Addr, Addr * Set<Addr>>
  NoRets: HashSet<Addr>
  CondNoRets: HashSet<Addr>
  CallTargets: HashSet<Addr>
  /// Consider explicit references only except implicit (fall-through)
  /// references.
  References: Dictionary<Addr, List<Addr>>
  DataReferences: HashSet<Addr>
  //BasicBlocks: SortedList<Addr, BasicBlock>
  AddrMap: Dictionary<Addr, int>
  mutable BasicBlocks: BasicBlock []
  mutable Preds: int list []
  mutable Succs: int list []
  InitArray: HashSet<Addr>
  FiniArray: HashSet<Addr>
  ExceptionInfo: HashSet<Addr>
  DynamicSymbols: HashSet<Addr>
  RelocationSymbols: HashSet<Addr>
  LandingPads: Dictionary<Addr, Addr>
  JumpTargets: HashSet<Addr>
  Constants: HashSet<Addr>
  Addrs: HashSet<Addr>
}
with
  static member init hdl =
    let ep =
      match hdl.FileInfo.EntryPoint with
      | Some ep -> ep
      | None -> 0UL
    let text = hdl.FileInfo.GetTextSections () |> Seq.item 0
    let got =
      match hdl.ISA.Arch with
      | Arch.IntelX86 ->
        hdl.FileInfo.GetSymbols ()
        |> Seq.tryFind (fun symb -> symb.Name = "_GLOBAL_OFFSET_TABLE_")
        |> function
          | None ->
            let gots = hdl.FileInfo.GetSections ".got.plt"
            let got =
              if Seq.length gots = 0 then
                hdl.FileInfo.GetSections ".got"
                |> Seq.item 0
              else
                Seq.item 0 gots
            got.Address
          | Some symb -> symb.Address
      | Arch.MIPS32 | Arch.MIPS64 ->
        hdl.FileInfo.GetSymbols ()
        |> Seq.tryFind (fun symb -> symb.Name = "_gp")
        |> function
          | None ->
            let got =
              hdl.FileInfo.GetSections ".got"
              |> Seq.item 0
            got.Address + 0x7ff0UL
          | Some symb -> symb.Address
      | _ -> 0UL
    { Handle = hdl
      EntryPoint = ep
      Text = text
      GOT = got
      Instrs = InstrMap ()
      PrevInsAddrs = Dictionary ()
      Stmts = StmtMap ()
      ResolvedJmpTbls = SortedList ()
      JumpTables = SortedList ()
      NoRets = HashSet ()
      CondNoRets = HashSet ()
      CallTargets = HashSet ()
      References = Dictionary ()
      DataReferences = HashSet ()
      AddrMap = Dictionary ()
      BasicBlocks = [||]
      Preds = [||]
      Succs = [||]
      InitArray = HashSet ()
      FiniArray = HashSet ()
      ExceptionInfo = HashSet ()
      DynamicSymbols = HashSet ()
      RelocationSymbols = HashSet ()
      LandingPads = Dictionary ()
      JumpTargets = HashSet ()
      Constants = HashSet ()
      Addrs = HashSet () }

  static member isInText addr core =
    core.Text.Address <= addr && addr < core.Text.Address + core.Text.Size

  static member addRef fromAddr toAddr core =
    if core.References.ContainsKey toAddr then ()
    else core.References[toAddr] <- List ()
    core.References[toAddr].Add fromAddr
