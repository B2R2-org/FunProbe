module FunProbe.Main

open B2R2
open B2R2.FrontEnd.BinInterface

let buildBinHandle (binPath: string) =
  BinHandle.Init (ISA.DefaultISA, binPath)

let dump path solutions =
  let lines =
    Seq.fold (fun lines addr ->
      sprintf "%x" addr :: lines) [] solutions
  System.IO.File.WriteAllLines (path, lines)

let dumpProb probPath probMap =
  let lines =
    Seq.fold (fun lines (KeyValue (addr, (shallows, prob))) ->
      sprintf "%x %A %f" addr shallows prob :: lines) [] probMap
  System.IO.File.WriteAllLines (probPath, lines)

let run (opts: CmdOpts) =
  let binPath = opts.BinPath
  let resPath = opts.DumpPath
  let hdl = buildBinHandle binPath
  let core = Core.init hdl
  if opts.Exception then
    DataAnalyzer.analyze core
    CodeAnalyzer.analyze opts core
    DataAnalyzer.harvestCodePointers core
    DataAnalyzer.detectJumpTables core
    let constMgr = ConstraintBuilder.build core
    let solutions, probMap = ConstraintSolver.solve opts core constMgr
    dump resPath solutions
    if opts.ProbPath <> "" then dumpProb opts.ProbPath probMap
  else
    try
      DataAnalyzer.analyze core
      CodeAnalyzer.analyze opts core
      DataAnalyzer.harvestCodePointers core
      DataAnalyzer.detectJumpTables core
      let constMgr = ConstraintBuilder.build core
      let solutions, probMap = ConstraintSolver.solve opts core constMgr
      dump resPath solutions
      if opts.ProbPath <> "" then dumpProb opts.ProbPath probMap
    with
      | _ -> printfn "[ERROR] run: %s" binPath

[<EntryPoint>]
let main (args: string []) =
  let opts = CmdOpts.optParse args
  run opts
  0
