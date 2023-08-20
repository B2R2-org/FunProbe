module FunProbe.CodeAnalyzer

let computeRefs core =
  core.Instrs
  |> Seq.iter (fun (KeyValue (addr, ins)) ->
    match ins.DirectBranchTarget () with
    | true, target -> Core.addRef addr target core
    | _ -> ())

let addIndirectJumpRefs core =
  core.JumpTables
  |> Seq.iter (fun (KeyValue (addr, (_, targets))) ->
    targets
    |> Set.iter (fun target -> Core.addRef addr target core))

let loadAddrs opts core =
  if opts.AddrPath <> "" then
    let lines = System.IO.File.ReadAllLines opts.AddrPath
    lines
    |> Array.iter (fun line ->
      let addr = System.Convert.ToUInt64 (line, 16)
      if core.AddrMap.ContainsKey addr then
        core.Addrs.Add addr |> ignore
      else ())
  else ()

let analyze opts core =
  Disassembler.run core
  DataSeparator.run core
  NonReturningAnalyzer.run opts core
  computeRefs core
  SwitchAnalyzer.analyze core
  addIndirectJumpRefs core
  BasicBlockBuilder.run core
  loadAddrs opts core
