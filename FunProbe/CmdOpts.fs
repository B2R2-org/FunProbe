namespace FunProbe

open B2R2.FsOptParse

type CmdOpts = {
  BinPath: string
  MIPSGOTPath: string
  DumpPath: string
  ProbPath: string
  AddrPath: string
  Exception: bool
  PositiveProb: float
  NegativeProb: float
  Solver: string
}

[<RequireQualifiedAccess>]
module CmdOpts =

  let private defaultOpts =
    { BinPath = ""
      MIPSGOTPath = ""
      DumpPath = ""
      ProbPath = ""
      AddrPath = ""
      Exception = false
      PositiveProb = 0.65
      NegativeProb = 0.4
      Solver = "bdp" }

  let private spec =
    [ Option (descr = "# Options\n", dummy = true)

      Option (descr = "Specify the target binary.",
              required = true,
              extra = 1,
              callback = (fun opts args -> { opts with BinPath = args[0] }),
              short = "-b",
              long = "--bin")

      Option (descr = "Specify the parsed got path.",
              required = false,
              extra = 1,
              callback = (fun opts args -> { opts with MIPSGOTPath = args[0] }),
              long = "--mipsgot")

      Option (descr = "Specify the function file to dump.",
              required = true,
              extra = 1,
              callback = (fun opts args -> { opts with DumpPath = args[0] }),
              short = "-f",
              long = "--function")

      Option (descr = "Specify the probability file to dump.",
              required = false,
              extra = 1,
              callback = (fun opts args -> { opts with ProbPath = args[0] }),
              short = "-i",
              long = "--info")

      Option (descr = "Specify the address file to feed.",
              required = false,
              extra = 1,
              callback = (fun opts args -> { opts with AddrPath = args[0] }),
              short = "-a",
              long = "--addr")

      Option (descr = "Specify the exception suppression option.",
              required = false,
              callback = (fun opts args -> { opts with Exception = true }),
              short = "-e",
              long = "--exception")

      Option (descr = "Specify the probability of positive constraints.",
              required = false,
              extra = 1,
              callback = (fun opts args -> { opts with PositiveProb = float args[0] }),
              short = "-p",
              long = "--positive")

      Option (descr = "Specify the probability of negative constraints.",
              required = false,
              extra = 1,
              callback = (fun opts args -> { opts with NegativeProb = float args[0] }),
              short = "-n",
              long = "--negative")

      Option (descr = "Specify the graph solver.",
              required = false,
              extra = 1,
              callback = (fun opts args -> { opts with Solver = args[0] }),
              long = "--solver" )

      ]

  let private usageGetter () = "\nUsage: FunProbe [Options]"

  let [<Literal>] private Prog = "FunProbe"

  let private exit1 () = exit 1

  let usagePrint () =
    usagePrint spec Prog usageGetter exit1

  let optParse args =
    try
      optParse spec usageGetter Prog args defaultOpts |> snd
    with
      | SpecErr msg ->
        System.Console.WriteLine ($"Invalid spec: {msg}")
        usagePrint ()
      | RuntimeErr msg ->
        System.Console.WriteLine ($"Invalid args: {msg}")
        usagePrint ()
