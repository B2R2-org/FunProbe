namespace FunProbe

open B2R2

[<AbstractClass>]
type Solver () =

  abstract Belief: int -> Factor

  abstract Graph: FactorGraph

module SolverUtils =

  let initFunc opts addr =
    Factor.init opts ([|addr|], FuncCond)

  let initBasic opts addr factors =
    Factor.init opts ([|addr|], BasicCond) :: factors

  let initNop opts addr factors =
    Factor.init opts ([|addr|], NopCond) :: factors

  let initCall opts addr factors =
    Factor.init opts ([|addr|], CallCond) :: factors

  let initUnreachable opts addr factors =
    Factor.init opts ([|addr|], UnreachableCond) :: factors

  let initTailCall opts addr1 addr2 =
    Factor.init opts ([|addr1; addr2|], TailCallCond)

  let initCondTailCall opts addr factors =
    Factor.init opts ([|addr|], CondTailCallCond) :: factors

  let initFakeCall opts addr factors =
    Factor.init opts ([|addr|], FakeCallCond) :: factors

  let initSurround opts addr factors =
    Factor.init opts ([|addr|], SurroundCond) :: factors

  let initException opts addr factors =
    Factor.init opts ([|addr|], ExceptionCond) :: factors

  let initReloc opts addr factors =
    Factor.init opts ([|addr|], RelocCond) :: factors

  let initCold opts addr factors =
    Factor.init opts ([|addr|], ColdCond) :: factors

  let initCallJmp opts addr factors =
    Factor.init opts ([|addr|], CallJmpCond) :: factors

  let initJmpNop opts addr factors =
    Factor.init opts ([|addr|], JmpNopCond) :: factors

  let initConst opts addr factors =
    Factor.init opts ([|addr|], ConstCond) :: factors

  let initAddr opts addr factors =
    Factor.init opts ([|addr|], AddrCond) :: factors

  let rec combineShallows opts addr (idx, factors) c =
    if c then
      if idx = 1 then idx + 1, initBasic opts addr factors
      elif idx = 2 then idx + 1, initNop opts addr factors
      elif idx = 3 then idx + 1, initCall opts addr factors
      elif idx = 4 then idx + 1, initUnreachable opts addr factors
      elif idx = 5 then idx + 1, initCondTailCall opts addr factors
      elif idx = 6 then idx + 1, initFakeCall opts addr factors
      elif idx = 7 then idx + 1, initSurround opts addr factors
      elif idx = 8 then idx + 1, initException opts addr factors
      elif idx = 9 then idx + 1, initReloc opts addr factors
      elif idx = 10 then idx + 1, initCold opts addr factors
      elif idx = 11 then idx + 1, initCallJmp opts addr factors
      elif idx = 12 then idx + 1, initJmpNop opts addr factors
      elif idx = 13 then idx + 1, initConst opts addr factors
      elif idx = 14 then idx + 1, initAddr opts addr factors
      else Utils.impossible ()
    else idx + 1, factors
