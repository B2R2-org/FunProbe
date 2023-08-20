module FunProbe.NopOracle

open B2R2
open B2R2.FrontEnd.BinLifter
open B2R2.FrontEnd.BinInterface

let isIntelNop (ins: Instruction) =
  let ins = ins :?> Intel.IntelInstruction
  match ins.Opcode, ins.Operands with
  | Intel.Opcode.NOP, _ -> true
  | Intel.Opcode.INT3, _ -> true
  | Intel.Opcode.XCHG, Intel.TwoOperands (Intel.OprReg r1, Intel.OprReg r2) ->
    r1 = r2
  | Intel.Opcode.LEA,
      Intel.TwoOperands (Intel.OprReg r1, Intel.OprMem (Some r2, None, Some 0L, _)) ->
    r1 = r2
  | Intel.Opcode.MOV, Intel.TwoOperands (Intel.OprReg r1, Intel.OprReg r2) ->
    r1 = r2
  | _ -> false

let isARMNop (ins: Instruction) =
  ins.IsNop ()

let isMIPSNop (ins: Instruction) =
  ins.IsNop ()

let isNop hdl ins =
  match hdl.FileInfo.ISA.Arch with
  | Arch.IntelX86
  | Arch.IntelX64 -> isIntelNop ins
  | Arch.AARCH64 -> isARMNop ins
  | Arch.MIPS32
  | Arch.MIPS64 -> isMIPSNop ins
  | _ -> false
