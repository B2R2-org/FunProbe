from ghidra.program.model.block import BasicBlockModel
from ghidra.app.decompiler import *
from ghidra.framework.plugintool.util import OptionsService

def dumpFuncs(outPath):
  # Set image base to 0
  curImageBase = currentProgram.getImageBase()
  currentProgram.setImageBase(curImageBase.subtract(curImageBase.getOffset()), False)

  funcs = []

  functionManager = currentProgram.getFunctionManager()
  for func in functionManager.getFunctions(True):
    if func.isExternal() or func.isThunk():
      continue

    entry = func.getEntryPoint().getOffset()
    funcs.append(entry)

  with open(outPath, 'w') as f:
    for entry in funcs:
      f.write('%x\n' % entry)

if __name__ =='__main__':
  outPath = getScriptArgs()[0]
  dumpFuncs(outPath)
