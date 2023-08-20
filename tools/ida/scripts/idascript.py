import idaapi, idautils, ida_funcs, idc

def dump_funcs(res_path):
  funcs = []

  for entry in Functions():
    funcs.append(int(entry))

  with open(res_path, 'w') as f:
    for entry in funcs:
      f.write('%x\n' % entry)

if __name__ == '__main__':
  idaapi.auto_wait()

  res_path = idc.ARGV[1]
  dump_funcs(res_path)

  ida_pro.qexit(0)
