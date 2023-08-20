import binaryninja, sys

def main(bin_path, res_path):
  bv = binaryninja.open_view(bin_path)

  with open(res_path, 'w') as f:
    for func in bv.functions:
      f.write('%x\n' % func.start)

if __name__ == '__main__':
  bin_path = sys.argv[1]
  res_path = sys.argv[2]

  main(bin_path, res_path)
