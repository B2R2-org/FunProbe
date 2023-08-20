import os, sys, time

BENCHDIR = '/outputs/bench'
RESDIR = '/outputs/results'
LISTPATH = '/outputs/binlist.txt'

def load_binlist():
  bin_list = []

  with open(LISTPATH) as f:
    for line in f.readlines():
      bin_list.append(line.strip().split(','))

  return bin_list

def main(pos, neg):
  bin_list = load_binlist()

  for pkg, arch, comp, pie, opt, name in bin_list:
    res_dir = os.path.join(RESDIR, 'param_%.2f_%.2f' % (pos, neg), pkg, arch, comp, pie, opt)

    os.system('mkdir -p %s' % res_dir)

    bin_path = os.path.join(BENCHDIR, 'stripbin', pkg, arch, comp, pie, opt, name)
    res_path = os.path.join(res_dir, name)

    os.system('rm -f %s' % res_path)

    arg = '--solver bdp'
    arg += ' -p %.2f' % pos
    arg += ' -n %.2f' % neg
    if arch in ['mips', 'mips64']:
      got_dir = os.path.join(BENCHDIR, 'mips_got', pkg, arch, compiler, pie, opt)
      got_path = os.path.join(got_dir, name)
      arg += ' --mipsgot %s' % got_path

    os.system('/scripts/run_funprobe.sh %s %s "%s"' % (bin_path, res_path, arg))

if __name__ == '__main__':
  pos = float(sys.argv[1])
  neg = float(sys.argv[2])
  main(pos, neg)
