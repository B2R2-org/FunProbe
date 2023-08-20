import os, sys, time

BENCHDIR = '/outputs/bench'
RESDIR = '/outputs/results'
TOOLNAME = 'funprobe-xda'

def main(pkg, arch, comp, pie, opt):
  bin_dir = os.path.join(BENCHDIR, 'stripbin', pkg, arch, comp, pie, opt)
  res_dir = os.path.join(RESDIR, TOOLNAME, pkg, arch, comp, pie, opt)
  xda_dir = os.path.join(RESDIR, 'xda', pkg, arch, comp, pie, opt)

  os.system('mkdir -p %s' % res_dir)

  for name in os.listdir(bin_dir):
    bin_path = os.path.join(bin_dir, name)
    res_path = os.path.join(res_dir, name)
    xda_path = os.path.join(xda_dir, name)

    os.system('rm -f %s' % res_path)

    arg = '--solver bdp'
    arg += ' --addr %s' % xda_path
    if arch in ['mips', 'mips64']:
      got_dir = os.path.join(BENCHDIR, 'mips_got', pkg, arch, comp, pie, opt)
      got_path = os.path.join(got_dir, name)
      arg += ' --mipsgot %s' % got_path

    os.system('/scripts/run_funprobe.sh %s %s "%s"' % (bin_path, res_path, arg))

if __name__ == '__main__':
  pkg = sys.argv[1]
  arch = sys.argv[2]
  comp = sys.argv[3]
  pie = sys.argv[4]
  opt = sys.argv[5]
  main(pkg, arch, comp, pie, opt)
