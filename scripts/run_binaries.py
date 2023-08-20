import os, sys, time

BENCHDIR = '/outputs/bench'
RESDIR = '/outputs/results'
TOOLNAME = 'funprobe-%s'

def main(solver, pkg, arch, comp, pie, opt, timeout):
  bin_dir = os.path.join(BENCHDIR, 'stripbin', pkg, arch, comp, pie, opt)
  res_dir = os.path.join(RESDIR, TOOLNAME % solver, pkg, arch, comp, pie, opt)
  time_dir = os.path.join(RESDIR, TOOLNAME % solver + '_time', pkg, arch, comp, pie, opt)

  os.system('mkdir -p %s' % res_dir)
  os.system('mkdir -p %s' % time_dir)

  for name in os.listdir(bin_dir):
    bin_path = os.path.join(bin_dir, name)
    res_path = os.path.join(res_dir, name)
    time_path = os.path.join(time_dir, name)

    os.system('rm -f %s' % res_path)
    os.system('rm -f %s' % time_path)

    arg = '--solver %s' % solver
    if arch in ['mips', 'mips64']:
      got_dir = os.path.join(BENCHDIR, 'mips_got', pkg, arch, comp, pie, opt)
      got_path = os.path.join(got_dir, name)
      arg += ' --mipsgot %s' % got_path

    stime = time.time()
    os.system('timeout %s /scripts/run_funprobe.sh %s %s "%s"' % (timeout, bin_path, res_path, arg))
    etime = time.time()

    with open(time_path, 'w') as f:
      f.write('%f\n' % (etime - stime))

if __name__ == '__main__':
  solver = sys.argv[1]
  pkg = sys.argv[2]
  arch = sys.argv[3]
  comp = sys.argv[4]
  pie = sys.argv[5]
  opt = sys.argv[6]
  timeout = sys.argv[7]
  main(solver, pkg, arch, comp, pie, opt, timeout)
