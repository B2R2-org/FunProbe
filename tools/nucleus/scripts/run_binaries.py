import os, sys, time

BENCHDIR = '/outputs/bench'
RESDIR = '/outputs/results'
TOOLNAME = 'nucleus'

def main(pkg, arch, comp, pie, opt, timeout):
  bin_dir = os.path.join(BENCHDIR, 'stripbin', pkg, arch, comp, pie, opt)
  res_dir = os.path.join(RESDIR, TOOLNAME, pkg, arch, comp, pie, opt)
  time_dir = os.path.join(RESDIR, TOOLNAME + '_time', pkg, arch, comp, pie, opt)

  os.system('mkdir -p %s' % res_dir)
  os.system('mkdir -p %s' % time_dir)

  for name in os.listdir(bin_dir):
    bin_path = os.path.join(bin_dir, name)
    res_path = os.path.join(res_dir, name)
    time_path = os.path.join(time_dir, name)

    os.system('rm -f %s' % res_path)
    os.system('rm -f %s' % time_path)

    if arch == 'x86':
      arch_str = 'x86-32'
    elif arch == 'x64':
      arch_str = 'x86-64'
    else:
      arch_str = arch

    stime = time.time()
    os.system('timeout %s /scripts/run_nucleus.sh %s %s %s' % (timeout, bin_path, res_path, arch_str))
    etime = time.time()

    with open(time_path, 'w') as f:
      f.write('%f\n' % (etime - stime))

if __name__ == '__main__':
  pkg = sys.argv[1]
  arch = sys.argv[2]
  comp = sys.argv[3]
  pie = sys.argv[4]
  opt = sys.argv[5]
  timeout = sys.argv[6]
  main(pkg, arch, comp, pie, opt, timeout)
