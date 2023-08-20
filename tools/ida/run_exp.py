import argparse, os, subprocess, sys, time

# Put run.bat and idascript.py into the same directory

def parse_args():
  p = argparse.ArgumentParser()
  p.add_argument('--datadir', required=True)
  p.add_argument('--toolname', required=True, default='ida')
  p.add_argument('--package', nargs='+', required=True)
  p.add_argument('--arch', nargs='+', required=True)
  p.add_argument('--compiler', nargs='+', required=True)
  p.add_argument('--pie', nargs='+', required=True)
  p.add_argument('--optlevel', nargs='+', required=True)
  args = p.parse_args()

  return args

def check_args(args):
  os.system('mkdir -p %s' % args.datadir)

  for pkg in args.package:
    if pkg not in ['coreutils', 'binutils', 'spec']:
      print('Invalid package: %s' % pkg)
      sys.exit(-1)

  for arch in args.arch:
    if arch not in ['x86', 'x64', 'arm', 'aarch64', 'mips', 'mips64']:
      print('Invalid architecture: %s' % arch)
      sys.exit(-1)

  for comp in args.compiler:
    if comp not in ['gcc', 'clang']:
      print('Invalid compiler: %s' % comp)
      sys.exit(-1)

  for pie in args.pie:
    if pie not in ['pie', 'nopie']:
      print('Invalid PIE option kind: %s' % pie)
      sys.exit(-1)

  for opt in args.optlevel:
    if opt not in ['o0', 'o1', 'o2', 'o3', 'os', 'ofast']:
      print('Invalid optimization level option kind: %s' % pie)
      sys.exit(-1)

# -------------------------------->%

def enumerate_confs(args):
  confs = []

  for pkg in args.package:
    for arch in args.arch:
      for comp in args.compiler:
        for pie in args.pie:
          for opt in args.optlevel:
            conf = pkg, arch, comp, pie, opt
            confs.append((args, conf))

  return confs

# -------------------------------->%

def run_cmd(cmd):
  print('[*] Executing: %s' % ' '.join(cmd))

  try:
    si = subprocess.STARTUPINFO()
    si.dwFlags |= subprocess.STARTF_USESHOWWINDOW
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE, startupinfo=si)
    p.communicate()
    p.wait()
  except Exception as e:
    print(e)
    exit(-1)

# -------------------------------->%

def run_ida(args, conf):
  pkg, arch, comp, pie, opt = conf

  bin_dir = os.path.join(args.datadir, 'bench', 'stripbin', pkg, arch, comp, pie, opt)
  res_dir = os.path.join(args.datadir, 'results', args.toolname, pkg, arch, comp, pie, opt)
  time_dir = os.path.join(args.datadir, 'results', args.toolname + '_time', pkg, arch, comp, pie, opt)

  os.makedirs(res_dir, exist_ok=True)
  os.makedirs(time_dir, exist_ok=True)

  for name in os.listdir(bin_dir):
    if name.endswith('.i64'):
      continue

    bin_path = os.path.join(bin_dir, name)
    res_path = os.path.join(res_dir, name)
    time_path = os.path.join(time_dir, name)

    stime = time.time()

    cmd = ['C:\\scripts\\run.bat', bin_path, res_path]
    run_cmd(cmd)

    etime = time.time()

    with open(time_path, 'w') as f:
      f.write('%f\n' % (etime - stime))

def main(args):
  confs = enumerate_confs(args)
  for conf in confs:
    run_ida(args, conf)

if __name__ == '__main__':
  args = parse_args()
  check_args(args)
  main(args)
