import argparse, os, random, subprocess, sys

from multiprocessing import Pool

def run_cmd(cmd):
  print('[*] Executing: %s' % cmd)
  cmd_args = cmd.split()

  try:
    subprocess.call(cmd_args)
  except Exception as e:
    print(e)
    exit(-1)

def run_cmd_in_docker(name, cmd):
  print("[*] Executing (in container %s): %s" % (name, cmd))
  cmd_prefix = "docker exec %s /bin/bash -c" %  name
  cmd_args = cmd_prefix.split()
  cmd_args.append(cmd)

  try:
    subprocess.call(cmd_args)
  except Exception as e:
    print(e)
    exit(1)

def get_proc_output(cmd):
  proc = subprocess.Popen(cmd, stdout=subprocess.PIPE)
  out, _ = proc.communicate()
  out = out.decode()
  lines = out.split('\n')

  return lines

# -------------------------------->%

def parse_args():
  p = argparse.ArgumentParser()
  p.add_argument('--datadir', required=True)
  p.add_argument('--image', required=False, default='funprobe-benchmark')
  p.add_argument('--package', nargs='+', required=True)
  p.add_argument('--arch', nargs='+', required=True)
  p.add_argument('--compiler', nargs='+', required=True)
  p.add_argument('--pie', nargs='+', required=True)
  p.add_argument('--optlevel', nargs='+', required=True)
  p.add_argument('--ncores', required=False, type=int, default=1)
  p.add_argument('--uid', required=False, default='0')
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
  random.shuffle(confs)

  return confs

def conf_to_str(conf):
    return '%s-%s-%s-%s-%s' % conf

# -------------------------------->%

def spawn_container(image, name, uid, dirpath):
  cmd = ''
  cmd += 'docker run'
  cmd += ' -u $(id -u %s:%s' % (uid, uid)
  cmd += ' -v %s:/outputs' % dirpath
  cmd += ' --rm'
  cmd += ' -dit'
  cmd += ' --name %s' % name
  cmd += ' %s' % image

  run_cmd(cmd)

def kill_container(name):
  cmd = ''
  cmd += 'docker kill %s' % name

  run_cmd(cmd)

# -------------------------------->%

def run_builder(conf, name):
  pkg, arch, comp, pie, opt = conf

  cmd = ''
  if pkg == 'spec':
    cmd += '/packages/build_spec.sh'
  else:
    cmd += '/packages/build_benchmark.sh'
    cmd += ' %s' % pkg
  cmd += ' %s' % arch
  cmd += ' %s' % comp
  cmd += ' %s' % pie
  cmd += ' %s' % opt
  cmd += ' /outputs'

  run_cmd_in_docker(name, cmd)

def collect_binary(args, conf, name):
  pkg, arch, comp, pie, opt = conf

  build_dir = os.path.join(args.datadir, 'build')
  bench_dir = os.path.join(args.datadir, 'bench')

  build_dir = os.path.join(build_dir, name)
  bin_dir = os.path.join(bench_dir, 'bin', pkg, arch, comp, pie, opt)
  stripbin_dir = os.path.join(bench_dir, 'stripbin', pkg, arch, comp, pie, opt)

  os.system('mkdir -p %s' % bin_dir)
  os.system('mkdir -p %s' % stripbin_dir)

  os.system('cp %s/bin/* %s' % (build_dir, bin_dir))
  os.system('cp %s/stripbin/* %s' % (build_dir, stripbin_dir))

def analyze_symbol(conf, bin_path):
  _, arch, _, _, _ = conf
  symbols = set()

  cmd = ['objdump', '-tf', bin_path]
  lines = get_proc_output(cmd)

  for line in lines:
    if 'F .text' in line:
      fname = line.split()[-1]
      addr = int(line.split()[0], 16)
      if '.part' in fname or '.cold' in fname:
        continue
      else:
        if addr != 0:
          symbols.add(addr)
        if arch.startswith('mips') and '__start' == fname:
          symbols.add(addr + 0xc)

  if arch == 'x86':
    cmd = ['gdb', '-batch', '-ex', "disas _start", bin_path]
    lines = get_proc_output(cmd)

    for line in lines:
      if 'call' in line and '_start+' in line:
        line = line.strip()
        symbols.add(int(line.split()[3], 16))
        break

  return symbols

def analyze_dwarf(bin_path):
  dwarfs = set()

  cmd = ['readelf', '--debug-dump=info', bin_path]
  lines = get_proc_output(cmd)

  in_elem = False
  for line in lines:
    if 'Abbrev Number' in line:
      line = line.split('Abbrev Number:')[1]
      if '(' in line:
        tag = line.split('(')[1].split(')')[0]
        if tag == 'DW_TAG_subprogram':
          in_elem = True
        else:
          in_elem = False
      else:
        in_elem = False
    elif in_elem:
      if 'DW_AT_low_pc' in line:
        addr = line.split(':')[1]
        addr = addr.strip()
        addr = int(addr, 16)
        if addr != 0:
          dwarfs.add(addr)

  return dwarfs

def build_mips_got(bin_path, got_path):
  cmd = ['readelf', '-a', bin_path]
  lines = get_proc_output(cmd)

  got_lines = []
  f = False
  for line in lines:
    line = line.strip()
    if line == '':
      continue

    if line.startswith('Global entries:'):
      f = True

    if f and line.startswith('0'):
      got_lines.append(line)

  func_map = {}
  for line in got_lines:
    tokens = line.split()
    if len(tokens) == 7:
      addr = int(tokens[0], 16)
      ty = tokens[4]
      name = tokens[6]
    elif len(tokens) == 6:
      addr = int(tokens[0], 16)
      ty = tokens[3]
      name = tokens[5]

    if ty != 'FUNC':
      continue

    func_map[addr] = name

  with open(got_path, 'w') as f:
    for addr in func_map:
      f.write('%x %s\n' % (addr, func_map[addr]))

def build_gt(args, conf):
  pkg, arch, comp, pie, opt = conf

  bench_dir = os.path.join(args.datadir, 'bench')
  bin_dir = os.path.join(bench_dir, 'bin', pkg, arch, comp, pie, opt)
  gt_dir = os.path.join(bench_dir, 'gt', pkg, arch, comp, pie, opt)

  os.system('mkdir -p %s' % gt_dir)

  for bin_name in os.listdir(bin_dir):
    bin_path = os.path.join(bin_dir, bin_name)
    symbols = analyze_symbol(conf, bin_path)
    dwarfs = analyze_dwarf(bin_path)
    gt = symbols | dwarfs

    gt_path = os.path.join(gt_dir, bin_name)
    with open(gt_path, 'w') as f:
      for addr in gt:
        f.write('%x\n' % addr)

  if arch in ['mips', 'mips64']:
    got_dir = os.path.join(bench_dir, 'mips_got', pkg, arch, comp, pie, opt)

    os.system('mkdir -p %s' % got_dir)

    for bin_name in os.listdir(bin_dir):
      bin_path = os.path.join(bin_dir, bin_name)
      got_path = os.path.join(got_dir, bin_name)
      build_mips_got(bin_path, got_path)

def build(arg):
  args, conf = arg
  conf_str = conf_to_str(conf)
  build_dir = os.path.join(args.datadir, 'build')
  spawn_container(args.image, conf_str, args.uid, build_dir)
  run_builder(conf, conf_str)
  kill_container(conf_str)
  collect_binary(args, conf, conf_str)
  build_gt(args, conf)

# -------------------------------->%

def main(args):
  confs = enumerate_confs(args)
  pool = Pool(args.ncores)
  pool.map(build, confs)

# -------------------------------->%

if __name__ == '__main__':
  args = parse_args()
  check_args(args)
  main(args)
