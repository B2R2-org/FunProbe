import argparse, subprocess, sys

from multiprocessing import Pool
import multiprocessing

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

# -------------------------------->%

def parse_args():
  p = argparse.ArgumentParser()
  p.add_argument('--datadir', required=True)
  p.add_argument('--tool', required=True)
  p.add_argument('--package', nargs='+', required=True)
  p.add_argument('--arch', nargs='+', required=True)
  p.add_argument('--compiler', nargs='+', required=True)
  p.add_argument('--pie', nargs='+', required=True)
  p.add_argument('--optlevel', nargs='+', required=True)
  p.add_argument('--ncores', required=False, type=int, default=1)
  p.add_argument('--timeout', required=False, default='1h')
  args = p.parse_args()

  return args

def check_args(args):
  if args.tool not in ['binaryninja', 'funprobe', 'funprobe-lbp', 'ghidra', 'nucleus', 'xda']:
    print('Invalid tool: %s' % args.tool)
    sys.exit(-1)

  for pkg in args.package:
    if pkg not in ['coreutils', 'binutils', 'spec']:
      print('Invalid package: %s' % pkg)
      sys.exit(-1)

  for arch in args.arch:
    if args.tool == 'binaryninja':
      if arch not in ['x86', 'x64', 'arm', 'aarch64', 'mips']:
        print('Invalid architecture: %s' % arch)
        sys.exit(-1)
    elif args.tool == 'nucleus':
      if arch not in ['x86', 'x64', 'aarch64']:
        print('Invalid architecture: %s' % arch)
        sys.exit(-1)
    elif args.tool == 'xda':
      if arch not in ['x86', 'x64']:
        print('Invalid architecture: %s' % arch)
        sys.exit(-1)
    else:
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

def conf_to_str(conf):
    return '%s-%s-%s-%s-%s' % conf

# -------------------------------->%

def spawn_container(image, name, dirpath):
  cmd = ''
  cmd += 'docker run'
  #cmd += ' --cpus=1'
  cmd += ' --ipc=host --gpus all'
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

def run_tool(args, conf, name, timeout):
  if args.tool == 'xda':
    cur = multiprocessing.current_process()
    pid = cur.__dict__['_identity'][0]
    assert (pid > 0)

    pkg, arch, comp, pie, opt = conf

    cmd = ''
    cmd += 'python3 /scripts/run_binaries.py'
    cmd += ' %s' % pkg
    cmd += ' %s' % arch
    cmd += ' %s' % comp
    cmd += ' %s' % pie
    cmd += ' %s' % opt
    cmd += ' %s' % timeout
    cmd += ' %d' % (pid - 1)

    run_cmd_in_docker(name, cmd)
  elif args.tool == 'funprobe':
    pkg, arch, comp, pie, opt = conf

    cmd = ''
    cmd += 'python3 /scripts/run_binaries.py'
    cmd += ' bdp'
    cmd += ' %s' % pkg
    cmd += ' %s' % arch
    cmd += ' %s' % comp
    cmd += ' %s' % pie
    cmd += ' %s' % opt
    cmd += ' %s' % timeout

    run_cmd_in_docker(name, cmd)
  elif args.tool == 'funprobe-lbp':
    pkg, arch, comp, pie, opt = conf

    cmd = ''
    cmd += 'python3 /scripts/run_binaries.py'
    cmd += ' lbp'
    cmd += ' %s' % pkg
    cmd += ' %s' % arch
    cmd += ' %s' % comp
    cmd += ' %s' % pie
    cmd += ' %s' % opt
    cmd += ' %s' % timeout

    run_cmd_in_docker(name, cmd)
  else:
    pkg, arch, comp, pie, opt = conf

    cmd = ''
    cmd += 'python3 /scripts/run_binaries.py'
    cmd += ' %s' % pkg
    cmd += ' %s' % arch
    cmd += ' %s' % comp
    cmd += ' %s' % pie
    cmd += ' %s' % opt
    cmd += ' %s' % timeout

    run_cmd_in_docker(name, cmd)

def run(arg):
  args, conf = arg
  conf_str = conf_to_str(conf)
  if args.tool == 'funprobe-lbp':
    image = 'funprobe-funprobe'
  else:
    image = 'funprobe-%s' % args.tool
  spawn_container(image, conf_str, args.datadir)
  run_tool(args, conf, conf_str, args.timeout)
  kill_container(conf_str)

# -------------------------------->%

def main(args):
  confs = enumerate_confs(args)
  pool = Pool(args.ncores)
  pool.map(run, confs)

# -------------------------------->%

if __name__ == '__main__':
  args = parse_args()
  check_args(args)
  main(args)
