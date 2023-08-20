import argparse, os, random, sys

def parse_args():
  p = argparse.ArgumentParser()
  p.add_argument('--datadir', required=True)
  p.add_argument('--package', nargs='+', required=True)
  p.add_argument('--arch', nargs='+', required=True)
  p.add_argument('--compiler', nargs='+', required=True)
  p.add_argument('--pie', nargs='+', required=True)
  p.add_argument('--optlevel', nargs='+', required=True)
  p.add_argument('--seed', required=False, type=int, default=31337)
  p.add_argument('--nbins', required=False, type=int, default=10)
  args = p.parse_args()

  return args

def check_args(args):
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
            confs.append(conf)

  return confs

# -------------------------------->%

def pick_binaries(args, conf):
  pkg, arch, comp, pie, opt = conf

  bin_dir = os.path.join(args.datadir, 'bench', 'bin', pkg, arch, comp, pie, opt)
  bin_names = os.listdir(bin_dir)

  bin_names = random.choices(bin_names, k=args.nbins)
  bench = list(map(lambda name: (conf, name), bin_names))

  return bench

def choose_benchmark(args, confs):
  random.seed(args.seed)

  bench = []
  for conf in confs:
    bench += pick_binaries(args, conf)

  with open('./binlist.txt', 'w') as f:
    for conf, name in bench:
      f.write('%s,%s,%s,%s,%s,%s\n' % (*conf, name))

# -------------------------------->%

def main(args):
  confs = enumerate_confs(args)
  choose_benchmark(args, confs)

# -------------------------------->%

if __name__ == '__main__':
  args = parse_args()
  check_args(args)
  main(args)
