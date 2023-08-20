import argparse, os, random, sys
from elftools.elf.elffile import ELFFile

def parse_args():
  p = argparse.ArgumentParser()
  p.add_argument('--datadir', required=True)
  p.add_argument('--seed', required=False, default=31337)
  p.add_argument('--package', nargs='+', required=True)
  p.add_argument('--arch', nargs='+', required=True)
  p.add_argument('--compiler', nargs='+', required=True)
  p.add_argument('--pie', nargs='+', required=True)
  p.add_argument('--optlevel', nargs='+', required=True)
  args = p.parse_args()

  return args

def check_args(args):
  for pkg in args.package:
    if pkg not in ['coreutils', 'binutils', 'spec']:
      print('Invalid package: %s' % pkg)
      sys.exit(-1)

  for arch in args.arch:
    if arch not in ['x86', 'x64']:
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

def select_binaries(conf):
  pkg, arch, comp, pie, opt = conf

  bench_dir = os.path.join(args.datadir, 'bench')
  bin_dir = os.path.join(bench_dir, 'bin', pkg, arch, comp, pie, opt)

  bin_names = os.listdir(bin_dir)

  # Select 20% of binaries for training & validation
  selected = random.choices(bin_names, k=int(len(bin_names)*0.2))

  # #train : #valid = 9 : 1
  train = selected[:int(len(selected)*0.9)]
  valid = selected[int(len(selected)*0.9):]
  test = list(set(bin_names) - set(selected))

  train = list(map(lambda name: (conf, name), train))
  valid = list(map(lambda name: (conf, name), valid))
  test = list(map(lambda name: (conf, name), test))

  return train, valid, test

# -------------------------------->%

def load_text_section(bin_path):
  elf = ELFFile(open(bin_path, 'rb'))
  text = elf.get_section_by_name('.text')
  text_addr = text['sh_addr']
  text_data =text.data()

  return text_addr, text_data

def load_gt(gt_path):
  gt = set()
  with open(gt_path) as f:
    for line in f.readlines():
      addr = int(line.strip(), 16)
      gt.add(addr)

  return gt

def dump_dataset(data, list_path, data_path, label_path):
  with open(list_path, 'w') as f:
    for conf, name in data:
      pkg, arch, comp, pie, opt = conf
      f.write('%s,%s,%s,%s,%s,%s\n' % (pkg, arch, comp, pie, opt, name))

  data_lines = []
  label_lines = []
  for conf, name in data:
    pkg, arch, comp, pie, opt = conf

    bench_dir = os.path.join(args.datadir, 'bench')
    bin_path = os.path.join(bench_dir, 'bin', pkg, arch, comp, pie, opt, name)
    gt_path = os.path.join(bench_dir, 'gt', pkg, arch, comp, pie, opt, name)

    text_addr, text_data = load_text_section(bin_path)
    gt = load_gt(gt_path)

    nblks = len(text_data) // 512
    for i in range(nblks):
      data = []
      label = []
      for j in range(512):
        d = text_data[512*i+j]
        data.append('%02x' % d)
        addr = text_addr + 512*i + j
        if addr in gt:
          l = 'S'
        else:
          l = 'N'
        label.append(l)

      data_line = ' '.join(data)
      data_lines.append(data_line)

      label_line = ' '.join(label)
      label_lines.append(label_line)

    # Padding for un-alined data
    ndata = len(text_data) % 512
    if ndata != 0:
      data = []
      label = []
      for j in range(ndata):
        d = text_data[512*nblks+j]
        data.append('%02x' % d)
        addr = text_addr + 512*i + j
        if addr in gt:
          l = 'S'
        else:
          l = 'N'
        label.append(l)

      for j in range(ndata, 512):
        data.append('00')
        label.append('N')

      data_line = ' '.join(data)
      data_lines.append(data_line)

      label_line = ' '.join(label)
      label_lines.append(label_line)

  with open(data_path, 'w') as f:
    for line in data_lines:
      f.write('%s\n' % line)

  with open(label_path, 'w') as f:
    for line in label_lines:
      f.write('%s\n' % line)

# -------------------------------->%

def main(args):
  # Control random
  random.seed(args.seed)

  confs = enumerate_confs(args)

  trains = []
  valids = []
  tests = []
  for conf in confs:
    train, valid, test = select_binaries(conf)
    trains += train
    valids += valid
    tests += test

  train_list_path = os.path.join(args.datadir, 'train.list')
  train_data_path = os.path.join(args.datadir, 'train.data')
  train_label_path = os.path.join(args.datadir, 'train.label')
  dump_dataset(trains, train_list_path, train_data_path, train_label_path)

  valid_list_path = os.path.join(args.datadir, 'valid.list')
  valid_data_path = os.path.join(args.datadir, 'valid.data')
  valid_label_path = os.path.join(args.datadir, 'valid.label')
  dump_dataset(valids, valid_list_path, valid_data_path, valid_label_path)

  test_list_path = os.path.join(args.datadir, 'test.list')
  test_data_path = os.path.join(args.datadir, 'test.data')
  test_label_path = os.path.join(args.datadir, 'test.label')
  dump_dataset(tests, test_list_path, test_data_path, test_label_path)

# -------------------------------->%

if __name__ == '__main__':
  args = parse_args()
  check_args(args)
  main(args)
