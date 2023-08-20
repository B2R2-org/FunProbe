import argparse, os, subprocess, string, sys

from elftools.elf.elffile import ELFFile

def parse_args():
  p = argparse.ArgumentParser()
  p.add_argument('--datadir', required=True)
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

def is_valid_res(path):
  if not os.path.exists(path):
    return False

  if os.path.getsize(path) == 0:
    return False

  return True

# Out-of-scope
def load_oos_ranges(bin_path):
  ranges = []

  elf = ELFFile(open(bin_path, 'rb'))

  plt = elf.get_section_by_name('.plt')
  if plt is not None:
    saddr = plt['sh_addr']
    eaddr = saddr + plt['sh_size']
    ranges.append((saddr, eaddr))

  pltsec = elf.get_section_by_name('.plt.sec')
  if pltsec is not None:
    saddr = pltsec['sh_addr']
    eaddr = saddr + pltsec['sh_size']
    ranges.append((saddr, eaddr))

  init = elf.get_section_by_name('.init')
  if init is not None:
    saddr = init['sh_addr']
    eaddr = saddr + init['sh_size']
    ranges.append((saddr, eaddr))

  fini = elf.get_section_by_name('.fini')
  if fini is not None:
    saddr = fini['sh_addr']
    eaddr = saddr + fini['sh_size']
    ranges.append((saddr, eaddr))

  return ranges

def is_out_of_scope(oos_ranges, addr):
  for saddr, eaddr in oos_ranges:
    if saddr <= addr and addr < eaddr:
      return True

  return False

def load_result(args, conf, bin_path, res_path):
  oos_ranges = load_oos_ranges(bin_path)

  result = set()
  with open(res_path) as f:
    for line in f.readlines():
      line = line.strip()

      has_error = not set(line[2:]).issubset(string.hexdigits)
      if has_error:
        break

      addr = int(line, 16)

      is_oos = is_out_of_scope(oos_ranges, addr)
      if is_oos:
        continue

      result.add(addr)

  if has_error:
    return set(), True
  else:
    return result, False

def load_gt(gt_path):
  gt = set()
  with open(gt_path) as f:
    for line in f.readlines():
      gt.add(int(line.strip(), 16))

  return gt

def accumulate_result(args, conf, accuracy):
  pkg, arch, comp, pie, opt = conf
  bin_dir = os.path.join(args.datadir, 'bench', 'bin', pkg, arch, comp, pie, opt)
  gt_dir = os.path.join(args.datadir, 'bench', 'gt', pkg, arch, comp, pie, opt)
  res_dir = os.path.join(args.datadir, 'results', 'funprobe-xda', pkg, arch, comp, pie, opt)

  if arch not in accuracy:
    accuracy[arch] = {}
  if comp not in accuracy[arch]:
    accuracy[arch][comp] = [0, 0, 0, 0, 0] # nbin, tp, fp, fn, failed

  target_list = []
  with open(os.path.join(args.datadir, 'test.list')) as f:
    for line in f.readlines():
      pkg_, arch_, comp_, pie_, opt_, name = line.strip().split(',')
      if (pkg, arch, comp, pie, opt) == (pkg_, arch_, comp_, pie_, opt_):
        target_list.append(name)

  for name in os.listdir(bin_dir):
    if name not in target_list:
      continue
    bin_path = os.path.join(bin_dir, name)
    gt_path = os.path.join(gt_dir, name)
    res_path = os.path.join(res_dir, name)

    is_invalid = False

    if not is_valid_res(res_path):
      is_invalid = True

    if not is_invalid:
      result, has_error = load_result(args, conf, bin_path, res_path)

      if has_error:
        accuracy[arch][comp][4] += 1
      else:
        gt = load_gt(gt_path)

        tp = len(result & gt)
        fp = len(result - gt)
        fn = len(gt - result)

        accuracy[arch][comp][0] += 1
        accuracy[arch][comp][1] += tp
        accuracy[arch][comp][2] += fp
        accuracy[arch][comp][3] += fn
    else:
      accuracy[arch][comp][4] += 1

def show_summary(args, confs):
  accuracy = {}
  for conf in confs:
    accumulate_result(args, conf, accuracy)

  total_bins = 0
  total_tp = 0
  total_fp = 0
  total_fn = 0

  for arch in args.arch:
    for comp in args.compiler:
      nbins, tp, fp, fn, _ = accuracy[arch][comp]

      total_bins += nbins
      total_tp += tp
      total_fp += fp
      total_fn += fn

      prec = tp / (tp + fp)
      rec = tp / (tp + fn)
      f1 = 1 / (((1 / prec) + (1 / rec)) / 2)

      prec = round(prec * 100, 2)
      rec = round(rec * 100, 2)
      f1 = round(f1 * 100, 2)

      print('%s - %s: %.3f %.3f %.3f' % (arch, comp, prec, rec, f1))

  total_prec = total_tp / (total_tp + total_fp)
  total_rec = total_tp / (total_tp + total_fn)
  total_f1 = 1 / (((1 / total_prec) + (1 / total_rec)) / 2)

  total_prec = round(total_prec * 100, 2)
  total_rec = round(total_rec * 100, 2)
  total_f1 = round(total_f1 * 100, 2)

  print('Total: %.3f %.3f %.3f' % (total_prec, total_rec, total_f1))

# -------------------------------->%

def main(args):
  confs = enumerate_confs(args)
  show_summary(args, confs)

# -------------------------------->%

if __name__ == '__main__':
  args = parse_args()
  check_args(args)
  main(args)
