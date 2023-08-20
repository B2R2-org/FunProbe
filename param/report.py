import argparse, os, subprocess, string, sys

from elftools.elf.elffile import ELFFile

def parse_args():
  p = argparse.ArgumentParser()
  p.add_argument('--datadir', required=True)
  args = p.parse_args()

  return args

# -------------------------------->%

POSITIVES = [0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95]
NEGATIVES = [0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45]

def enumerate_params(args):
  params = []

  for pos in POSITIVES:
    for neg in NEGATIVES:
      params.append((pos, neg))

  return params

def load_binlist(args):
  list_path = os.path.join(args.datadir, 'binlist.txt')

  bin_list = []
  with open(list_path) as f:
    for line in f.readlines():
      bin_list.append(line.strip().split(','))

  return bin_list

# -------------------------------->%

def is_valid_res(path):
  if not os.path.exists(path):
    return False

  if os.path.getsize(path) == 0:
    return False

  return True

def load_time(time_path):
  with open(time_path) as f:
    return float(f.read().strip())

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
  has_error = False
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

def accumulate_result(args, pos, neg, b, accuracy):
  pkg, arch, comp, pie, opt, name = b
  conf = pkg, arch, comp, pie, opt
  bin_path = os.path.join(args.datadir, 'bench', 'bin', pkg, arch, comp, pie, opt, name)
  gt_path = os.path.join(args.datadir, 'bench', 'gt', pkg, arch, comp, pie, opt, name)
  res_path = os.path.join(args.datadir, 'results', 'param_%.2f_%.2f' % (pos, neg), pkg, arch, comp, pie, opt, name)

  if (pos, neg) not in accuracy:
    accuracy[(pos, neg)] = [0, 0, 0, 0] # nbin, tp, fp, fn

  result, has_error = load_result(args, conf, bin_path, res_path)
  assert not has_error

  gt = load_gt(gt_path)

  tp = len(result & gt)
  fp = len(result - gt)
  fn = len(gt - result)

  accuracy[(pos, neg)][0] += 1
  accuracy[(pos, neg)][1] += tp
  accuracy[(pos, neg)][2] += fp
  accuracy[(pos, neg)][3] += fn

def show_summary(args, params):
  accuracy = {}
  for pos, neg in params:
    bin_list = load_binlist(args)

    for b in bin_list:
      accumulate_result(args, pos, neg, b, accuracy)

  for pos, neg in params:
    nbins, tp, fp, fn = accuracy[(pos, neg)]

    prec = tp / (tp + fp)
    rec = tp / (tp + fn)
    f1 = 1 / (((1 / prec) + (1 / rec)) / 2)

    prec = round(prec * 100, 2)
    rec = round(rec * 100, 2)
    f1 = round(f1 * 100, 2)

    print('%.2f - %.2f: %.3f %.3f %.3f' % (pos, neg, prec, rec, f1))

# -------------------------------->%

def main(args):
  params = enumerate_params(args)
  show_summary(args, params)

# -------------------------------->%

if __name__ == '__main__':
  args = parse_args()
  main(args)
