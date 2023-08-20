import subprocess, sys

def get_proc_output(cmd):
  cmd = cmd.split()
  proc = subprocess.Popen(cmd, stdout=subprocess.PIPE)
  out, _ = proc.communicate()
  out = out.decode()
  lines = out.split('\n')

  return lines

# -------------------------------->%

def main(bin_path, res_path):
  cmd = 'readelf -a %s' % bin_path
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

  with open(res_path, 'w') as f:
    for addr in func_map:
      f.write('%x %s\n' % (addr, func_map[addr]))

# -------------------------------->%

if __name__ == '__main__':
  bin_path = sys.argv[1]
  res_path = sys.argv[2]
  main(bin_path, res_path)
