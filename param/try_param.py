import argparse, subprocess, sys

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

# -------------------------------->%

def parse_args():
  p = argparse.ArgumentParser()
  p.add_argument('--datadir', required=True)
  p.add_argument('--ncores', required=False, type=int, default=1)
  args = p.parse_args()

  return args

# -------------------------------->%

POSITIVES = [0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95]
NEGATIVES = [0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45]

def enumerate_params(args):
  params = []

  for pos in POSITIVES:
    for neg in NEGATIVES:
      params.append((args, pos, neg))

  return params

# -------------------------------->%

def spawn_container(image, name, dirpath):
  cmd = ''
  cmd += 'docker run'
  #cmd += ' --cpus=1'
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

def run_funprobe(name, pos, neg):
  cmd = ''
  cmd += 'python3 /scripts/run_param.py'
  cmd += ' %.3f' % pos
  cmd += ' %.3f' % neg

  run_cmd_in_docker(name, cmd)

def run(arg):
  args, pos, neg = arg
  param_str = '%.2f_%.2f' % (pos, neg)
  image = 'funprobe-funprobe'
  spawn_container(image, param_str, args.datadir)
  run_funprobe(param_str, pos, neg)
  kill_container(param_str)

# -------------------------------->%

def main(args):
  params = enumerate_params(args)
  pool = Pool(args.ncores)
  pool.map(run, params)

# -------------------------------->%

if __name__ == '__main__':
  args = parse_args()
  main(args)
