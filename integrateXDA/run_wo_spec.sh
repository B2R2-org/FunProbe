#!/bin/bash

# arg1: data directory
# arg2: # cores

python3 run_exp.py --datadir $1 --package coreutils binutils --arch x86 x64 --compiler gcc clang --pie pie nopie --optlevel o0 o1 o2 o3 os ofast --ncore $2
