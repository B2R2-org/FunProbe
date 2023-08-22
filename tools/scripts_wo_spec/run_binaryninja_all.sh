#!/bin/bash

# arg1: data directory
# arg2: # cores

python3 run_exp.py --datadir $1 --tool binaryninja --package coreutils binutils --arch x86 x64 arm aarch64 mips --compiler gcc clang --pie pie nopie --optlevel o0 o1 o2 o3 os ofast --ncore $2
