#!/bin/bash

# arg1: data directory
# arg2: # cores

python3 build_benchmark.py --datadir $1 --package coreutils binutils spec --arch x86 x64 arm aarch64 mips mips64 --compiler gcc clang --pie pie nopie --optlevel o0 o1 o2 o3 os ofast --ncore $2
