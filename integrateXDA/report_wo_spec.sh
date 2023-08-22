#!/bin/bash

# arg1: data directory

python3 report.py --datadir $1 --package coreutils binutils --arch x86 x64 --compiler gcc clang --pie pie nopie --optlevel o0 o1 o2 o3 os ofast
