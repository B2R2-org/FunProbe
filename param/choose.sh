#!/bin/bash

# arg1: data directory

python3 choose.py --datadir $1 --package coreutils --arch x86 x64 arm aarch64 --compiler gcc clang --pie pie nopie --optlevel o0 o1 o2 o3 os ofast
