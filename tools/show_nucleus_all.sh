#!/bin/bash

# arg1: data directory

python3 show_res.py --datadir $1 --tool nucleus --package coreutils binutils spec --arch x86 x64 aarch64 --compiler gcc clang --pie pie nopie --optlevel o0 o1 o2 o3 os ofast --timeout 3600
