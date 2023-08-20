#!/bin/bash

set -e

cd /deps
git clone https://github.com/llvm/llvm-project.git
cd llvm-project
git checkout llvmorg-13.0.1
cmake -S llvm -B build -G "Unix Makefiles" -DLLVM_ENABLE_PROJECTS="clang" -DCMAKE_BUILD_TYPE=Release
cd build
make -j$1
make install
