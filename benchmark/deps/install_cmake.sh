#!/bin/bash

set -e

cd /deps
wget -O /deps/cmake-3.20.0.tar.gz https://github.com/Kitware/CMake/releases/download/v3.20.0/cmake-3.20.0.tar.gz
tar -zvxf cmake-3.20.0.tar.gz
cd cmake-3.20.0
./bootstrap
make -j$1
make install
