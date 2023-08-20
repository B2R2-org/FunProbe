#!/bin/bash

cd /packages
wget -O /packages/coreutils-9.0.tar.xz https://ftp.gnu.org/gnu/coreutils/coreutils-9.0.tar.xz
wget -O /packages/binutils-2.37.tar.xz https://ftp.gnu.org/gnu/binutils/binutils-2.37.tar.xz
tar -xf coreutils-9.0.tar.xz
tar -xf binutils-2.37.tar.xz

if [[ $1 == 1 ]]; then # Prepare spec
  7z x -o/packages/cpu2017 /packages/cpu2017.iso
  chmod -R +x /packages/cpu2017
  cd /packages/cpu2017
  ./install.sh -f
  cp /assets/cpu2017.cfg /packages/cpu2017/config/cpu2017.cfg
fi
