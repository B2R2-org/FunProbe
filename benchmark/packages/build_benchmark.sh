#!/bin/bash

#!/bin/bash

# Arg1: source package (coreutils, binutils)
# Arg2: architecture (x86, x64, arm, aarch64, mips, mips64)
# Arg3: compiler kind (gcc, clang)
# Arg4: pie option (pie, nopie)
# Arg5: code optimization option (o0, o1, o2, o3, ofast, os)
# Arg6: output directory

# Ensure no errors
set -e

PKGOPT=$1
ARCHOPT=$2
COMPOPT=$3
PIEOPT=$4
OPTOPT=$5
OUTDIR=$6

BUILDDIR=$OUTDIR/$PKGOPT-$ARCHOPT-$COMPOPT-$PIEOPT-$OPTOPT
BINDIR=$BUILDDIR/bin
STRIPBINDIR=$BUILDDIR/stripbin

CONFIGOPT=""
FLAGS="-ggdb"

if [[ $PKGOPT == "coreutils" ]]; then
  SRCDIR=/packages/coreutils-9.0
  CONFIGOPT="$CONFIGOPT FORCE_UNSAFE_CONFIGURE=1 --enable-no-install-program=stdbuf"
  LISTFILE=/assets/coreutils-9.0_list.txt
elif [[ $PKGOPT == "binutils" ]]; then
  SRCDIR=/packages/binutils-2.37
  CONFIGOPT="$CONFIGOPT --disable-plugins --disable-multilib --disable-shared --enable-static"
  LISTFILE=/assets/binutils-2.37_list.txt
fi

if [[ $ARCHOPT == "x86" ]]; then
  BUILDTRIPLE=i686-linux-gnu
  CONFIGOPT="$CONFIGOPT --host=$BUILDTRIPLE --target=$BUILDTRIPLE"
  if [[ $COMPOPT == "clang" ]]; then
    FLAGS="$FLAGS --target=$BUILDTRIPLE"
  fi
  STRIPBIN=$BUILDTRIPLE-strip
elif [[ $ARCHOPT == "x64" ]]; then
  BUILDTRIPLE=
  CONFIGOPT="$CONFIGOPT"
  STRIPBIN=strip
elif [[ $ARCHOPT == "arm" ]]; then
  BUILDTRIPLE=arm-linux-gnueabi
  CONFIGOPT="$CONFIGOPT --host=$BUILDTRIPLE --target=$BUILDTRIPLE"
  FLAGS="$FLAGS -march=armv7-a"
  if [[ $COMPOPT == "clang" ]]; then
    FLAGS="$FLAGS --target=$BUILDTRIPLE -lm"
  fi
  STRIPBIN=$BUILDTRIPLE-strip
elif [[ $ARCHOPT == "armhf" ]]; then
  BUILDTRIPLE=arm-linux-gnueabihf
  CONFIGOPT="$CONFIGOPT --host=$BUILDTRIPLE --target=$BUILDTRIPLE"
  FLAGS="$FLAGS -march=armv7-a"
  if [[ $COMPOPT == "clang" ]]; then
    FLAGS="$FLAGS --target=$BUILDTRIPLE -lm"
  fi
  STRIPBIN=$BUILDTRIPLE-strip
elif [[ $ARCHOPT == "aarch64" ]]; then
  BUILDTRIPLE=aarch64-linux-gnu
  CONFIGOPT="$CONFIGOPT --host=$BUILDTRIPLE --target=$BUILDTRIPLE"
  if [[ $COMPOPT == "clang" ]]; then
    FLAGS="$FLAGS --target=$BUILDTRIPLE"
  fi
  STRIPBIN=$BUILDTRIPLE-strip
elif [[ $ARCHOPT == "mips" ]]; then
  BUILDTRIPLE=mipsel-linux-gnu
  CONFIGOPT="$CONFIGOPT --host=$BUILDTRIPLE --target=$BUILDTRIPLE"
  if [[ $COMPOPT == "clang" ]]; then
    FLAGS="$FLAGS --target=$BUILDTRIPLE"
  fi
  STRIPBIN=$BUILDTRIPLE-strip
elif [[ $ARCHOPT == "mips64" ]]; then
  BUILDTRIPLE=mips64el-linux-gnuabi64
  CONFIGOPT="$CONFIGOPT --host=$BUILDTRIPLE --target=$BUILDTRIPLE"
  if [[ $COMPOPT == "clang" ]]; then
    FLAGS="$FLAGS --target=$BUILDTRIPLE"
  fi
  STRIPBIN=$BUILDTRIPLE-strip
fi

if [[ $COMPOPT == "gcc" ]]; then
  if [[ $ARCHOPT == "x64" ]]; then
    CCOMP=gcc-8
    CXXCOMP=g++-8
  else
    CCOMP=$BUILDTRIPLE-gcc-8
    CXXCOMP=$BUILDTRIPLE-g++-8
  fi
elif [[ $COMPOPT == "clang" ]]; then
  CCOMP=clang-13
  CXXCOMP=clang++-13
fi

if [[ $PIEOPT == "pie" ]]; then
  FLAGS="$FLAGS -pie -fPIE"
elif [[ $PIEOPT == "nopie" ]]; then
  FLAGS="$FLAGS -no-pie -fno-PIC"
fi

if [[ $OPTOPT == "o0" ]]; then
  FLAGS="$FLAGS -O0"
elif [[ $OPTOPT == "o1" ]]; then
  FLAGS="$FLAGS -O1"
elif [[ $OPTOPT == "o2" ]]; then
  FLAGS="$FLAGS -O2"
elif [[ $OPTOPT == "o3" ]]; then
  FLAGS="$FLAGS -O3"
elif [[ $OPTOPT == "os" ]]; then
  FLAGS="$FLAGS -Os"
elif [[ $OPTOPT == "ofast" ]]; then
  FLAGS="$FLAGS -Ofast"
fi


mkdir -p $BUILDDIR
mkdir -p $BINDIR
mkdir -p $STRIPBINDIR
cd $BUILDDIR
echo CC=$CCOMP CXX=$CXXCOMP CFLAGS=$FLAGS CXXFLAGS=$FLAGS $SRCDIR/configure $CONFIGOPT
CC=$CCOMP CXX=$CXXCOMP CFLAGS=$FLAGS CXXFLAGS=$FLAGS $SRCDIR/configure $CONFIGOPT
make

while IFS='' read -r line || [[ -n "$line" ]]; do
  RELPATH=$(echo $line | awk -F' ' '{print $1}')
  cp $BUILDDIR/$RELPATH $BINDIR/
  cp $BUILDDIR/$RELPATH $STRIPBINDIR/
done < $LISTFILE

$STRIPBIN $STRIPBINDIR/*

touch $BUILDDIR/.success
