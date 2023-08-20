#!/bin/bash

# Arg1: architecture (x86, x64, arm, aarch64, mips, mips64)
# Arg2: compiler kind (gcc, clang)
# Arg3: pie option (pie, nopie)
# Arg4: code optimization option (o0, o1, o2, o3, ofast, os)
# Arg5: output directory

# Ensure no errors
set -e

ARCHOPT=$1
COMPOPT=$2
PIEOPT=$3
OPTOPT=$4
OUTDIR=$5

CONFPATH=/packages/cpu2017/config/cpu2017.cfg

BUILDNAME=$ARCHOPT-$COMPOPT-$PIEOPT-$OPTOPT

BUILDDIR=$OUTDIR/spec-$BUILDNAME
BINDIR=$BUILDDIR/bin
STRIPBINDIR=$BUILDDIR/stripbin

FLAGS="-ggdb"

SRCDIR=/packages/cpu2017

if [[ $ARCHOPT == "x86" ]]; then
  BUILDTRIPLE=i686-linux-gnu
  if [[ $COMPOPT == "clang" ]]; then
    FLAGS="$FLAGS --target=$BUILDTRIPLE"
  fi
  STRIPBIN=$BUILDTRIPLE-strip
elif [[ $ARCHOPT == "x64" ]]; then
  BUILDTRIPLE=
  STRIPBIN=strip
elif [[ $ARCHOPT == "arm" ]]; then
  BUILDTRIPLE=arm-linux-gnueabi
  FLAGS="$FLAGS -march=armv7-a"
  if [[ $COMPOPT == "clang" ]]; then
    FLAGS="$FLAGS --target=$BUILDTRIPLE -lm"
  fi
  STRIPBIN=$BUILDTRIPLE-strip
elif [[ $ARCHOPT == "armhf" ]]; then
  BUILDTRIPLE=arm-linux-gnueabihf
  FLAGS="$FLAGS -march=armv7-a"
  if [[ $COMPOPT == "clang" ]]; then
    FLAGS="$FLAGS --target=$BUILDTRIPLE -lm"
  fi
  STRIPBIN=$BUILDTRIPLE-strip
elif [[ $ARCHOPT == "aarch64" ]]; then
  BUILDTRIPLE=aarch64-linux-gnu
  if [[ $COMPOPT == "clang" ]]; then
    FLAGS="$FLAGS --target=$BUILDTRIPLE"
  fi
  STRIPBIN=$BUILDTRIPLE-strip
elif [[ $ARCHOPT == "mips" ]]; then
  BUILDTRIPLE=mipsel-linux-gnu
  if [[ $COMPOPT == "clang" ]]; then
    FLAGS="$FLAGS --target=$BUILDTRIPLE"
  fi
  STRIPBIN=$BUILDTRIPLE-strip
elif [[ $ARCHOPT == "mips64" ]]; then
  BUILDTRIPLE=mips64el-linux-gnuabi64
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
  CCOMP=clang
  CXXCOMP=clang++
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

sed -i "s/FUNPROBELBL/$BUILDNAME/g" $CONFPATH
sed -i "s/FUNPROBECC/$CCOMP/g" $CONFPATH
sed -i "s/FUNPROBECXX/$CXXCOMP/g" $CONFPATH
sed -i "s/FUNPROBEFLAGS/$FLAGS/g" $CONFPATH

cd $SRCDIR
source $SRCDIR/shrc

runcpu -a build --config=cpu2017.cfg 500.perlbench_r 502.gcc_r 505.mcf_r 520.omnetpp_r 523.xalancbmk_r 525.x264_r 531.deepsjeng_r 541.leela_r 557.xz_r 508.namd_r 510.parest_r 511.povray_r 519.lbm_r 526.blender_r 538.imagick_r 544.nab_r

mkdir -p $BUILDDIR
mkdir -p $BINDIR

while IFS=, read -r tag name || [[ -n "$line" ]]; do
  BINPATH=$SRCDIR/benchspec/CPU/$tag/exe/$name\_base.$BUILDNAME-m64
  ASMDIR=$SRCDIR/benchspec/CPU/$tag/build/build_base_$BUILDNAME-m64.0000
  cp $BINPATH $BINDIR/$tag
done < /assets/cpu-2017_list.txt

cp -r $BINDIR $STRIPBINDIR
$STRIPBIN $STRIPBINDIR/*

touch $BUILDDIR/.success
