# Benchmarks

This document explains the details of `FunProbe` benchmark dataset. To the
best of our knowledge, this is the largest benchmark used to evaluate function
identification algorithms.


## Dependencies

To build our benchmark, we have the following software dependencies:
```
- Python3
- Docker
- binutils
```
We have tested the benchmark building script with Python 3.10.12 and Docker
20.10.21, but other versions should work.


## SPEC CPU2017

Our benchmark includes SPEC CPU2017, whose license should be purchased. If you
have your own license, put your iso under `FunProbe/benchmark/packages/`
directory by renaming it to `cpu2017.iso`. We have used CPU2017 1.1.5 in our
benchmark, so different versions may be failed to work. Please create an issue
in that case. Nevertheless, we provide two scripts to build benchmarks with and
without SPEC.


## Downloading the benchmark

We provide a
[link](https://zenodo.org/record/8266657/files/bench.zip?download=1) to
download our benchmark to reduce the cost of building.  Note that the link does
not include SPEC benchmark, for the license issue. You need to build SPEC
benchmark binaries on your own if you want to test `FunProbe` on them.


## Building the benchmark

### Docker

We provide the benchmark building environment as a Docker image. To build the
Docker image,
```
user@ubuntu:~/FunProbe/benchmark$ ./build_image.sh <# CPU cores> # If you have your own SPEC benchmark
```
or
```
user@ubuntu:~/FunProbe/benchmark$ ./build_image_wo_spec.sh <# CPU cores> # If you do not have SPEC benchmark
```

`<# CPU cores>` argument specifies the number of CPU cores for building the
Docker image. We recommend giving CPU cores as many as possible to boost the
image creation, because the creation process includes building LLVM.

### Build script

The usage of our benchmark building script is as follows:
```
user@ubuntu:~/FunProbe/benchmark$ python3 build_benchmark.py --datadir DATADIR [--image IMAGE] --package PACKAGE [PACKAGE ...] --arch ARCH [ARCH ...] --compiler COMPILER [COMPILER ...] --pie PIE [PIE ...] --optlevel OPTLEVEL [OPTLEVEL ...] [--ncores NCORES]
```
`--datadir` switch specifies the data directory where the benchmark will be
stored. `--image` switch is an optional switch to specify the Docker image. If
you build the image using `./build_image.sh`, then you can omit this switch.
`--ncores` switch can increase the number of CPU cores.  The remaining switches
are used to build a specific subset of benchmarks. For example, if you want to
build a set of `Position Independent Executable` binaries on `AArch64` from
`GNU Coreutils` compiled by `clang` with `O3` optimization, an example command
will be:
```
user@ubuntu:~/FunProbe/benchmark$ python3 build_benchmark.py --datadir /tmp/data --package coreutils --arch aarch64 --compiler clang --pie pie --optlevel o3
```

We provide ready-made scripts to build the benchmark.
```
user@ubuntu:~/FunProbe/benchmark$ ./build_all.sh <data direcotry> <# CPU cores> # If you have your own SPEC benchmark
```
or
```
user@ubuntu:~/FunProbe/benchmark$ ./build_all_wo_spec.sh <data direcotry> <# CPU cores> # If you do not have SPEC benchmark
```

When building SPEC benchmark, Docker containers occupy large disk spaces. So we
recommend not to build SPEC benchmarks at a time.

## Benchmark directory structure

After a whole (or a subset of) benchmark is built, you will find the `bench`
directory at the data directory you specified. The `bench` directory has the
following directory structure.
```
path-to-data-dir            (an example data directory)
`+-bench                     (benchmark directory)
 | `+-bin                     (binaries with symbol information)
 |  | `+-...
 |  +-stripbin                (binaries without symbol information)
 |  | `+-...
 |  +-gt                      (ground truth data)
 |    `+-...
 |  +-mips_got                (GOT section information only for MIPS/MIPS64 binaries)
 |    `+-...
 +-...
```
