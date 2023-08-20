FROM ubuntu:18.04
MAINTAINER Soomin Kim <soomink@kaist.ac.kr>

ARG NPROC=1

# Install packages from apt
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt update
RUN apt install -y wget gcc g++ \
	gcc-8 g++-8 \
	gcc-8-i686-linux-gnu g++-8-i686-linux-gnu \
	gcc-8-arm-linux-gnueabi g++-8-arm-linux-gnueabi \
	gcc-8-arm-linux-gnueabihf g++-8-arm-linux-gnueabihf \
	gcc-8-aarch64-linux-gnu g++-8-aarch64-linux-gnu \
	gcc-8-mipsel-linux-gnu g++-8-mipsel-linux-gnu \
	gcc-8-mips64el-linux-gnuabi64 g++-8-mips64el-linux-gnuabi64 \
	make xz-utils p7zip-full vim gettext file git cmake python3 libssl-dev

# Build & Install dependencies (CMake & LLVM)
ADD deps /deps
RUN /deps/install_cmake.sh $NPROC
RUN /deps/install_llvm.sh $NPROC

ARG HASSPEC=0

# Prepare packages; Put cpu2017.iso into packages/ directory if you have the benchmark
ADD packages /packages
ADD assets /assets
RUN /packages/prepare_packages.sh $HASSPEC
