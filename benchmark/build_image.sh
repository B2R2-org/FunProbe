#!/bin/bash

# ARG1 : nproc

docker build --build-arg NPROC=$1 --build-arg HASSPEC=1 -t funprobe-benchmark .
