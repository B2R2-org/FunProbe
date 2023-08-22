# Requirements

## Hardware Requirements

### GPU

`FunProbe` does not need any GPUs. However, our script to run `XDA` requires a
single GPU core. Also, if you want to finetune `XDA` by yourself, you will need
a couple of GPU cores.

### Disk space

For the partial benchmark, one needs 20GB of empty disk space. 30GB+ of empty
disk space will additionally be needed to build the partial benchmark. For the
full benchmark building, it will cost around 200GB of empty disk space.

## Software Requirements

As we specified in
[README.md](https://github.com/B2R2-org/FunProbe/blob/fse23-ae/README.md),
installing `FunProbe` on the naive system requires the
following software dependencies:

- .NET 6.0
- Python3 (with `pyelftools` package)
- GNU Binutils

To build Docker environments, one additionally needs

- Docker

The above dependencies will be sufficient to run all components of this
artifact.

### Proprietary Softwares

To build the full benchmark and all of the experiments, one needs

- SPEC CPU2017 (v1.1.5)
- IDA pro (v7.7)
- Binary Ninja (v3.0.3233)

The versions in the parentheses specify the versions used in the associated
paper.
