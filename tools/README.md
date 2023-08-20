# Existing function identification tools

This document describes how to run the existing function identification tools
on our benchmark to evaluate their performance and compare them with
`FunProbe`.

We provide Docker containers for all tools to conveniently evaluate them,
except `IDA pro`. `IDA pro` needs a Windows VM. To fairly compare `XDA` with
`FunProbe`, we finetuned the model with our dataset. Therefore, you need to
properly locate the model-related files to build the Docker container of XDA.
For the commercial binary analysis tools, Binary Ninja and IDA pro, you have to
prepare your own license to evaluate them.

The below table summarizes the characteristics of each comparison target:
| Tool Name    | Platform | Need Additional Files | Need License       |
| ------------ | -------- | --------------------- | ------------------ |
| Ghidra       | Linux    | :x:                   | :x:                |
| Nucleus      | Linux    | :x:                   | :x:                |
| XDA          | Linux    | :heavy_check_mark:    | :x:                |
| Binary Ninja | Linux    | :x:                   | :heavy_check_mark: |
| IDA pro      | Windows  | :x:                   | :heavy_check_mark: |

Below are the detailed instructions to establish the evaluation environment and
how to run them.


## Building Docker images of Ghidra and Nucleus

To build a Docker image of `Ghidra` or `Nucleus`, try
```
user@ubuntu:~/FunProbe/tools/ghidra$ ./build.sh
```
or
```
user@ubuntu:~/FunProbe/tools/nucleus$ ./build.sh
```


## Building Docker image of Binary Ninja

You need to prepare the following two files to install `Binary Ninja` in the
Docker image:
- BinaryNinja.zip
- license.txt

Put these files under `tools/binaryninja/packages/` directory and try to
execute the following command:
```
user@ubuntu:~/FunProbe/tools/binaryninja$ ./build.sh
```


## Building Docker image of XDA

### Fine-tuning the model

We fine-tuned our own model using our training dataset atop the pre-trained
model of `XDA`. To randomly select 20% of x86 and x86-64 binaries in our
benchmark, run:
```
user@ubuntu:~/FunProbe/tools/xda/model/select.sh <data directory>
```
This command will generate several files under `<data directory>`. Follow the
instructions at [XDA repository](https://github.com/CUMLSec/XDA) with
`train.data`, `train.label`, `valid.data`, and `valid.label`. When fine-tuning
is done, copy `checkpoint_best.pt`, `data-dict.txt`, and `label-dict.txt` under
`tools/xda/model`.

To reduce the time for finetuning, we provide our [finetuned
parameters](https://zenodo.org/record/8266657/files/model.zip?download=1).
Extract the zip file and put `checkpoint_best.pt`, `data-dict.txt`, and
`label-dict.txt` in `tools/xda/model`.

### Building Docker image

Execute the following command:
```
user@ubuntu:~/FunProbe/tools/xda$ ./build.sh
```


## Building Windows VM of IDA pro

In order to prepare a Windows VM for `IDA pro`, follow the below steps:

1. Prepare a Windows VM. You may download the VM from
   [here](https://developer.microsoft.com/en-us/windows/downloads/virtual-machines/).
2. Install `IDA pro` inside the VM. Make sure that `IDA pro` is installed at
   `C:\Program Files\IDA Pro 7.7\`. Note that the version may differ, and in
   that case, fix the version number in `tools/ida/scripts/run.bat` manually.
3. If you successfully installed IDA pro, then Python 3 will also be installed
   on your system. If not, install Python 3.
4. Copy `tools/ida/scripts/` directory under `C:\`.
5. Copy `tools/ida/run_all.bat` and `tools/ida/run_exp.py` somewhere, but both
   files should be placed in the same directory.
6. Prepare our benchmark inside the VM.


## Running the experiments

We provide `run_exp.py` (at `tools/ida/` for `IDA pro` and `tools/` for others)
to easily run our comparison targets on our benchmark. It has several command
line
options:
- `--datadir`: specifies a directory where the benchmark directory `bench` is
  placed at.
- `--tool`: specifies the name of the comparison target.
- `--package`, `--arch`, `--compiler`, `--pie`, `--optlevel`: specifies target
  benchmark configurations.
- `--ncores` (except for `IDA pro`): manages the number of processes that can
  be run simultaneously.
- `--timeout`: specifies the time limits (in the format of `timeout`).

### Running Ghidra, Nucleus, Binary Ninja, and XDA

To run `Ghidra`, `Nucleus`, `Binary Ninja`, and `XDA`, try to execute the
following command with the appropriate tool name:
```
user@ubuntu:~/FunProbe/tools$ ./run_[toolname].sh <data directory> <# CPU cores>
```

### Running IDA pro

To run `IDA pro`, open the `cmd` window inside the VM and run `run_all.bat [path to the data directory inside VM]`.
After the experiment is finished, copy `[path to the data directory inside VM]/results/ida`
to `[path to the data directory outside VM]/results/ida` for ease of
reporting results.


## Reporting the results

We provide `show_res.py` to easily report the experimental results. It has
several commandline options:
- `--datadir`, `--tool`, `--package`, `--arch`, `--compiler`, `--pie`,
  `--optlevel`: same as `run_exp.py`.
- `--timeout`: same as `run_exp.py`, but in seconds.

We also provide `show_[toolname].sh` scripts so that you don't need to care
about the command line options. To use them:
```
user@ubuntu:~/FunProbe/tools$ ./show_[toolname].sh <data directory>
```
