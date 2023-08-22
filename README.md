# FunProbe

FunProbe is a function identification tool based on Bayesian Network. For a
detailed description, please refer to our paper "FunProbe: Probing Functions
from Binary Code through Probabilistic Analysis", which will be published in
the ACM Joint European Software Engineering Conference and Symposium on the
Foundations of Software Engineering 2023.


## Dependencies

To build and run FunProbe, we have the following software dependencies:

- .NET 6.0
- Python3
- GNU Binutils

FunProbe is written in [F#](https://fsharp.org/). Currently, FunProbe depends
on Python3 and binutils for extracting GOT section information of MIPS binaries
because parsing such information is currently not supported by our front-end,
B2R2. We plan to remove these dependencies and make FunProbe pure F# code.

### Setting up B2R2

For now, we release the B2R2 front-end in binary form. This is because the B2R2
version we used in FunProbe is not yet released in the [upstream
repository](https://github.com/B2R2-org/B2R2).

To set up B2R2, you need to download the [zip
file](https://zenodo.org/record/8266657/files/B2R2-dll.zip?download=1), and
extract it to `B2R2-dll`. The expected structure of the root directory would
be:
```
FunProbe         (the root directory)
`+-B2R2-dll      (B2R2 DLL directory)
 |  `+-...
 +-FunProbe      (FunProbe source directory)
 |  `+-...
 +-FunPRobe.sln  (Visual Studio solution file)
 +-...
```

### Setting up Python3

`FunProbe` does not need any special Python3 packages. However, for
experimental scripts, `pyelftools` is required. To install pyelftools,

```
user@ubuntu:~/FunProbe$ python3 -m pip install pyelftools
```

or

```
user@ubuntu:~/FunProbe$ python3 -m pip install -r requirements.txt
```
In this document (and other documents in this repository), we assume that you
cloned the repository at `~/FunProbe`. The prefix before commands will indicate
where you need to execute the command.


## Building FunProbe

Clone the repository:
```
user@ubuntu:~$ git clone https://github.com/B2R2-org/FunProbe.git
```

To build FunProbe, type the following command:
```
user@ubuntu:~/FunProbe$ dotnet build -c Release
```

### Docker environment

We also provide Docker environment to easily set up the environment and
evaluate FunProbe. To build FunProbe docker image:
```
user@ubuntu:~/FunProbe$ ./build_docker.sh
```


## Usage

After building FunProbe, you can find the FunProbe binary from
`FunProbe/bin/Release/net6.0/FunProbe`. Executing it with `-h` switch will show
possible options. Several important options:

- `-b` or `--bin`: specifies the path to the target binary.
- `-f` or `--function`: specifies the path which will store the function entry
  addresses.
- `--mipsgot`: specifies the path which contains MIPS GOT section information.
- `-p` or `--positive`: specifies the probability of positive hints.
- `-n` or `--negative`: specifies the probability of negative hints.

### Dump MIPS GOT information

There is a simple python3 script (`utils/dump_mipsgot.py`) to dump GOT section
information of MIPS binaries. The usage is like below:
```
user@ubuntu:~/FunProbe/utils$ python3 dump_mipsgot.py <binary path> <output path>
```
Note that the input binary doesn't need to have symbols.

### Example

An example command for running FunProbe:
```
user@ubuntu:~/FunProbe$ FunProbe/bin/Release/net6.0/FunProbe -b /bin/ls -f ./res.txt
```
After a few seconds, function entry points found by `FunProbe` will be stored in `./res.txt`.


## Artifact

We make our experimental settings public to support open science. Our
experiments consist of three different settings:

1. Parameter selection: running `FunProbe` with various parameter settings on a
   limited benchmark. (RQ1)
2. Performance evaluation: running FunProbe, FunProbe-lbp, and other comparison
   targets on a full benchmark. (RQ2 - RQ4)
3. FunProbe + XDA: feeding results from XDA to FunProbe on an Intel benchmark.
   (RQ5)

### Data directory

We repeatedly use the term `data directory` for a directory that stores all
data needed for the evaluation. After all experiments are done, the data
directory will look like:
```
path-to-data-dir       (an example data directory)
`+-bench               (benchmark directory)
 |  `+-...
 +-results             (tool results directory)
 |  `+-...
 +-...
```


### Benchmark

You first need to build the benchmark we used to reproduce our experiments.
For the building instruction, see
[here](https://github.com/B2R2-org/FunProbe/blob/fse23-ae/benchmark/README.md).

### Parameter selection

We picked 10 random binaries from GNU Coreutils for each build configuration
using this command:
```
user@ubuntu:~/FunProbe/param$ ./choose.sh <data directory>
```
This will generate `binlist.txt`, which stores the list of binaries. The list
of binaries we used in the evaluation is in
[param/binlist.txt](https://github.com/B2R2-org/FunProbe/blob/fse23-ae/param/binlist.txt).

We tried different parameter combinations on the small benchmark to evaluate
their effect. To run `FunProbe` on different settings:
```
user@ubuntu:~/FunProbe/param$ ./run.sh <data directory> <# CPU cores>
```

The results can be shown in:
```
user@ubuntu:~/FunProbe/param$ ./report.sh <data directory>
```

### Performance evaluation

We have a separate
[document](https://github.com/B2R2-org/FunProbe/blob/fse23-ae/tools/README.md)
explaining how to set up various comparison targets, run them, and report the
results.

### FunProbe + XDA

We used FunProbe as a post-processor of XDA. Therefore, running XDA on the
benchmark should proceed. After results from XDA are prepared (under
`<data directory>/results/xda`), run:
```
user@ubuntu:~/FunProbe/integrateXDA$ ./run[_wo_spec].sh <data directory> <# CPU cores>
```

The results can be shown in:
```
user@ubuntu:~/FunProbe/integrateXDA$ ./report[_wo_spec].sh <data directory>
```


## Citation

If you use `FunProbe` in scientific work, consider citing our paper:
```
T.B.D.
```
