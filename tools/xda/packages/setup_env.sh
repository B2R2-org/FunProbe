#!/bin/bash

set -e

python3 -m pip install numpy==1.19.5 scipy scikit-learn colorama pyelftools
python3 -m pip install torch==2.0.0+cu118 torchvision==0.15.1+cu118 torchaudio==2.0.1 --index-url https://download.pytorch.org/whl/cu118

git clone https://github.com/CUMLSec/XDA
