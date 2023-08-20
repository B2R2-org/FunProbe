FROM ubuntu:20.04

RUN apt update
RUN apt -y install vim git wget gcc g++ python3 python3-pip

ADD packages /packages
WORKDIR /packages
RUN /packages/setup_env.sh

RUN mkdir -p /packages/XDA/checkpoints/funcbound
ADD model/checkpoint_best.pt /packages/XDA/checkpoints/funcbound/checkpoint_best.pt
RUN mkdir -p /packages/XDA/data-bin/funcbound/data
ADD model/data-dict.txt /packages/XDA/data-bin/funcbound/data/dict.txt
RUN mkdir -p /packages/XDA/data-bin/funcbound/label
ADD model/label-dict.txt /packages/XDA/data-bin/funcbound/label/dict.txt

ADD scripts /scripts
RUN cp /scripts/run_xda.py /packages/XDA/run_xda.py
