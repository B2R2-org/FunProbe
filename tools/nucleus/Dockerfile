FROM ubuntu:16.04

RUN apt update
RUN apt -y install git make g++ python3

ADD packages /packages
WORKDIR /packages

RUN git clone https://bitbucket.org/vusec/nucleus.git
RUN /packages/build_nucleus.sh

ADD scripts /scripts
