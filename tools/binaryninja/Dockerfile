FROM ubuntu:20.04

RUN apt update
RUN apt -y install unzip python3 libdbus-1-3

# Put BinaryNinja.zip and license.txt into packages/ directory
ADD packages/ /packages
WORKDIR /packages

RUN /packages/setup_binaryninja.sh

ADD scripts/ /scripts
