FROM ubuntu:20.04

RUN apt update
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt -y install openjdk-17-jdk wget unzip python3

RUN mkdir -p /packages
WORKDIR /packages

RUN wget https://github.com/NationalSecurityAgency/ghidra/releases/download/Ghidra_10.1.5_build/ghidra_10.1.5_PUBLIC_20220726.zip
RUN unzip ghidra_10.1.5_PUBLIC_20220726.zip

ADD scripts/ /scripts
