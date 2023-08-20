FROM ubuntu:20.04

RUN apt update
RUN apt -y install wget python3

ADD packages /packages
WORKDIR /packages
RUN /packages/install_dotnet.sh

ADD FunProbe.sln /packages/FunProbe.sln
ADD FunProbe /packages/FunProbe
ADD B2R2-dll /packages/B2R2-dll
RUN /packages/build_funprobe.sh

ADD scripts /scripts
