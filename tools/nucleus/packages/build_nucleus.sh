#!/bin/bash

set -e

sed -i 's/sudo apt/apt -y/g' /packages/nucleus/Makefile
cd /packages/nucleus
make setup
make
