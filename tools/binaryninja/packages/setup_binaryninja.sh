#!/bin/bash

set -e

unzip BinaryNinja.zip
/packages/binaryninja/scripts/linux-setup.sh -d -m
cp /packages/license.txt /root/.binaryninja/license.dat
