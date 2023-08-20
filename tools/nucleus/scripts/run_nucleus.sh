#!/bin/bash

/packages/nucleus/nucleus -e $1 -d linear -t elf -a $3 -f 2>/dev/null >$2
