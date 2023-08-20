#!/bin/bash

/packages/ghidra_10.1.5_PUBLIC/support/analyzeHeadless . myproject -scriptPath /scripts -postScript ghidra_script.py $2 -import $1 -deleteProject
