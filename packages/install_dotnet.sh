#!/bin/bash

set -e

wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
dpkg -i packages-microsoft-prod.deb

apt update
apt -y install dotnet-sdk-6.0 python3
