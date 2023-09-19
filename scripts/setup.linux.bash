#! /bin/bash

set -euo pipefail

sudo apt-get install -qy \
     build-essential \
     graphviz \
     libgif-dev \
     libgnutls28-dev \
     libjpeg-dev \
     libpng-dev \
     librsvg2-dev \
     libtinfo-dev \
     libtiff-dev \
     libwebp-dev \
     libxaw7-dev \
     libxpm-dev \
     libz-dev \
     texlive-full

cd $(dirname ${BASH_SOURCE[0]})

cd ../src
make
sudo make install
cd ..

./scripts/setup.common.bash
