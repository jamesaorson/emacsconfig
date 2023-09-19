#! /bin/bash

set -euox pipefail

sudo apt-get install -qy \
     build-essential \
     gcc-10 \
     graphviz \
     libgccjit-10-dev \
     libgif-dev \
     libgnutls28-dev \
     libharfbuzz-dev \
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
./autogen.sh
./configure \
    --with-native-compilation \
    CC=gcc-10

make -j8
sudo make install
cd ../scripts
./setup.common.bash

