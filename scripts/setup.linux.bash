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
     libjansson-dev \
     libjpeg-dev \
     libpng-dev \
     librsvg2-dev \
     libtinfo-dev \
     libtiff-dev \
     libtree-sitter-dev \
     libwebp-dev \
     libxaw7-dev \
     libxpm-dev \
     libz-dev \
     texlive-full

cd $(dirname ${BASH_SOURCE[0]})

cd ../src
./autogen.sh
./configure \
  --disable-silent-rules \
  --with-gnutls \
  --with-json \
  --with-modules \
  --with-tree-sitter \
  --with-xml2 \
  --without-dbus \
  --without-imagemagick \
  --with-native-compilation \
  CC=gcc-10

make -j8
sudo make install
cd ../scripts
./setup.common.bash

