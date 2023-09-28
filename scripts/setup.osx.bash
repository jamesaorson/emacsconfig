#! /bin/bash

set -euox pipefail

cd $(dirname ${BASH_SOURCE[0]})

brew install \
    gnutls \
    texinfo \
    tree-sitter

cd ../src
./autogen.sh
./configure \
  --disable-silent-rules \
  --with-gnutls \
  --without-x \
  --with-xml2 \
  --without-dbus \
  --with-modules \
  --without-imagemagick \
  --without-selinux \
  --with-tree-sitter \
  --with-ns \
  --with-cocoa

make -j8
sudo make install
ln -s -f $(pwd)/nextstep/Emacs.app /Applications/Emacs.app

cd ..
./scripts/setup.common.bash

