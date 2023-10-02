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
  --with-cocoa \
  --with-gnutls \
  --with-modules \
  --with-ns \
  --with-tree-sitter \
  --with-xml2 \
  --without-dbus \
  --without-imagemagick \
  --without-selinux \
  --without-x

make -j8
sudo make install
ln -s -f $(pwd)/nextstep/Emacs.app /Applications/Emacs.app

cd ..
./scripts/setup.common.bash

