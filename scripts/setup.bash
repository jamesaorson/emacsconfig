#! /bin/bash

set -euox pipefail

cd $(dirname ${BASH_SOURCE[0]})

if sudo -v; then
  PLATFORM="$(uname -s)"
  case ${PLATFORM} in
    Linux*)     if sudo -v; then
                  sudo apt-get install -qy \
	                   build-essential \
	                   gcc-10 \
	                   graphviz \
	                   libacl1-dev \
	                   libc6-dev \
	                   libdbus-1-dev \
	                   libgccjit-10-dev \
	                   libgconf2-dev \
	                   libgif-dev \
	                   libgnutls28-dev \
	                   libgpm-dev \
	                   libgtk-3-dev \
	                   libharfbuzz-bin \
	                   libharfbuzz-dev \
	                   libice-dev \
	                   libjansson-dev \
	                   libjpeg-dev \
	                   libm17n-dev \
	                   libmagickcore-dev \
	                   libmagickwand-dev \
	                   libncurses5-dev \
	                   libotf-dev \
	                   libpng-dev \
	                   librsvg2-dev \
	                   libsm-dev \
	                   libsystemd-dev \
	                   libtiff-dev \
	                   libtinfo-dev \
	                   libwebkit2gtk-4.0-dev \
	                   libwebp-dev \
	                   libx11-dev \
	                   libxaw3dxft8-dev \
	                   libxaw7-dev \
	                   libxext-dev \
	                   libxft-dev \
	                   libxft2 \
	                   libxi-dev \
	                   libxmu-dev \
	                   libxmuu-dev \
	                   libxpm-dev \
	                   libxrandr-dev \
	                   libxt-dev \
	                   libxtst-dev \
	                   libxv-dev \
	                   libz-dev \
                       ripgrep \
	                   texlive-full \
	                   xaw3dg-dev \
	                   zlib1g-dev

                  TREE_SITTER=libtree-sitter-dev
                  if [[ $(apt-cache search ${TREE_SITTER}) != "" ]]; then
	                sudo apt-get install -qy ${TREE_SITTER}
                  fi
                fi
                CONFIGURE_ARGS=$(cat <<-EOF
--with-cairo \
--with-gconf \
--with-gnutls \
--with-imagemagick \
--with-json \
--with-modules \
--with-native-compilation \
--with-tree-sitter=ifavailable \
--with-x \
--with-xft \
--with-xml2 \
--with-xwidgets \
--with-x-toolkit=gtk3 \
CC=gcc-10
EOF
)
                ;;
    Darwin*)    brew install \
                     gnutls \
                     ripgrep \
                     texinfo \
                     tree-sitter
                CONFIGURE_ARGS=$(cat <<-EOF
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
EOF
)
                ;;
    *)          echo "UNKNOWN:${unameOut}"; exit 1;;
  esac
fi

LOCAL_DIR=${HOME}/.local

cd ../src
./autogen.sh
./configure \
  --prefix=${LOCAL_DIR} \
  ${CONFIGURE_ARGS}
make -j16
make install

./post.sh

