#! /bin/bash

set -euo pipefail

cd $(dirname ${BASH_SOURCE[0]})/..

BIN_DIR=~/.bin
CONFIG_DIR=~/.emacs.d

mkdir -p ${BIN_DIR}
mkdir -p ${CONFIG_DIR}

ln -s $(pwd)/bin/emacs-ssh ${BIN_DIR}/emacs-ssh

for file in "init.el" "early-init.el"; do
    ln -s $(pwd)/config/${file} ${CONFIG_DIR}/${file}
done

echo "alias emacs=\"emacs -nw\"" >> ~/.zshrc
echo "alias vi=\"emacs -nw\"" >> ~/.zshrc

