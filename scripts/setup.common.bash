#! /bin/bash

set -euox pipefail

cd $(dirname ${BASH_SOURCE[0]})/..

BIN_DIR=~/.bin
CONFIG_DIR=~/.emacs.d

mkdir -p ${BIN_DIR}
mkdir -p ${CONFIG_DIR}

ln -s $(pwd)/bin/emacs-ssh ${BIN_DIR}/emacs-ssh

for file in "init.el" "early-init.el"; do
    ln -s $(pwd)/config/${file} ${CONFIG_DIR}/${file}
done

echo 'export EDITOR=“$(which emacs) -nw”' >> ~/.zshrc
echo 'alias emacs=“${EDITOR}”' >> ~/.zshrc
echo 'alias vi=“${EDITOR}”' >> ~/.zshrc

