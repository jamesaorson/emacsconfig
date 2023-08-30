#! /bin/bash

set -euo pipefail

cd $(dirname ${BASH_SOURCE[0]})/..

BIN_DIR=~/.bin
CONFIG_DIR=~/.emacs.d

mkdir -p ${BIN_DIR}
mkdir -p ${CONFIG_DIR}

ln -s -f $(pwd)/bin/emacs-ssh ${BIN_DIR}/emacs-ssh

for file in "init.el" "early-init.el"; do
    ln -s -f $(pwd)/emacs.d/${file} ${CONFIG_DIR}/${file}
done

# NOTE: --user avoids an error: https://emacs.stackexchange.com/questions/34022/error-initialization-user-has-no-home-directory
# echo "alias emacs=\"emacs -nw --user=''\"" >> ~/.zshrc
# echo "alias vi=\"emacs -nw --user=''\"" >> ~/.zshrc

