#! /bin/bash

set -euox pipefail

cd $(dirname ${BASH_SOURCE[0]})

BIN_DIR=${HOME}/.local/bin
CONFIG_DIR=~/.emacs.d

mkdir -p ${BIN_DIR}
mkdir -p ${CONFIG_DIR}

ln -s -f $(pwd)/bin/emacs-ssh ${BIN_DIR}/emacs-ssh

CWD=$(pwd)
pushd ${CONFIG_DIR}
for file in ${CWD}/emacs.d/early-init.el ${CWD}/emacs.d/init.el ${CWD}/emacs.d/packages; do
    ln -s -f ${file}
done
popd

# NOTE: --user avoids an error: https://emacs.stackexchange.com/questions/34022/error-initialization-user-has-no-home-directory
# echo "alias emacs=\"emacs -nw --user=''\"" >> ~/.zshrc
# echo "alias vi=\"emacs -nw --user=''\"" >> ~/.zshrc

