#! /bin/bash

set -euox pipefail

cd $(dirname ${BASH_SOURCE[0]})/..

CONFIG_DIR=~/.emacs.d

mkdir -p ${CONFIG_DIR}

for file in "init.el" "early-init.el"; do
    ln -s $(pwd)/config/${file} ${CONFIG_DIR}/${file}
done

echo 'export EDITOR=“$(which emacs) -nw”' >> ~/.zshrc
echo 'alias emacs=“${EDITOR}”' >> ~/.zshrc
echo 'alias vi=“${EDITOR}”' >> ~/.zshrc

