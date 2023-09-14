#! /bin/bash

set -euo pipefail

cd $(dirname ${BASH_SOURCE[0]})/..

brew install emacs
brew services start emacs
sudo apt-get install -qy \
     graphviz \
     texlive-full

./scripts/setup.common.bash

