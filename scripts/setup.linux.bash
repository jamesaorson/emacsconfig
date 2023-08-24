#! /bin/bash

set -euo pipefail

cd $(dirname ${BASH_SOURCE[0]})/..

sudo apt-get install -qy \
     emacs \
     texlive-full

./scripts/setup.common.bash

