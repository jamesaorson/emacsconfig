#! /bin/bash

set -euo pipefail

cd $(dirname ${BASH_SOURCE[0]})/..

brew install --cask \
     emacs     

brew install \
     mactex

./scripts/setup.common.bash

