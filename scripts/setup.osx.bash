#! /bin/bash

set -euox pipefail

cd $(dirname ${BASH_SOURCE[0]})/..

brew install --cask \
    emacs

./scripts/setup.common.bash
