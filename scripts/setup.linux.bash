#! /bin/bash

set -euox pipefail

cd $(dirname ${BASH_SOURCE[0]})/..

sudo apt-get install emacs
