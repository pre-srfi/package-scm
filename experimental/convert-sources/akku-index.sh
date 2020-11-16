#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
curl --location --fail --silent --show-error \
     -o akku-index.scm \
     https://archive.akkuscm.org/archive/Akku-index.scm
