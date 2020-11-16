#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
test -d eggs-4-latest || \
    git clone git://code.call-cc.org/eggs-4-latest --depth 1
./eggs-helper.scm eggs-4-latest >eggs-4-latest.scm.new
mv -f eggs-4-latest.scm.new eggs-4-latest.scm
