#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
test -d eggs-5-latest || \
    git clone git://code.call-cc.org/eggs-5-latest --depth 1
./eggs-5-latest-helper.scm >eggs-5-latest.scm.new
mv -f eggs-5-latest.scm.new eggs-5-latest.scm
