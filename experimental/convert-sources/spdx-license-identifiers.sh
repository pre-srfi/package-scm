#!/bin/bash
set -eu -o pipefail
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
curl --location --fail --silent --show-error \
    https://raw.githubusercontent.com/spdx/license-list-data/master/json/licenses.json |
    jq '.licenses[].licenseId' >spdx-license-identifiers.scm
