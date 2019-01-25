#!/bin/bash

set -euo pipefail

# git filter-branch -f --tree-filter $(pwd)/filter.sh --prune-empty -- HEAD

find . -not -name 'erjoalgo-compile*'  \
     -a -not -name 'autobuild*' \
     -a -not -name 'compile-settings*' \
     -a -not -iregex '[.]/[.]git/.*' \
     -type f \
     -exec  git rm -r -f --ignore-unmatch {} + \
     > /dev/null
