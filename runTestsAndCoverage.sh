#!/bin/sh

set -e

export LC_ALL=C
export LANG=C

rm -f testsuite.tix

./dist/build/testsuite/testsuite -a1000 $*

# cabal test --show-details=always --test-options="-j4 -a1000 $*"

DIR=dist/hpc

rm -Rf $DIR
mkdir -p $DIR

EXCLUDES='
'

EXCL=""

for m in $EXCLUDES; do
    EXCL="$EXCL --exclude=$m"
done

hpc markup $EXCL --destdir=$DIR testsuite >/dev/null 2>&1

rm -f testsuite.tix

cat <<EOF

Test coverage report written to $DIR.
EOF
