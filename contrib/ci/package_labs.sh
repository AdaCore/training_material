#! /usr/bin/env bash
set -e

orig=$1
OUT=$(realpath "$2")
ar=$OUT/$(basename "$orig").zip
if [ ! -f "$orig/package.sh" ]; then
    (cd "$orig" && git archive HEAD -o "$ar")
else
    (cd "$orig" && bash package.sh "$ar")
fi
(
 set +e
 cd "$OUT" && zip "$ar" *.pdf
)
