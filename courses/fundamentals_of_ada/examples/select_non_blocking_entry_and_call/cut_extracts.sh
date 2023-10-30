#! /usr/bin/env bash
set -e
FWD=$(dirname "$0")
ADACUT=$FWD/../../../../contrib/adacut.py

(
 set -e
 cd "$FWD"

 set -x
 "$ADACUT" -c 1 --dedent src/tasks.adb > extracts/tasks.adb
 for f in src/main*.adb; do
     "$ADACUT" -c 1 --dedent "$f" > "extracts/$(basename "${f%.*}").adb"
 done
)
