#! /usr/bin/env bash
set -e
FWD=$(dirname $0)
ADACUT=$FWD/../../../../contrib/adacut.py

(
 set -e
 cd "$FWD"

 set -x
 "$ADACUT" -c 2 1 --dedent src/tasks.adb > extracts/tasks.bodies.adb
)
