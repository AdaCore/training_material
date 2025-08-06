#! /usr/bin/env bash
set -e
FWD=$(dirname "$0")
ADACUT=$(which adacut)

(
 set -e
 cd "$FWD"

 set -x
 "$ADACUT" -c 2 1 --dedent src/tasks.adb > extracts/tasks.bodies.adb
)
