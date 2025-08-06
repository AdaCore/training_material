#! /usr/bin/env bash
set -e
FWD=$(dirname "$0")
ADACUT=$(which adacut)

(
 set -e
 cd "$FWD"

 set -x
 "$ADACUT" -c 1 2 --dedent src/tasks.adb > extracts/tasks.body.adb
)
