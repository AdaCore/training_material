#! /usr/bin/env bash
set -e
FWD=$(dirname "$0")
ADACUT=$(which adacut)

(
 set -e
 cd "$FWD"

 set -x
 "$ADACUT" --cut=1 --dedent src/task_select.adb > extracts/task_select.select_or.adb
)
