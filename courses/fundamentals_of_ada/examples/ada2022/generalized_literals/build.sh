#! /usr/bin/env bash
set -ex

$ADACUT -c=1 src/main.adb > extracts/main.adb
$ADACUT src/my_int.ads -c 1 2 > extracts/my_int.ads
