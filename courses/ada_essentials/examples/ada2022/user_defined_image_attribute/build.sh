#! /usr/bin/env bash
set -ex

$ADACUT -c=1 src/main.adb > extracts/spec.ads
$ADACUT -c=2 src/main.adb > extracts/body.adb
