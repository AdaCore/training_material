#! /usr/bin/env bash
set -ex

$ADACUT -d -c=1 src/main.adb > extracts/spec.ads
