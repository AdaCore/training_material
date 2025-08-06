#! /usr/bin/env bash
set -ex

$ADACUT -d -c=1 src/main.adb > extracts/static_expr_fun.ads
