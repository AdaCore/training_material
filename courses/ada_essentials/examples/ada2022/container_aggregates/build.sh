#! /usr/bin/env bash
set -ex

$ADACUT -d -c=1 src/main.adb > extracts/decl_aspect.ads
$ADACUT -d -c=2 src/main.adb > extracts/decl_object.ads
