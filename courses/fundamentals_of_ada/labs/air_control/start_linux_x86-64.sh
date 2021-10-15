#!/bin/bash

set -ex

ROOT=$(dirname $(readlink -f $( (cd ${0}/..; pwd) )/../../..))

INSTALL=$HOME/install

for d in $ROOT/extern/game_support $ROOT/extern/gnat_sdl; do
    test -d $d
    export GPR_PROJECT_PATH=$d:$GPR_PROJECT_PATH
done

export HOST=Linux

exec gnatstudio
