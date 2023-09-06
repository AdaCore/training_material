#!/bin/bash
set -e

IDE=gnatstudio2
DIR=$(realpath $(dirname $0))

if ! which gnatstudio2 2>/dev/null >&2; then
    IDE=code
fi

which $IDE

INSTALL=$HOME/install

export GPR_PROJECT_PATH="$DIR/game_support:$GPR_PROJECT_PATH"
export GPR_PROJECT_PATH="$DIR/gnat_sdl:$GPR_PROJECT_PATH"

export HOST=Linux

exec $IDE
