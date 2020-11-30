#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

INSTALL=$HOME/install

export GPR_PROJECT_PATH=$DIR/game_support:$GPR_PROJECT_PATH
export GPR_PROJECT_PATH=$DIR/gnat_sdl:$GPR_PROJECT_PATH

export HOST=Linux

exec gnatstudio
