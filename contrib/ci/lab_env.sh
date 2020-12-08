#!/usr/bin/bash
if [ "$GITHUB_WORKSPACE" != "" ]; then
    ROOTDIR=$GITHUB_WORKSPACE
else
    # Be smart, and dangerous
    ROOTDIR=$(realpath $(realpath $(dirname $0))/../../)
fi

test "$ROOTDIR" != ""

EXTERN=$ROOTDIR/extern

export LD_LIBRARY_PATH=/usr/lib
export GPR_PROJECT_PATH=$EXTERN/gnat_sdl:$EXTERN/game_support

linker_display_search_path() {
    gcc -m64 -Xlinker --verbose  2>/dev/null \
        | grep SEARCH \
        | sed 's/SEARCH_DIR("=\?\([^"]\+\)"); */\1\n/g' \
        | grep -vE '^$'
}
