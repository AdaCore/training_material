#!/usr/bin/bash
# https://unix.stackexchange.com/a/4673
# if we are sourced from sh, then we can only rely on GITHUB_WORKSPACE
sourced_from=$_

if [ "$GITHUB_WORKSPACE" != "" ]; then
    ROOTDIR=$GITHUB_WORKSPACE
else
    # Be smart, and dangerous
    if [ -n "$sourced_from" ] && [ "$(basename "$sourced_from")" == "sh" ]; then
        echo "In order to source this script from sh you must set GITHUB_WORKSPACE">&2
        exit 2
    fi
    ROOTDIR=$(realpath "$(realpath "$(dirname "$0")/../../")")
fi
CONTRIB_CI_DIR=$ROOTDIR/contrib/ci

test "$ROOTDIR" != ""

export LD_LIBRARY_PATH=/usr/lib

linker_display_search_path() {
    gcc -m64 -Xlinker --verbose  2>/dev/null \
        | grep SEARCH \
        | sed 's/SEARCH_DIR("=\?\([^"]\+\)"); */\1\n/g' \
        | grep -vE '^$'
}

# shellcheck disable=SC1091
. "$CONTRIB_CI_DIR/alr_gnat_env.profile"
