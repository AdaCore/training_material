#!/usr/bin/bash
for d in ~/.config/alire/cache/dependencies/*/bin; do
    # Hack to get access to the gnat and gprbuild executables...
    export PATH=$d:$PATH
done
