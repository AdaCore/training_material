#! /usr/bin/env bash
set -e
ADACUT="adacut"

for d in template/*; do
    ans_dest_dir=answer/$(basename "$d")
    q_dest_dir=src/$(basename "$d")
    mkdir -p "$ans_dest_dir"
    mkdir -p "$q_dest_dir"
    for f in "$d"/*; do
        echo "$f"
        $ADACUT -m question "$f" > "$q_dest_dir/$(basename "$f")"
        $ADACUT -m answer "$f" > "$ans_dest_dir/$(basename "$f")"
    done
done
