#! /usr/bin/env bash
set -e

PROJECT=$(realpath $(dirname $0))
FILTER=$PROJECT/pandoc/beamer_filter.py
OUT=out

name_and_convert_to_pdf () {
    CURD=$PWD
    output_file_format=pdf
    input_file_rel=$1

    input_dir=$(dirname $input_file_rel)
    input_file_name=$(basename $input_file_rel)
    input_file_name_no_ext=$(echo $input_file_name | cut -d '.' -f 1)
    output_file_rel=$OUT/${input_file_name_no_ext}.$output_file_format
    output_file=$CURD/$output_file_rel

    echo "$input_file_rel -> $output_file_rel"

    pushd $input_dir > /dev/null
    pandoc -s -t beamer $input_file_name -o $output_file "--filter=$FILTER" \
        "-Vtheme=adacore" "-Vcolortheme=adacore"
    popd > /dev/null
}

mkdir -p $OUT
for f in ${@:1} ; do
    name_and_convert_to_pdf "$f"
done
