set -ex

root=$(dirname $(readlink -e $0))
out=$1
zip="$out.zip"

test ! -z "$out"
test -d "$out" || mkdir "$out"

for sub in src answer mini_cinema.gpr mini_cinema.adc include logs resources tests testsuite README.md; do
    test -f "$root/$sub" -o -d "$root/$sub"
    cp -r "$root/$sub" "$out"
done

cd $(dirname $out)

rm -f "$zip"
zip -r "$zip" "$out"

