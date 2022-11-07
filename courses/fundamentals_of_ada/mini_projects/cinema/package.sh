set -e

root=$(dirname $(readlink -e $0))
out=$1
zip="$out.zip"

test -d "$out" || mkdir "$out"

for sub in src answer mini_cinema.gpr mini_cinema.adc resources tests testsuite README.md; do
    cp -r "$root/$sub" "$out"
done

cd $(dirname $out)

rm -f "$zip"
zip -r "$zip" "$out"

