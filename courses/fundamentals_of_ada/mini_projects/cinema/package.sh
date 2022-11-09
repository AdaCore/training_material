set -ex

root=$(dirname $(readlink -e $0))
out="$root/pkg"
zip="$1"
test ! -z "$zip"

test -d "$out" || mkdir "$out"
rm -rf "$out/*"

for sub in src answer mini_cinema.gpr mini_cinema.adc include logs resources tests testsuite README.md; do
    test -f "$root/$sub" -o -d "$root/$sub"
    cp -r "$root/$sub" "$out"
done

(
cd $(dirname $out)

rm -f "$zip" 2>/dev/null
zip -r "$zip" "$out"
)
