set -e

orig=$(realpath $1)
out=$(realpath $2)

mkdir -p $out

ar=$out/$(basename $orig).zip
(cd $orig && git archive HEAD -o $ar)
(cd $out && zip $ar *.pdf) || true
