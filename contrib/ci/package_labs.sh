set -e

orig=$1
OUT=$(realpath $2)
ar=$OUT/$(basename $orig).zip
(cd $orig && git archive HEAD -o $ar)
(cd $OUT && zip $ar *.pdf)
