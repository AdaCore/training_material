set -e

orig=$1
OUT=$PWD/out
ar=$OUT/$(basename $orig).zip
(cd $orig && git archive HEAD -o $ar)
(cd $OUT && unzip $ar)
