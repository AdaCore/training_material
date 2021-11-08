set -e
FWD=$(dirname $0)
ADACUT=$FWD/../../../../../contrib/adacut.py

(
 set -e
 cd "$FWD"
 echo "------------"
 echo "Cut template"
 echo "------------"
 for f in template/*.ad?; do
   echo $f
   "$ADACUT" --mode=answer $f > answers/$(basename $f)
   "$ADACUT" --mode=question $f > src/$(basename $f)
 done

 echo "-------------"
 echo "Check results"
 echo "-------------"
 for mode in Question Solution; do
    echo
    echo "> Mode $mode"
    echo
    gprbuild -Xmode=$mode && obj/main
 done
)
