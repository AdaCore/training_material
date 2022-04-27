set -e
FWD=$(dirname $0)
ADACUT=$FWD/../../../../contrib/adacut.py

(
 set -e
 cd "$FWD"

 set -x
 "$ADACUT" --cut=1 --dedent src/protected_objects.ads > extracts/protected_objects.lock_free_declare.ads
)
