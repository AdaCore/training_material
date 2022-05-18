set -e
FWD=$(dirname $0)
ADACUT=$FWD/../../../../contrib/adacut.py

(
 set -e
 cd "$FWD"

 set -x
 "$ADACUT" -c 1 2 --dedent src/tasks.adb > extracts/tasks.body.adb
)
