set -ex

$ADACUT -c=1 src/main.adb > extracts/put_line.adb
gprbuild -P prj.gpr
obj/main | sed '/^[[:space:]]*$/d' > out.txt
