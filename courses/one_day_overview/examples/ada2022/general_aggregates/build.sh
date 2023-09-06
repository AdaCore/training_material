set -ex

$ADACUT -d -c=1 src/main.adb > extracts/square_brackets.ads
$ADACUT -d src/main.adb -c 2 > extracts/iteration_filters.ads
