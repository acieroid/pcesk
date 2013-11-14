TIMEOUT=30
FAIL=124
CMD="timeout $TIMEOUT ./main.byte -quiet"
FILES=$(find input/ -type f -name \*.scm)

function run {
  FILE="$1"
  shift
  RES=$($CMD -i "$FILE" "$@")
  if [ "$?" -eq "$FAIL" ]; then
    echo -n "--- "
  else
    echo -n $RES " "
  fi
}

for f in $FILES; do
  echo -n $f " "
  run $f -k 0
  run $f -k 0 -gc
  run $f -k 1
  run $f -k 1 -gc
  echo ""
done
