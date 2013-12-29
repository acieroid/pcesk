TIMEOUT=30
FAIL=124
CMD="timeout $TIMEOUT ./main.byte -quiet"
FILES=$(find input/bench-seq -type f -name \*.scm)

function run {
  FILE="$1"
  shift
  RES=$($CMD -i "$FILE" "$@")
  if [ "$?" -eq "$FAIL" ]; then
    echo -n -e "---\t"
  else
    echo -n -e $RES "\t"
  fi
}

printf "%25.25s\t%s\t\t%s\t\t%s\t\t%s\t\t%s\t\t%s\t\t%s\t\t%s\t\t%s\n" "file" "0" "0+gc" "0+s" "0+s+gc" "1" "1+gc" "1+s" "1+s+gc"
for f in $FILES; do
  basef=$(basename $f)
  printf "%25.25s\t" $basef
  run $f -k 0
  run $f -k 0 -gc-after
  run $f -k 0 -s
  run $f -k 0 -gc-after -s
  run $f -k 1
  run $f -k 1 -gc-after
  run $f -k 1 -s
  run $f -k 1 -gc-after -s
  echo ""
done
