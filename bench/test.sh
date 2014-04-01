TIMEOUT=600 # 10 min
TIMEOUTFAIL=124
CMD="timeout $TIMEOUT ./main.byte -quiet"
FILES=$(find input/seq -type f -name \*.scm)

function run {
  FILE="$1"
  shift
  RES=$($CMD -i "$FILE" "$@" 2>&1)
  ERR="$?"
  if [ "$ERR" -eq "$TIMEOUTFAIL" ]; then
    echo -n -e "---\t"
  else
    echo -n -e $RES "\t"
  fi
}

printf "%25.25s\t%s\n" "file" "success"
for f in $FILES; do
  basef=$(basename $f)
  printf "%25.25s\t" $basef
  run $f -k 0 -gc-after -s -e bfs
  echo
done
