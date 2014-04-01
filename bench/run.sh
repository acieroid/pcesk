TIMEOUT=600 # 10 min
FAIL=124
CMD="timeout $TIMEOUT ./main.byte -quiet"
FILES=$(find input/seq -type f -name \*.scm)

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

printf "%25.25s\t%s\t\t%s\t\t%s\t\t%s\t\t%s\t\t%s\t\t%s\t\t%s\t\t%s\n" "file" "na" "gc" "sbfs" "sdfs" "gc+sbfs" "gc+sdfs"
for k in $(seq 0 2); do
  echo "k = $k -------------------------------------------------"
  for f in $FILES; do
    basef=$(basename $f)
    printf "%25.25s\t" $basef
    run $f -k $k
    run $f -k $k -gc-after
    run $f -k $k -s -e bfs
    run $f -k $k -s -e dfs
    run $f -k $k -gc-after -s -e bfs
    run $f -k $k -gc-after -s -e dfs
    echo
  done
done
