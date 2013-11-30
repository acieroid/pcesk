 cat bench.txt | sed -E 's/\t+/|/g' | column -s '|' -t
