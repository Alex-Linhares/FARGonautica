#!/usr/bin/env bash
# Item 12: follow src/README.md.  Every ```sh block in the README is extracted
# and run as written from Software/Numbo/numbo-sbcl/, and must print "Done :".  The first
# block (puzzle 3, seed 18) must also end with the output the README shows
# (its ``` block that follows "The output ends with:"), and the check-solution
# block must report a valid solution.  Exit 0 = pass.
set -u
cd "$(dirname "$0")/.." || exit 1
readme=src/README.md
fail=0
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# Split the README's ```sh blocks into $tmp/block-N.sh.
awk -v dir="$tmp" '
  /^```sh$/ { n++; f = dir "/block-" n ".sh"; inblk = 1; next }
  /^```$/   { inblk = 0; next }
  inblk     { print > f }' "$readme"

# The expected output: the first plain ``` block after "The output ends with:".
awk '
  /The output ends with:/ { want = 1; next }
  want && /^```$/ && !inblk { inblk = 1; next }
  inblk && /^```$/ { exit }
  inblk { print }' "$readme" > "$tmp/expected.txt"

blocks=$(ls "$tmp"/block-*.sh 2>/dev/null | wc -l)
if [ "$blocks" -lt 4 ]; then
    echo "readme: expected at least 4 sh blocks, found $blocks"; exit 1
fi

for i in $(seq 1 "$blocks"); do
    out="$tmp/out-$i.txt"
    bash "$tmp/block-$i.sh" > "$out"
    # The check-solution block captures the run's output and prints the
    # checker's verdict instead.
    if grep -q '^Done :' "$out" || grep -q '^(T NIL ' "$out"; then
        echo "readme: block $i prints Done : or a valid check-solution"
    else
        echo "readme: FAIL block $i prints neither Done : nor a valid check-solution"; fail=1
    fi
done

# Block 1: its output must end with the README's expected text (trailing
# blanks ignored: PRINT ends with a space, "applied " has one).
n=$(wc -l < "$tmp/expected.txt")
if [ "$n" -ge 5 ] && diff -b <(tail -n "$n" "$tmp/out-1.txt") "$tmp/expected.txt" > "$tmp/diff.txt"; then
    echo "readme: block 1 ends with the shown output ($n lines)"
else
    echo "readme: FAIL block 1 output differs from the README:"; cat "$tmp/diff.txt"; fail=1
fi

# The check-solution block must report a valid solution.
if grep -qF '(T NIL "31 = (14 x (5 - 3)) + 3")' "$tmp"/out-*.txt; then
    echo "readme: check-solution block accepts the solution"
else
    echo "readme: FAIL no block printed the accepted solution"; fail=1
fi

exit "$fail"
