#!/usr/bin/env bash
# src/ must be the archived original, except for the edits logged in
# PORTING_NOTES.md.  Every file that differs must be named in
# tests/src-diff-allowed.txt (one file name per line), and every file named
# there must differ (so the list does not go stale).
set -u
cd "$(dirname "$0")/.." || exit 1
archive="../Melanie-Mitchell-[LISP]"
allowed="tests/src-diff-allowed.txt"
status=0
differing=$(diff -rq "$archive" src 2>&1 | grep -v "Only in $archive: ccat.tar.gz" \
            | sed -n 's/^Files .*\/\([^/]*\) and .* differ$/\1/p')
for f in $differing; do
    if ! grep -qx "$f" "$allowed"; then
        echo "src/$f differs from the archive but is not in $allowed"
        status=1
    fi
done
while read -r f; do
    [ -z "$f" ] && continue
    case "$f" in \#*) continue ;; esac
    if ! echo "$differing" | grep -qx "$f"; then
        echo "$allowed names $f but src/$f is identical to the archive"
        status=1
    fi
done < "$allowed"
# Nothing else may be in src/ either.
extra=$(diff -rq "$archive" src 2>&1 | grep "^Only in src" )
if [ -n "$extra" ]; then echo "$extra"; status=1; fi
exit $status
