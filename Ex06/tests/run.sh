#!/bin/sh

rm tests/actual/* >/dev/null 2>&1

cmd=':l Ex06.hs'

for f in tests/input/*.txt; do
    bname=$(basename "$f")

    cmd="${cmd}\nonlyUnique \"${f}\" \"tests/actual/${bname}\""
done

# TODO: change `stack repl` to `cabal repl` on CSE
# Can check `tests/ghci.log` for ghci output
echo "$cmd" | stack repl >tests/ghci.log 2>&1

for f in tests/actual/*.txt; do
    bname=$(basename "$f")
    expected="tests/expected/${bname}"
    actual="tests/actual/${bname}"

    if ! diff -q "$expected" "$actual" >/dev/null 2>&1; then
        echo "TEST FAILED at ${bname}"
        echo 'Your output was:'
        cat "$actual"
        echo 'The expected output was:'
        cat "$expected"

        exit 1
    fi
done

echo 'All tests passed!'
