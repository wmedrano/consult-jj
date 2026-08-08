#!/usr/bin/env bash
set -euo pipefail

echo "Byte compiling package"
emacs --batch -L . \
  --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile consult-jj.el

echo "Byte compiling tests"
emacs --batch -L . \
  --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile consult-jj-tests.el

echo "Running checkdoc"
checkdoc_output=$(emacs --batch -L . --eval '(checkdoc-file "consult-jj.el")' 2>&1)
if echo "$checkdoc_output" | grep -q "^Warning (emacs):"; then
    echo "$checkdoc_output"
    echo "Checkdoc warnings found."
    exit 1
fi

emacs --batch -L . -l consult-jj -l consult-jj-tests \
  -f ert-run-tests-batch-and-exit
