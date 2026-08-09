#!/usr/bin/env bash
set -euo pipefail

echo "Byte compiling package"
emacs --batch \
  --eval '(package-activate-all)' \
  --eval '(add-to-list (quote load-path) default-directory)' \
  --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile consult-jj.el consult-jj-diff.el consult-jj-describe.el

echo "Byte compiling tests"
emacs --batch \
  --eval '(package-activate-all)' \
  --eval '(add-to-list (quote load-path) default-directory)' \
  --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile consult-jj-tests.el

echo "Running checkdoc"
checkdoc_output=$(emacs --batch \
  --eval '(package-activate-all)' \
  --eval '(add-to-list (quote load-path) default-directory)' \
  --eval '(checkdoc-file "consult-jj.el")' \
  --eval '(checkdoc-file "consult-jj-diff.el")' \
  --eval '(checkdoc-file "consult-jj-describe.el")' 2>&1)
if echo "$checkdoc_output" | grep -q "^Warning (emacs):"; then
    echo "$checkdoc_output"
    echo "Checkdoc warnings found."
    exit 1
fi

emacs --batch \
  --eval '(package-activate-all)' \
  --eval '(add-to-list (quote load-path) default-directory)' \
  -l consult-jj -l consult-jj-tests \
  -f ert-run-tests-batch-and-exit
