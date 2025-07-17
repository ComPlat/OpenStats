#!/bin/bash

# Sys.setenv(SHINYTEST2_APP_DRIVER_TEST_ON_CRAN=1)
export SHINYTEST2_APP_DRIVER_TEST_ON_CRAN=1

TEST_DIR="./OpenStats/inst/tinytest"

if [ ! -d "$TEST_DIR" ]; then
  echo "Error: Directory $TEST_DIR does not exist."
  exit 1
fi

echo "Running tests in $TEST_DIR..."

FAIL=0

for TEST_FILE in "$TEST_DIR"/*.R; do
  echo "Running: $TEST_FILE"

  OUT=$(mktemp)
  Rscript "$TEST_FILE" >"$OUT" 2>&1

  if grep -qE "FAILED|ERROR" "$OUT"; then
    echo "❌ Test failed: $TEST_FILE"

    # Print line number and surrounding context
    grep -nE "FAILED|ERROR" "$OUT" | while IFS=: read -r line_num line_text; do
      echo "--- Context around line $line_num ---"
      start=$((line_num - 3))
      [ $start -lt 1 ] && start=1
      end=$((line_num + 3))
      sed -n "${start},${end}p" "$OUT"
      echo "-------------------------------------"
    done

    FAIL=1
  else
    echo "✅ Passed: $TEST_FILE"
  fi

  rm "$OUT"
done

if [ $FAIL -eq 1 ]; then
  echo "Some tests failed."
  exit 1
else
  echo "All tests passed successfully!"
  exit 0
fi
