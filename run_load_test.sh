#!/usr/bin/env bash
# Rebuild OpenStats from the current local source into a throwaway container,
# then run N sequential dose-response analyses in one browser session,
# logging elapsed time and container memory per iteration.
#
# Usage: ./run_load_test.sh [N_ITERATIONS] [PORT]
set -euo pipefail

N="${1:-20}"
PORT="${2:-3839}"
CONTAINER="openstats-loadtest"
IMAGE="openstats"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "=== [1/4] reinstalling OpenStats from local source + starting container on port ${PORT} ==="
docker rm -f "$CONTAINER" > /dev/null 2>&1 || true
docker run -d --name "$CONTAINER" -p "${PORT}:${PORT}" \
  -v "${ROOT}/OpenStats:/pkg" \
  "$IMAGE" \
  bash -c "R CMD INSTALL /pkg && Rscript -e \"library(OpenStats); app <- OpenStats:::app(); shiny::runApp(shiny::shinyApp(app\\\$ui, app\\\$server), host='0.0.0.0', port=${PORT})\"" \
  > /dev/null

echo "=== [2/4] waiting for the app to become reachable ==="
for i in $(seq 1 60); do
  if curl -sS -o /dev/null -w '' --max-time 2 "http://127.0.0.1:${PORT}/" 2>/dev/null; then
    echo "up after ${i}s"
    break
  fi
  sleep 1
done

echo "=== [3/4] starting memory sampler + running ${N} sequential analyses ==="
MEMLOG="${ROOT}/loadtest/memlog.csv"
echo "timestamp,mem_usage,mem_perc,cpu_perc" > "$MEMLOG"
(
  while docker inspect "$CONTAINER" > /dev/null 2>&1; do
    ts=$(date +%s)
    stats=$(docker stats --no-stream --format '{{.MemUsage}},{{.MemPerc}},{{.CPUPerc}}' "$CONTAINER" 2>/dev/null) || break
    echo "${ts},${stats}" >> "$MEMLOG"
    sleep 1
  done
) &
MONITOR_PID=$!

cd "${ROOT}/loadtest"
LOADTEST_PORT="$PORT" node repeated.js "$N"

kill "$MONITOR_PID" > /dev/null 2>&1 || true

echo "=== [4/4] summary ==="
echo "--- timing (repeated_log.csv) ---"
column -s, -t repeated_log.csv
echo ""
echo "--- memory: first vs last sample ---"
head -2 "$MEMLOG" | tail -1
tail -1 "$MEMLOG"
echo ""
echo "Container left running as '${CONTAINER}' on port ${PORT} for inspection. Remove with: docker rm -f ${CONTAINER}"
