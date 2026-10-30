#!/usr/bin/env bash
# Serialised startup timing for the shinylive speed work: only one browser timing
# run at a time on this machine (lock shared by all agents), so runs do not compete
# for the 4 CPU cores. Reports page-open -> ready and -> first results (Analyze
# clicked as soon as possible) for the Dannheim app.
# Usage: work/shinylive/timed-run.sh URL OUT.json [RUNS=3]
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
export METAUI_PLAYWRIGHT="${METAUI_PLAYWRIGHT:?Set METAUI_PLAYWRIGHT to an installed playwright module}"
export METAUI_CHROMIUM="${METAUI_CHROMIUM:?Set METAUI_CHROMIUM to an existing Chromium executable}"
exec flock /tmp/shinylive-speed-timing.lock env URL="$1" OUT="$2" RUNS="${3:-3}" node "$here/startup.cjs"
