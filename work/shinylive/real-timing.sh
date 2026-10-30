#!/usr/bin/env bash
# Startup timing WITHOUT browser automation. Playwright/CDP makes every caught
# JavaScript exception expensive, and webR throws thousands while relocating
# package code, which inflated automated startup times about 2x. This script runs
# plain headless Chromium on a copy of the app page (timing.html) that clicks
# Analyze as soon as the app is ready and reports page-relative times as
# requests to /__timing, read back from the static server's log.
# Usage: real-timing.sh APP_DIR APP_URL SERVER_LOG [RUNS=3]
#   APP_DIR: exported app folder containing index.html (timing.html is written next to it)
#   APP_URL: URL of that folder; SERVER_LOG: stderr log of work/shinylive/serve.py
set -euo pipefail
app_dir="$1"; app_url="$2"; log="$3"; runs="${4:-3}"
chrome="${METAUI_CHROMIUM:?Set METAUI_CHROMIUM to an existing Chromium executable}"
python3 - "$app_dir" <<'PY'
import sys, pathlib
d = pathlib.Path(sys.argv[1]); html = (d / "index.html").read_text()
probe = """<script>
(function () {
  var t0 = performance.now(), ready = false, done = false, preview = false, takeover = false;
  function report(k) { fetch('/__timing?' + k + '=' + (performance.now() / 1000).toFixed(3) + '&id=' + location.hash.slice(1)).catch(function () {}); }
  var iv = setInterval(function () {
    var p = document.querySelector('#metaui-preview');
    var estimate = p && p.contentDocument && p.contentDocument.querySelector('.metaui-primary-value');
    if (!preview && estimate && p.contentDocument.readyState === 'complete' && estimate.getBoundingClientRect().height > 0) {
      preview = true; report('preview');
    }
    if (!takeover && document.documentElement.dataset.metauiLive === 'true') { takeover = true; report('takeover'); }
    var f = document.querySelector('#root iframe'); if (!f || !f.contentDocument) return;
    var w = f.contentWindow, d = f.contentDocument, go = d.querySelector('#go');
    if (!ready && go && !go.disabled && w.Shiny && w.Shiny.shinyapp && w.Shiny.shinyapp.isConnected() &&
        w.Shiny.shinyapp.$inputValues && Array.isArray(w.Shiny.shinyapp.$inputValues.outliers_z_scores)) {
      ready = true; report('ready'); if (!window.metauiPrecomputed) go.click();
    }
    var t = ready && d.querySelector('#effectestimate');
    if (t && t.innerText.indexOf('Source-reference') >= 0 && !done && (!window.metauiPrecomputed || takeover)) { done = true; report('results'); if (!window.metauiPrecomputed || takeover) clearInterval(iv); }
  }, 100);
})();
</script>"""
(d / "timing.html").write_text(html.replace("</body>", probe + "\n</body>"))
PY
for i in $(seq 1 "$runs"); do
  id="r$(date +%s%N)"; profile="$(mktemp -d "${TMPDIR:-/tmp/shinylive-speed/W2}/timing.XXXXXX")"
  flock "${TMPDIR:-/tmp/shinylive-speed/W2}/timing.lock" bash -c '
    "$1" --headless=new --no-sandbox --disable-gpu --no-first-run --user-data-dir="$2" "$3/timing.html#$4" >/dev/null 2>&1 & pid=$!
    for s in $(seq 1 600); do grep -aq "__timing?results=.*id=$4" "$5" && break; sleep 0.5; done
    kill $pid 2>/dev/null; wait $pid 2>/dev/null || true' _ "$chrome" "$profile" "$app_url" "$id" "$log"
  sleep 1; rm -rf "$profile" 2>/dev/null || true
  ready=$(grep -ao "__timing?ready=[0-9.]*&id=$id" "$log" | sed 's/.*ready=\([0-9.]*\).*/\1/' | tail -1 || true)
  results=$(grep -ao "__timing?results=[0-9.]*&id=$id" "$log" | sed 's/.*results=\([0-9.]*\).*/\1/' | tail -1 || true)
  preview=$(grep -ao "__timing?preview=[0-9.]*&id=$id" "$log" | sed 's/.*preview=\([0-9.]*\).*/\1/' | tail -1 || true)
  takeover=$(grep -ao "__timing?takeover=[0-9.]*&id=$id" "$log" | sed 's/.*takeover=\([0-9.]*\).*/\1/' | tail -1 || true)
  echo "{\"run\":$((${RUN_INDEX:-1} + i - 1)),\"preview_s\":${preview:-null},\"takeover_s\":${takeover:-null},\"ready_s\":${ready:-null},\"first_results_s\":${results:-null},\"loadavg_1m\":$(cut -d' ' -f1 /proc/loadavg)}"
done
