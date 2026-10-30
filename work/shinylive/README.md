# Shinylive exploration

This folder contains the static-export report (`report.html`), browser smoke and
startup/timing harnesses, a minimal upstream plot reproduction, and research
scripts comparing the metafor helpers with meta 8.5.0.

`evidence/` and `research/` contain point-in-time records of the specific runs
named in those files, including intermediate failures. Their recorded numbers
are not updated when the implementation changes. In particular, the older test
log skipped the opt-in export test, the PASS 1242 count belongs to that run, and
`research/validation.txt` describes an intermediate link-restore failure and a
force-click workaround. Current figures are in `report.html` and the PR
description: the validated suite at f1e546a had 98 tests and 1,620 expectations;
the opt-in export test passed separately.

The old `run-static-final.json` console entries include shinylive's relayed R
stderr (`preload error:`), including package-attachment and dataset messages,
and a `/favicon.ico` 404. The harness now records those separately as
`console_info`, retaining other console errors with their source URLs. Historical
JSON remains unchanged. Fresh review validation is recorded separately below.
The six existing `speed-final-real-timing.jsonl` rows retain every recorded value;
`pair` was added in chronological order (1, 2, 3) to identify alternating baseline
and new-site runs. `RUN_INDEX` now sets the first run number when invoking the
timing harness repeatedly.

`evidence/bundled-packages.csv` and `evidence/STATIC-NOTICE.md` are copies of
export artefacts, regenerated on 2026-10-30 from a fresh Dannheim export with this
branch's installed code. The research copies remain historical records.

## Reproduce from the repository root

Use R with metaUI dependencies, shinylive and archive available. The independent
comparison specifically requires **meta 8.5.0** and metafor; see
`research/README.txt` for configurable research output paths. Use the installed
Playwright module and Chromium executable already available on your machine;
set `METAUI_PLAYWRIGHT` and `METAUI_CHROMIUM` to their locations. Do not run
`playwright install` for these harnesses.

The following paths are disposable scratch directories. The exploration library
is an existing prerequisite supplying dependencies and meta 8.5.0; never install
this branch into it or a personal library. The site-library directory must exist
and be empty. Ports 8971 and 8972 must be free.

```sh
export R_LIBS_USER=/tmp/shinylive-speed/W2/lib:/tmp/shinylive-explore/lib
export R_LIBS_SITE=/tmp/shinylive-explore/emptysite
mkdir -p /tmp/shinylive-speed/W2/lib /tmp/shinylive-speed/W2/review
R CMD INSTALL --no-test-load -l /tmp/shinylive-speed/W2/lib .
cp -r inst/examples/dannheim /tmp/shinylive-speed/W2/review/example
(cd /tmp/shinylive-speed/W2/review/example && Rscript --vanilla build.R)
Rscript --vanilla -e 'metaUI::export_static("/tmp/shinylive-speed/W2/review/example/mental-health-app", "/tmp/shinylive-speed/W2/review/site", subdir="dannheim")'
```

On reruns, choose a fresh owned scratch subdirectory for the example and export;
copying into an existing example directory can nest the copy.

Start these in separate terminals, and stop those servers after validation:

```sh
python3 work/shinylive/serve.py /tmp/shinylive-speed/W2/review/site 8972 2>/tmp/shinylive-speed/W2/review/serve.log
Rscript --vanilla -e 'shiny::runApp("/tmp/shinylive-speed/W2/review/example/mental-health-app", host="127.0.0.1", port=8971, launch.browser=FALSE)'
```

Then run (with both browser environment variables set):

```sh
MODE=static URL=http://127.0.0.1:8972/dannheim/ OUT=/tmp/shinylive-speed/W2/review/smoke-static node work/shinylive/static-smoke.cjs
MODE=server URL=http://127.0.0.1:8971/ OUT=/tmp/shinylive-speed/W2/review/smoke-server node work/shinylive/static-smoke.cjs
bash work/shinylive/real-timing.sh /tmp/shinylive-speed/W2/review/site/dannheim http://127.0.0.1:8972/dannheim /tmp/shinylive-speed/W2/review/serve.log 1
R_LIBS_USER=/tmp/shinylive-explore/lib Rscript --vanilla work/shinylive/research/verify-metafor-independent.R
Rscript --vanilla work/shinylive/compare-workbooks.R /tmp/shinylive-speed/W2/review/smoke-server/workbook.xlsx /tmp/shinylive-speed/W2/review/smoke-static/workbook.xlsx
```

The smoke requires a restored notice, `Selected 11 of 12`, and analysis results
on the shared-selection page. Download validation and restore failures exit
nonzero. The independent 400-dataset check explicitly allows exactly one dataset
where both trimfill implementations fail to converge and three where meta's
trimfill fails to converge while the helper converges (datasets 128, 272, 382;
REML also fails to converge in both implementations on those same datasets). All other errors,
nonfinite comparisons, k0 mismatches and differences above 1e-8 fail validation.

## Evidence index

All historical files below were committed on 2026-10-30; the commit is the first
commit containing that file, not a claimed exact build SHA. Unless stated, the
exact producing build/command was not retained. Playwright/CDP startup measurements
inflate webR startup roughly twofold in the original experiment and must not be
compared directly with the automation-free `real-timing.sh` figures. Current runs
on a busy shared host may also take longer than the original laptop measurements.

`startup-lazy-loading.json` records the lazy-loading experiment in Playwright
Chromium. Its `first_analysis_s` is a click-to-results duration (about 27 s),
not an absolute time: the harness retained at its first commit, `5b45113`, sets
`s = Date.now()` immediately before the Analyze click and subtracts that value.
Thus readiness at about 23 s and first analysis taking about 27 s use different
origins by design, just like the other startup captures. Lazy loading moved work
to the first analysis; the exact experimental export/configuration was not retained.
This run is superseded by later preload/migration work; its numbers remain intact.

`package-load-times.txt` is an instrumented Dannheim webR startup before the meta
migration, captured with Playwright Chromium. `METAUI_T` marks application phases
(with R wall-clock timestamps); `METAUI_P` reports package load durations in seconds.
`pre` and `pre2` identify two preload passes; plain package names are the later
app package checks. The instrumentation command and exact build were not retained.
The explicit meta preload and ~41 s readiness belong to that old configuration.

`research/evidence/workbook-static-parity.txt` has a manually appended numeric
summary. Its claim excludes the two unsupported P-uniform star and Hedges-Vevea
rows with NA estimates/intervals; six numeric model rows were compared. Model
labels are already present in the current comparison script. The old file remains
a dated record. `helper-ordinary-summary.csv` was regenerated on 2026-10-30 from
retained `helper-all-comparisons.csv` after applying the ordinary-group filter to
the quantity list too; only the two spurious edge-only `unavailable` rows were
removed, and the recorded numeric comparisons are unchanged.

| File | Measurement / contents | First recorded commit | Harness | Status |
| --- | --- | --- | --- | --- |
| `evidence/STATIC-NOTICE.md` | Current export-generated package manifest / notice | `7be1143` (2026-10-30) | metaUI::export_static | Regenerated 2026-10-30, base f1e546a |
| `evidence/bundled-packages.csv` | Current export-generated package manifest / notice | `5b45113` (2026-10-30) | metaUI::export_static | Regenerated 2026-10-30, base f1e546a |
| `evidence/headless-static-forest.png` | headless static forest.png | `5b45113` (2026-10-30) | Playwright Chromium | Historical; superseded by current report and review validation |
| `evidence/headless-static-summary.png` | headless static summary.png | `5b45113` (2026-10-30) | Playwright Chromium | Historical; superseded by current report and review validation |
| `evidence/live-after-handover.png` | live after handover.png | `9e0bc7c` (2026-10-30) | Browser screenshot (exact capture command not retained) | Historical; superseded by current report and review validation |
| `evidence/live-summary-plot-filtered.png` | live summary plot filtered.png | `9e0bc7c` (2026-10-30) | Browser screenshot (exact capture command not retained) | Historical; superseded by current report and review validation |
| `evidence/package-load-times.txt` | Pre-migration package preload and app phase timings | `5b45113` (2026-10-30) | Playwright Chromium + temporary R instrumentation | Historical; superseded by current report and review validation |
| `evidence/precomputed-preview-desktop.png` | precomputed preview desktop.png | `9e0bc7c` (2026-10-30) | Browser screenshot (exact capture command not retained) | Historical; superseded by current report and review validation |
| `evidence/precomputed-preview-mobile.png` | precomputed preview mobile.png | `9e0bc7c` (2026-10-30) | Browser screenshot (exact capture command not retained) | Historical; superseded by current report and review validation |
| `evidence/run-server-bayes.json` | Browser functional smoke, downloads, tabs, link restore and timings | `5b45113` (2026-10-30) | Playwright Chromium | Historical; superseded by current report and review validation |
| `evidence/run-server.json` | Browser functional smoke, downloads, tabs, link restore and timings | `5b45113` (2026-10-30) | Playwright Chromium | Historical; superseded by current report and review validation |
| `evidence/run-static-bayes.json` | Browser functional smoke, downloads, tabs, link restore and timings | `5b45113` (2026-10-30) | Playwright Chromium | Historical; superseded by current report and review validation |
| `evidence/run-static-export_static.json` | Browser functional smoke, downloads, tabs, link restore and timings | `5b45113` (2026-10-30) | Playwright Chromium | Historical; superseded by current report and review validation |
| `evidence/run-static-final.json` | Browser functional smoke, downloads, tabs, link restore and timings | `9e0bc7c` (2026-10-30) | Playwright Chromium | Historical; superseded by current report and review validation |
| `evidence/run-static.json` | Browser functional smoke, downloads, tabs, link restore and timings | `5b45113` (2026-10-30) | Playwright Chromium | Superseded; predates successful link-restore fix; R stderr/favicon classified by current harness |
| `evidence/speed-baseline.json` | Startup/readiness and analysis timing; configuration named by filename | `d767048` (2026-10-30) | startup.cjs / Playwright Chromium | Historical; superseded by current report and review validation |
| `evidence/speed-final-real-timing.jsonl` | speed final real timing.jsonl | `9e0bc7c` (2026-10-30) | real-timing.sh; plain headless Chromium, no automation | Original paired reader-facing timing; pair labels added |
| `evidence/startup-cold-warm.json` | Startup/readiness and analysis timing; configuration named by filename | `5b45113` (2026-10-30) | startup.cjs / Playwright Chromium | Historical; superseded by current report and review validation |
| `evidence/startup-lazy-loading.json` | Startup/readiness and analysis timing; configuration named by filename | `5b45113` (2026-10-30) | startup.cjs / Playwright Chromium | Historical; superseded by current report and review validation |
| `evidence/startup-offline.json` | Startup/readiness and analysis timing; configuration named by filename | `5b45113` (2026-10-30) | startup.cjs / Playwright Chromium | Historical; superseded by current report and review validation |
| `evidence/t3-preview-mobile-link-restored.png` | t3 preview mobile link restored.png | `5b45113` (2026-10-30) | Browser screenshot (exact capture command not retained) | Historical; superseded by current report and review validation |
| `evidence/t3-preview-summary.png` | t3 preview summary.png | `5b45113` (2026-10-30) | Browser screenshot (exact capture command not retained) | Historical; superseded by current report and review validation |
| `evidence/webr-dependency-availability.csv` | CRAN versus wasm availability: 29 direct / 127 recursive dependencies | `5b45113` (2026-10-30) | R / export metadata | Historical availability snapshot (3 / 14 version differences) |
| `evidence/workbook-server-bayes.xlsx` | Downloaded workbook or server/static numeric parity comparison | `5b45113` (2026-10-30) | Playwright download; compare-workbooks.R for text | Historical; superseded by current report and review validation |
| `evidence/workbook-server.xlsx` | Downloaded workbook or server/static numeric parity comparison | `5b45113` (2026-10-30) | Playwright download; compare-workbooks.R for text | Historical; superseded by current report and review validation |
| `evidence/workbook-static-bayes.xlsx` | Downloaded workbook or server/static numeric parity comparison | `5b45113` (2026-10-30) | Playwright download; compare-workbooks.R for text | Historical; superseded by current report and review validation |
| `evidence/workbook-static-final.xlsx` | Downloaded workbook or server/static numeric parity comparison | `9e0bc7c` (2026-10-30) | Playwright download; compare-workbooks.R for text | Historical; superseded by current report and review validation |
| `evidence/workbook-static.xlsx` | Downloaded workbook or server/static numeric parity comparison | `5b45113` (2026-10-30) | Playwright download; compare-workbooks.R for text | Historical; superseded by current report and review validation |
| `research/evidence/bundled-packages.csv` | bundled packages.csv | `134882c` (2026-10-30) | R / export metadata | Historical; superseded by current report and review validation |
| `research/evidence/export-log.txt` | export log.txt | `134882c` (2026-10-30) | R / export metadata | Historical; superseded by current report and review validation |
| `research/evidence/funnel-after.png` | funnel after.png | `134882c` (2026-10-30) | R plot capture | Historical; superseded by current report and review validation |
| `research/evidence/funnel-before.png` | funnel before.png | `134882c` (2026-10-30) | R plot capture | Historical; superseded by current report and review validation |
| `research/evidence/helper-all-comparisons.csv` | meta 8.5.0 versus metafor helper audit | `134882c` (2026-10-30) | R equivalence audit + summarise-helpers.py | Historical; superseded by current report and review validation |
| `research/evidence/helper-mismatches.csv` | meta 8.5.0 versus metafor helper audit | `134882c` (2026-10-30) | R equivalence audit + summarise-helpers.py | Historical; superseded by current report and review validation |
| `research/evidence/helper-ordinary-summary.csv` | meta 8.5.0 versus metafor helper audit | `134882c` (2026-10-30) | R equivalence audit + summarise-helpers.py | Regenerated grouping from retained comparisons; see note above |
| `research/evidence/meta-8.5.0-baseline.csv` | meta 8.5.0 baseline.csv | `134882c` (2026-10-30) | R / export metadata | Historical; superseded by current report and review validation |
| `research/evidence/session.txt` | session.txt | `134882c` (2026-10-30) | R / export metadata | Historical; superseded by current report and review validation |
| `research/evidence/smoke-server.json` | Browser functional smoke, downloads, tabs, link restore and timings | `134882c` (2026-10-30) | Playwright Chromium | Historical; superseded by current report and review validation |
| `research/evidence/smoke-static-initial-failure.json` | Browser functional smoke, downloads, tabs, link restore and timings | `134882c` (2026-10-30) | Playwright Chromium | Historical; superseded by current report and review validation |
| `research/evidence/smoke-static.json` | Browser functional smoke, downloads, tabs, link restore and timings | `134882c` (2026-10-30) | Playwright Chromium | Historical; superseded by current report and review validation |
| `research/evidence/test-results.txt` | test results.txt | `134882c` (2026-10-30) | R / export metadata | Historical; superseded by current report and review validation |
| `research/evidence/timing.csv` | Paired startup measurements under high shared-host load | `134882c` (2026-10-30) | real-timing.sh; no browser automation | Historical; superseded by current report and review validation |
| `research/evidence/workbook-server-parity.txt` | Downloaded workbook or server/static numeric parity comparison | `134882c` (2026-10-30) | Playwright download; compare-workbooks.R for text | Historical; superseded by current report and review validation |
| `research/evidence/workbook-server.xlsx` | Downloaded workbook or server/static numeric parity comparison | `134882c` (2026-10-30) | Playwright download; compare-workbooks.R for text | Historical; superseded by current report and review validation |
| `research/evidence/workbook-static-parity.txt` | Downloaded workbook or server/static numeric parity comparison | `134882c` (2026-10-30) | Playwright download; compare-workbooks.R for text | Historical; superseded by current report and review validation |
| `research/evidence/workbook-static.xlsx` | Downloaded workbook or server/static numeric parity comparison | `134882c` (2026-10-30) | Playwright download; compare-workbooks.R for text | Historical; superseded by current report and review validation |

## Current review evidence

| File | Measurement / contents | Code / date | Harness | Status |
| --- | --- | --- | --- | --- |
| `evidence/review-2026-10-30-static.json` | Fresh static smoke; link restore asserted | base `f1e546a` (2026-10-30) | Playwright Chromium | Current review validation |
| `evidence/review-2026-10-30-server.json` | Fresh server smoke; link restore asserted | base `f1e546a` (2026-10-30) | Playwright Chromium | Current review validation |
| `evidence/review-2026-10-30-real-timing.jsonl` | Fresh shared-box startup timing | base `f1e546a` (2026-10-30) | real-timing.sh; no automation | Current review validation |
| `evidence/review-2026-10-30-workbook-parity.txt` | Fresh workbook numeric comparison | base `f1e546a` (2026-10-30) | compare-workbooks.R | Current review validation |
| `evidence/review-2026-10-30-independent.txt` | Independent 400-dataset numerical comparison | base `f1e546a` (2026-10-30) | verify-metafor-independent.R | Current review validation |
| `evidence/review-2026-10-30-static-v2.json` | Static smoke after timing-key rename | base `f1e546a` (2026-10-30) | Playwright Chromium | Final review validation |
| `evidence/review-2026-10-30-server-v2.json` | Server smoke after timing-key rename | base `f1e546a` (2026-10-30) | Playwright Chromium | Final review validation |
| `evidence/review-2026-10-30-workbook-parity-v2.txt` | Workbook parity with enforced tolerance/NA checks | base `f1e546a` (2026-10-30) | compare-workbooks.R | Final review validation |
| `evidence/review-2026-10-30-workbook-checks.txt` | Negative fixtures and custom-tolerance checks | base `f1e546a` (2026-10-30) | R fixture harness + compare-workbooks.R | Final review validation |

## Review validation, 2026-10-30

Fresh export: base f1e546a installed into W2/lib; copied Dannheim example rebuilt
with build.R and exported with `subdir = "dannheim"`. Generated manifest: 106
packages, no meta. The report's 127-dependency availability table is a separate,
earlier recursive-closure check, not the current export manifest.

The automation-free timing run on this shared box measured preview 0.254 s,
ready 38.621 s, handover/results 42.813 s at load average 7.96. These are new
host-specific figures, not replacements for the original laptop timing series.
The missing-iframe check exited 1 after its configured 200 ms poll window, with
a clear timeout; missing OUT fails before loading Playwright. The directory-URL
server check logged the actual 2,299-byte index.html response using HTTPStatus.
The changed report was rendered and visually inspected in the T3 preview.

## Timing keys and workbook tolerance

Current smoke `index_loaded_s`, `iframe_s`, `ui_visible_s` and `ready_s` are
seconds since the initial navigation began. Startup uses `iframe_s`, `ready_s`
and `first_results_s` on that same navigation-relative clock. Other smoke/startup
`*_s` fields are operation durations: analysis, downloads, upload, tab changes,
and link restoration (which starts a separate navigation). `start_epoch` is Unix
seconds at navigation; `heap_MB` and load average are not times. `real-timing.sh`
preview/ready/takeover/first_results fields are seconds since navigation, from
`performance.now()` in the page.

Historical smoke keys `index_loaded`, `iframe_created`, `ui_visible` and
`ready_to_analyse` are elapsed seconds from harness startup, just before browser
launch, rather than individual operation durations. They are retained as recorded,
including in the first review smoke captures. Historical startup keys use the
navigation clock for iframe/readiness; first_analysis_s is a click-to-results
duration, including in the lazy-loading capture (verified in the retained harness). All other historical smoke *_s fields are durations.

`compare-workbooks.R` fails on model/status/warning differences, differing NA
patterns or any estimate/CI difference above tolerance. Tolerance defaults to
1e-8; set it with a third positional argument or METAUI_WORKBOOK_TOLERANCE.
The Source-reference row must occur exactly once.

Workbook assertion checks also exercised an estimate mismatch, a custom tolerance,
a differing NA pattern, status/warning mismatches and a missing reference row;
all returned the expected pass/fail status. Both browser modes were rerun after
the timing-key rename; the v2 captures use the navigation clock described above.

The independent verifier completed successfully: 400 datasets, 396 paired
trimfill comparisons, one shared trimfill convergence failure (151), and three
meta-only trimfill convergence failures (128, 272, 382; both REML fits also fail
on those datasets). There were 288 imputing meta fits and zero k0 mismatches;
maximum paired differences were 1.78e-15 for trimfill and 3.55e-15 for REML/HK.
