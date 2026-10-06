# Correctness and performance follow-up (6 October 2026)

Development candidate 0.1.2.9001, branch `correctness-headless-rc`, draft PR #35.
This extends the historical 0.1.2.9000 record; it is not a substantial release.

## Scientific and implementation changes

Retain editable standalone R/Shiny apps. Exact-input session-local caching (bounded
three selections, custom-code opt-out) and reuse of compatible multilevel/RVE fits
avoid redundant calculations. P-curve deduplicates identical root-solving parameter
sets, preserving unmodified root tolerances and full original output. Tests compare
complete results with frozen pre-optimisation reference calculations.

Uploads persist per session, validate the source/fitting-scale contract, recompute
descriptive z against the built reference, restore missing/category choices and
report every successive filter exclusion. Empty saved category selections remain
empty on re-upload. Downloads retain applied filters even after controls change;
pending uploads block export. Automatic analysis waits for browser acknowledgement of restored
inputs. Egger residual SD is squared for a variance label. Singleton multilevel
variance splits are explicitly unidentified; estimator failures remain visible.

## Empirical example

`inst/examples/dannheim/` contains exactly 12 signed Hedges-g effects from the
mental-health pool of Dannheim et al. (2025), doi:10.5271/sjweh.4219, CC-BY-4.0.
Input membership, snapshot hashes, gate status, rounded CI-derived normal SEs,
source estimator and licence are documented in its provenance and README.
Benchmark files were read only; no LLM moderators, transformed dashboard effects,
full corpus or protected runs were copied. Source-reference REML/Hartung–Knapp
matches independent metafor calculations: g=-0.38356080, 95% CI [-0.68781519,
-0.07930642]. Published values are reproduced approximately because inputs are
rounded. Other methods are labelled explorations. No directional selection
hypothesis is inferred; z-curve's own sparse-input refusal is reported.

## Performance

See `performance-optimisation.md` and raw CSVs for fixture, version, repetition and
measurement details. At k=200 warm p-curve median falls from 1.610 to 0.1185 seconds;
at k=1000 from 7.9495 to 0.406 seconds. Reusing the same fit reduces heterogeneity
panel calculation from 1.010 seconds to below 1 ms at k=200. Chromium Summary
click-to-idle repeated-selection median falls from 2.561 to 0.294 seconds at k=200.
New selections still fit the same models: 2.394 to 2.140 seconds at k=200, and
6.931 to 6.971 seconds at k=1000. Cache hits are not faster estimation. These local
small-repetition measurements do not establish a universal latency guarantee.

## Review, checks and release gates

Opus 5.5 performed a full read-only code/scientific review and focused follow-ups
through the Claude Max subscription, with no permission denials. Parent review and
CLI JSON remain outside product files. The final source suite passes 206 assertions with no failures, warnings or skips.
The real Chromium upload round trips pass, including missing numeric values,
picker selections, automatic analysis and saved [-2,2] bounds despite an unrounded
outside effect. Forest rendering is exercised by the server test. Final checks and browser evidence are listed
in the private review report. GitHub CI checks release and oldrel-1. The initial
run installed dependencies in an empty R library on a fresh hosted Ubuntu Linux
runner; this is not a literal container or source-only dependency build.

Incremental metered API cost: US$0 (subscription tools only). No public app deployment,
release, tag, CRAN submission, main merge or third-party contact. The unresolved
Barroso source redistribution licence remains a release gate. Publication remains
conditional on independently created useful apps and documented research use;
repository metrics are not adoption. Literal container, Windows/macOS and source-only
builds are deferred. Resolve that licence (or replace the bundled dataset with a
compatible licensed fixture) before a substantial release; then seek independent
reuse without inventing an uptake claim.
