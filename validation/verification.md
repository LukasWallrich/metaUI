# Local verification record — 5 October 2026

Base: ffddd5a. Feature branch: correctness-headless-rc. No push, PR, release,
public deployment, CRAN submission, source-project edits, or contacts occurred.
Deferred: agent skill, benchmark apps, static analysis viewer, paper, runtime rewrite.

Linux baseline: Ubuntu 24.04.4, R 4.6.1, Intel i5-7600T, four cores. Fresh package
installation initially failed on ten unavailable Imports. Compatibility fixes were
needed for old system data.table/gsl builds under R 4.6.1; GSL development headers
were installed and compatible source packages installed into the scratch overlay.
The original waffle dependency needed its archived 1.0.2 source. The candidate
uses ggplot2 categorical bars instead. Existing compatible dependencies were reused;
this was not a fully clean dependency container. The isolated scratch tree occupies
about 69 MB, excluding system headers/browser binaries already available. An initial
ABI repair attempt honoured the user's Rprofile and installed data.table/plotly in
the user's R library; subsequent repairs used --vanilla and the explicit scratch
library. No existing source/data work was overwritten or removed.

Baseline installation and generated-app load/summary smoke passed after dependencies.
Baseline check (built without vignettes): two warnings from skipped vignette outputs,
three notes (unstated functions/variables, unused declared import, Rd braces).
Baseline runtime also showed meta argument deprecations and absent-column/Plotly
warnings. The final candidate full build/check with --no-manual reports Status OK;
PDF manual checks and remote CI were not run. The candidate fixes the check notes,
builds both vignettes, and uses the current meta argument names. Tests contain 20
blocks, including independent metafor/robumeta fits on duplicated-effect fixtures
and bundled Barroso Fisher-z data; scale transformations/backtransforms; explicit
sidedness; GLS covariance reference algebra; invalid/optional inputs; actual p-curve
statistic reflection; categorical/numeric generated-server output; conflict refusal
and custom model replacement; literal metadata; factor IDs; and slider membership.
Two known Plotly tooltip warnings remain in testServer tests; they do not indicate
missing models or failed tests. Browser results have no JavaScript/page errors.

The browser check loads a fresh generated app, dismisses its welcome dialog, clicks
Analyze, verifies estimates and unsupported reasons, and opens its contract page.
The default synthetic app keeps all 12 effects after the rounding fix. Its source
contains no install.packages. The tiny data are original GPL synthetic fixtures,
not benchmark pools. Barroso data are bundled preexisting data checked against
independent R calls; these are method-specific reference checks, not a claim to
reproduce every original published estimand. Benchmark instructions/gates were read,
but no benchmark data or pipeline files were consumed or changed.

Performance records use a fixed seed and single first/warm calls. Warm fit totals:

| Effects | Baseline, seven models | Candidate, five eligible models | Candidate, seven with explicit positive direction |
|---|---:|---:|---:|
| 50 | 1.279 s | 1.215 s | 1.461 s |
| 200 | 2.219 s | 1.911 s | 3.494 s |
| 1000 | 5.947 s | 3.130 s | 6.565 s |

These are separate runs with normal system contention, not speed-up evidence.
At k200 the directed summary render adds about 0.3 s; the individual forest adds
about 0.6 s. All seven directed model fits and all 36 panel timing calls succeeded.
Estimator warnings are retained in panel CSVs, including z-curve's warning about
small numbers of statistics. At k1000 the app refuses an individual forest above
its 200-effect limit. P-curve's k1000 warm call took 8.292 s; p-uniform* took about
3 s, and the diagnostics refit the multilevel model. This would justify reusing fits,
caching by filter state, and retaining lazy hidden panels in a later bounded pass.
There is no measured justification here for changing runtime or estimators.

Before public release: validate a fresh dependency-container install and CI on
supported R versions, audit all bundled source deposits' redistribution terms,
and check independent author reuse with the stated scale/dependence assumptions.
Publication remains conditional; see publication-readiness.md. No artificial delay
or star-count criterion substitutes for demonstrated usefulness.
