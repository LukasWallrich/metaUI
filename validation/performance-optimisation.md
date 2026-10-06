# Bounded performance optimisation — 6 October 2026

The baseline for this comparison is correctness commit **3d95199**, not the
original version with scientific/schema defects. Both runs use the same seeded
synthetic datasets, seven estimators, explicit positive direction, dependency
versions, and 50/200/1000 effects (two per independent study). Each runs in its
own fresh R process, sequentially on this Ubuntu/i5-7600T box. First calls include
namespace warm-up at k50; later k values are not independently cold processes.
Warm values below are medians of two further calls, not stable population estimates.

| Effects | P-curve before | P-curve after | Heterogeneity after summary, before | After compatible fit reuse |
|---|---:|---:|---:|---:|
| 50 | 0.433 s | 0.068 s | 1.092 s | <0.001 s |
| 200 | 1.610 s | 0.119 s | 1.010 s | <0.001 s |
| 1000 | 7.950 s | 0.406 s | 1.441 s | <0.001 s |

P-curve timings include fit and plot. Profiling the pre-optimisation 200-effect
scenario (100 selected studies) attributed roughly 86% of total sampled time to
`uniroot`: the same distribution parameters were solved once per study at each
power-grid point. The changed implementation solves each distinct exact parameter
row once per call and expands it in original order. Hexadecimal keys preserve
floating-point identity; intervals/tolerances and selection rules are unchanged.
The full returned lists match baseline snapshots exactly, including the power CI
case. The baseline's no-half-curve case raises an error; it remains an explicit
panel failure rather than being converted into invented statistics. Process
options are restored after both successful and failed p-curve calls.

A summary's compatible REML multilevel/RVE object is reused for heterogeneity and
the individual forest. Reuse requires exact default code, nonaggregated data,
no direction reflection, successful extraction, and identical prepared inputs.
Otherwise the original diagnostic fit is calculated. The sparse multilevel
estimator and all modelling assumptions remain unchanged.

Repeated identical seven-model calculations at k200 went from a warm median
**2.115 s to <0.001 s**, and k1000 from **6.826 s to <0.001 s**, excluding plot
rendering. These are cache hits, not faster estimation. First new-selection fits
remain similar: k200 **2.394 vs 2.140 s**, k1000 **6.931 vs 6.971 s**; single-run
noise does not justify claiming a new-selection speed-up. The 1000-effect summary
still spends seconds on the configured estimators; no runtime rewrite follows
from this measurement.

The three-entry LRU is scoped to one Shiny session. Keys compare the complete
dataframe/attributes, full model specifications/code, aggregation correlation,
and method with `identical()`. No hash-based/cross-user cache is used. Uploads
clear it and persist in session state; changed uploads/variances/direction/code
are tested as misses. Cached table failures/warnings and original `fit_seconds`
remain visible/downloadable; `cache_hit` and calculation time disclose reuse.
Custom models default to no cache. Use `options=list(fit_cache_entries=0)` to
disable; optional 1–3 explicitly opts deterministic custom models in. External
state/random models must keep it disabled. Edits to standalone R helpers need
an app restart. Hidden Shiny outputs keep their normal suspension behaviour.

Raw records: `timings-optimisation-before.csv`, `timings-optimisation-after.csv`,
and `optimisation-session.txt`. Browser response times are recorded separately;
fit/plot components cannot be added as a claim of click latency. The individual
forest retains its 200-effect cap.

```sh
# Before installed from git archive 3d95199 into a separate library:
Rscript --vanilla tools/measure-optimisation.R before before.csv /path/to/preopt-library
# Candidate installed into a separate library, same dependencies:
Rscript --vanilla tools/measure-optimisation.R after after.csv
```

Actual headless Chromium over the tailnet, 200 effects and all seven models,
Summary tab click-to-Shiny-idle (one session per version): initial **3.106 vs
2.975 s**, then repeated-click median **2.561 vs 0.294 s**. Both fit and visible
summary rendering are included. The QRP/PB plots also loaded without JavaScript
or output errors. Three clicks per version provide a useful local smoke/timing
check, not uncertainty bounds or a guarantee under network/server contention.
Records: `timings-browser.csv`. New selections still need their original fits.
