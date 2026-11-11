# Leaving out one study: fast route against full refits — 11 November 2026

Outlier diagnostics leaves out one study at a time. By default it holds the
variance components at their values for the full selection and recomputes the
average exactly (generalised least squares) without each study: the fast route.
Full refits re-estimate the variance components. This note compares the two on
the Diversity data (Wallrich et al., 2024: 646 studies, 2,628 effects).

Models: the paper's Correlated and Hierarchical Effects model (CHE: `rma.mv`,
`vcalc` with ρ = .6, REML, t tests with containment df) and metaUI's default
multilevel model (ML). The full data were run once with CHE; subsets of 8, 15,
30, 60 and 120 studies were drawn three times each (seeds `1000 × seed + size`)
and run with both models. Estimates are on the display scale (r).

Script: `leave-one-out/compare.R`. Results, one row per selection and model:
`leave-one-out/compare-full.csv` and `leave-one-out/compare-subsets.csv`. They
were produced at commit f29bfed, when an omission that could not be estimated
counted as no change in significance; the current script and app treat it as
missing. This affects only the significance counts of the three runs with
failed refits.

## Agreement

| Selection | Max abs. difference in estimate | Max abs. difference in CI bound | Largest change from a refit | Same most influential study | Rank correlation of changes | Significance marks: refit / disagreements |
|---|---|---|---|---|---|---|
| All 646 studies (CHE) | 0.00025 | 0.00038 | 0.0016 | yes | 0.9995 | 0 / 0 |
| 120 studies (×6) | ≤ 0.0018 | ≤ 0.0024 | ≤ 0.0054 | 5 of 6 | 0.96–1.00 | up to 53 / up to 5 |
| 60 studies (×6) | ≤ 0.0098 | ≤ 0.011 | ≤ 0.0085 | 3 of 6 | 0.87–0.99 | 0 / 1 |
| 30 studies (×6) | ≤ 0.0049 | ≤ 0.028 | ≤ 0.024 | 6 of 6 | 0.91–0.99 | 1 / 1 |
| 15 studies (×6) | ≤ 0.0051 | ≤ 0.031 | ≤ 0.026 | 6 of 6 | 0.87–0.98 | 0 / 0 |
| 8 studies (×6) | ≤ 0.026 | ≤ 0.051 | ≤ 0.034 | 2 of 6 | 0.31–0.98 | 2 / 0 |

Bound flags (bounds 0.02, 0.05 and 0.10) disagreed in 5 runs, in one study each.
Some refits failed: one deletion at 15 studies (ML), one at 60 (CHE) and four
at 120 (CHE). Those runs compare the deletions that both routes estimated; their
rank correlation and bound comparison are missing. The six runs per size are
three selections, each fitted with two models, not six independent datasets.

With all 646 studies the fast route is indistinguishable from refits for every
reader-facing purpose. With tens of studies the estimates stay within about
0.01, but rankings of small changes and significance marks near 0 can differ. In
one 60-study ML run, the largest fast-route error (0.0098) exceeded the largest
change from a refit (0.0074). With 8 studies the fast route picked the same most
influential study in only 2 of 6 runs. The app therefore refits small selections
automatically when that is quick, notes that the fast route can misstate
influence below 30 studies, and says that marks near 0 can change with a refit.
The fast route does not assess how heterogeneity changes; refits report the
prediction interval (or τ).

## Time

On a shared 4-core Ubuntu box under load (other R jobs running; refits ran three
in parallel), so the times are inflated:

- CHE fit, all 646 studies: 4.4–6 s alone, 11 s under load.
- Per-study pieces for the fast route: 3–10 ms; the fast route for all 646 rows: 1 ms.
- One CHE refit without one study: median 20 s under load (about 6 s alone); all
  646 refits: 14,827 s of CPU time.
- Refits of the subsets: median 1.1 s (8 studies) to 5.2 s (60 studies) each.

Later single-process timings showed that multithreaded OpenBLAS inflated a small
dense fit on this loaded box about 60-fold (8.3 s against 0.14 s with one
thread for 30 studies). The agreement results above do not depend on timing.
