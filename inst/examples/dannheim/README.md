# Mental-health pool from Dannheim et al. (2025)

Dannheim I, Ludwig-Walz H, Kirsch H, Bujard M, Buyken AE, Richardson KM, and
Kroke A. *Effectiveness of leader-targeted stress management interventions:
A systematic review and meta-analysis.* Scand J Work Environ Health 51:265–281.
[Article, methods, and licence](https://www.sjweh.fi/article/4219).
DOI: 10.5271/sjweh.4219. Article adaptation: **CC-BY-4.0**;
[licence](https://creativecommons.org/licenses/by/4.0/). Example R code: GPL-3-or-later.

Install `metaUI` and its dependencies, copy this directory to a fresh working folder, then run:

```sh
Rscript --vanilla build.R
# Separate launch, no deployment:
Rscript --vanilla -e 'shiny::runApp("mental-health-app", launch.browser=FALSE)'
```

The 12 rows are the exact mental-health pool (`pool_1621`) from the read-only
ES Meta Benchmarks working snapshot of 6 October 2026, with member IDs/checksums
and its verified `reml_g` gate in `provenance.json`. The snapshot has no Git
metadata; checksums identify the inputs. Its column named `reported_r` carries
native g for this gate; it is not a correlation. We retain **original signed
Hedges g**, not converted r or debiased d. The source sign convention is negative
for improvement. No p-values or sample counts are mapped or inferred.

`se_g` retains the benchmark's native-scale CI-width derivation:
`(upper-lower)/(2*qnorm(.975))`; variance is explicitly `se_g^2`.
The source forest-plot inputs are rounded. This assumes normal 95% study CIs;
CI asymmetry from rounding is recorded, not silently corrected. The numerical
facts are adapted/subsetted; no source PDFs, prose, or full corpus are bundled.
The released benchmark compilation carries CC0, but article attribution is retained.

The source authors already composite measures/follow-ups and select arms before
pooling. Each row here is their mental-health composite for a different study;
we neither combine other outcomes nor reconstruct unreported covariances.
Independence across those studies follows the source pooling contract. GLS
aggregation has no effect on singleton studies, whatever its correlation setting.

The **last row**, source-reference REML/Hartung–Knapp, uses the existing `meta`
estimator with no ad hoc HK adjustment. Its g/interval match a direct `metafor`
REML/Knapp–Hartung fit on these inputs. This approximately reproduces the printed
result g=-0.38, CI [-0.69,-0.08]; it does not claim access to unrounded source data.
That model rejects repeated study IDs, including uploads. `models.R` contains
standalone editable R specifications, without requiring metaUI at app startup.

Other models/diagnostics are **explorations with different methods**, not source
reproductions or recommendations. A two-component multilevel variance split is
unidentified with one effect per study: only its total is interpretable here.
Directional bias models are unsupported because no extra selection-model
hypothesis is specified. RVE `small=FALSE` and sparse-data bias diagnostics can
be anti-conservative; warnings/failures remain explicit. No moderators from LLM
enrichment are included. This reanalysis is not endorsed by the source authors.

Z-curve is not estimable for this pool under the installed estimator: only three
of its 12 first-per-study normal Wald statistics fall in the significant fitting
range, while zcurve requires at least 10 there. The app displays the estimator's
reason rather than constructing a replacement estimate. P-curve likewise requires
an explicit direction and is not estimated here. These are optional diagnostics;
the source-reference model remains eligible.
