# Correctness candidate: reproduce locally

Use Linux R >= 4.1 and a separate library. On this box, R 4.6.1 requires
current builds of compiled dependencies; the older Ubuntu R packages for
data.table/gsl cannot load. Existing compatible packages may be reused read-only.
For a fully clean install, install Imports plus testthat/knitr/rmarkdown in a
fresh library. No app installs packages at startup. `dependencies.csv` records
build versions; it is a manifest, not a lockfile or guarantee of identical future fits.

```sh
export R_LIBS_USER=/home/lukas/work/metaui-rc/library:/home/lukas/R/library
Rscript --vanilla -e 'roxygen2::roxygenise()'
R CMD INSTALL --library=/home/lukas/work/metaui-rc/library .
Rscript --vanilla -e 'library(metaUI); testthat::test_dir("tests/testthat")'
R CMD build .
R CMD check --no-manual metaUI_*.tar.gz  # the archive version matches DESCRIPTION
```

Copy `inst/examples/tiny.csv` and `tiny.json` into a fresh working folder, then:

```sh
Rscript --vanilla -e 'metaUI::build_app("tiny.json")'
# Separate action: launches and blocks the console until stopped.
Rscript --vanilla -e 'shiny::runApp("tiny-app", host="127.0.0.1", port=7862, launch.browser=FALSE)'
```

Building does not launch, deploy, or publish. Paths in JSON resolve relative to
the JSON file. A nonempty destination is refused. Build a new folder and compare
changes with hand-edited files; `generate_shiny(overwrite=TRUE)` is a deliberate
replacement of generated files only, without deletion of unrelated files.
The JSON builder has no overwrite switch. App R files remain standalone/editable.

The tiny data are synthetic GPL-3-or-later fixtures. Study IDs define independent
clusters, with two distinct effects in each; equal labels/values do not identify
effects. They are not a paper reproduction or benchmark dataset. No benchmark
project data were imported, modified, or copied.

## Scientific contract

`prepare_data()` maps study, effect, variance/SE, and optionally effect ID, p, N,
and filters. A missing SE is explicitly derived as sqrt(variance), and a missing
variance as SE squared. Supplied SE and variance must agree. Missing/invalid
required inputs are excluded with original row numbers/reasons under the default;
`na.rm=FALSE` errors on invalid required inputs. `na.rm=TRUE` deliberately drops
missing supplied inputs, including mapped p/N; unmapped placeholders are ignored. Optional invalid p/N are set to missing and
counted; default preparation never deletes rows for them.

SMD is fitted/displayed as SMD. COR requires `variance_scale="r"`; r becomes
atanh(r), with delta-method var(z)=var(r)/(1-r^2)^2. This is an approximation,
not the independent-Pearson rule 1/(N-3). No sampling design is guessed.
Prefer externally validated z/vi as ZCOR with `variance_scale="z"` when available.
ZCOR is never transformed again. Model summary estimates and limits are tanh
back-transformed to r. Forest plots now show r as well and export both scales;
raw data, diagnostics and moderators remain on the labelled fitting scale. Variance components remain z-squared.
Other metrics are explicitly unsupported in this candidate.

The primary multilevel model retains REML with t inference and **diagonal V**;
it models study/effect random intercepts, not known sampling covariance.
This t inference is not a Knapp–Hartung adjustment. RVE retains robumeta's
correlated-effects default rho=.8 and small=FALSE. The author must judge these
assumptions for their data; agreement with direct R calls does not establish
scientific appropriateness for any particular paper.

Study-level models use GLS aggregation with correlation=.6 by default, configurable
through `options$correlation_dependent` in [0,1). Aggregation assumes a common
within-study sampling correlation. Aggregated p/N are unavailable, not inferred.
The sample overview no longer sums N using an unverified independence rule.
Multilevel k counts effects, RVE/aggregated k counts studies, and trim-and-fill k
includes imputed effects. Collapsed rows are reported separately from exclusions.

P-uniform* and Hedges–Vevea retain right-sided selection assumptions. They require
`direction="positive"` or `"negative"`; the latter reflects input effects and
returns estimates/intervals on the original sign. Changing sidedness is a change
of model, not a generic sign-invariance requirement. No pooled-sign choice is made.
PET/PEESE require variable precision and positive residual degrees of freedom.
Other sparse-data/convergence issues are delegated to the estimator and surfaced
as warnings or failures, without invented universal small-k thresholds.
P-curve requires explicit direction and SMD. Selection takes the first effect per
study before excluding direction-contrary/zero effects, with counts. The evidential
value panel needs no N; optional effect estimation needs N > 2 on the remaining
selected effects. This selection needs author justification against hypotheses
and designs (https://www.p-curve.com/guide.pdf). Z-curve uses
normal Wald |effect/SE|, not supplied p. Trim-and-fill retains data-driven side selection by meta; it is not governed by
the direction setting. Moderator models retain ML and are labelled accordingly.
First-effect selection alone cannot prove
independence across studies. All these analyses are exploratory.

`validation.json` records preparation, scales, direction, derivations, aggregation,
and build-time model eligibility. Generation rejects stale preparation attributes
after row subsetting; re-prepare selected original input rows. Prepared uploads
are checked for unique IDs, valid variance/SE, and uniform scale/direction. It does not assert fits succeeded. The runtime
result table reports status, reason, estimator warnings, fitting values, and fit
time; downloadable summaries retain those fields. `COPYRIGHTS` records bundled
code/data attribution. Full source-data/asset licence checks remain a release gate.

## Timings

```sh
Rscript --vanilla tools/measure.R candidate candidate-timings.csv
# The baseline must be installed separately from git archive ffddd5a.
Rscript --vanilla tools/measure.R baseline baseline-timings.csv /path/to/baseline/library
```

Seed 20261005; 50/200/1000 effects, two per study. First/warm calls are in one
fresh process per version: cold includes first namespace/model load at k50, not
an independently cold process for every k. This is a bounded local fit/render
measurement, not an end-to-end network benchmark. The baseline uses its original
models; unspecified direction in the candidate intentionally excludes directional
fits. Comparison of all-model totals is therefore not a speed-up claim.
At k1000 the individual forest is explicitly limited to 200 effects by the app;
no 1000-effect forest render is claimed. Forest timings include a fresh RVE fit,
whereas the app reuses its cached fit, so they are conservative upper bounds. Synthetic data and known fitting-scale
assumptions allow timings, not estimator validation from performance alone.

The explicit CSV categorical mapping is `filters = list(Region="region")` plus
`categorical_filters = list("region")`. No text field becomes a moderator or
category without mapping; levels are sorted with radix order. Numeric filters are
unchanged. Mapping, CSV basename/checksum, and conversion rules enter validation.
Custom model code remains editable, but its extra dependencies must be declared
and installed by the author; the manifest covers the package's default models.

For all seven models on the explicitly positive synthetic scenario:

```sh
Rscript --vanilla tools/measure.R candidate-directed directed-timings.csv
Rscript --vanilla tools/measure-panels.R panel-timings.csv
```

Panel timings cover the local generated algorithms for REML heterogeneity, ML
numeric moderation, funnel, Egger, direction-screened p-curve, and z-curve without
bootstrap. These are separate fit/render measurements, not a sum of hidden panels
or a promise about click-to-result latency. Warm timings are single repeats, without
uncertainty intervals; normal system contention and timer resolution affect them.
Each panel timing includes opening a PNG device, and funnel and Egger each refit
their shared `metagen` object, which the app computes once; they are therefore
conservative upper bounds for the app's per-panel work.

The committed single-run records are `validation/timings-baseline.csv`,
`timings-candidate.csv` (unspecified direction), `timings-directed.csv`
(explicitly positive direction), and `timings-panels.csv`; the commands above
write the same schema to new scratch files. `timing-session.txt` records versions.
The full local check uses `--no-manual`; PDF manuals and remote CI were not run.
The initial milestone had two known Plotly tooltip warnings (fixed in the 6 October follow-up); model tables,
reference fits, and browser rendering pass. Browser verification also confirms
that corrected default slider bounds keep all 12 effects in the example.
The package was installed into a fresh Linux library overlay; compatible existing
dependencies were reused read-only. A container with all dependencies installed
fresh remains a release gate, rather than a claim from this run.

## Authorised follow-up — 6 October 2026

The earlier records above describe the first milestone. This follow-up adds one
CC-BY empirical pool and bounded performance changes; no public app deployment.
Copy all files from `inst/examples/dannheim` to a fresh directory and run:

```sh
Rscript --vanilla build.R
Rscript --vanilla -e 'shiny::runApp("mental-health-app", launch.browser=FALSE)'
```

Building and launching remain separate. The example uses the existing R entry
point for trusted editable custom model code. It deliberately does not expand the
JSON schema with executable code. Its last model row is a source-reference
REML/Hartung–Knapp fit on rounded g/SE inputs; preceding default rows/diagnostics
are different-method explorations. See the example README and provenance.json.
No optional p/N is supplied. Exact eligible membership, source sign/scale,
CI-width SE rule, gate status, and source-file checksums are recorded.

`fit_cache_entries` is the one additional presentation/performance option in JSON
and `generate_shiny`: integer 0–3. Default models retain three exact recent fit
selections per session; custom model code defaults to zero. It must remain zero
for random fits or code depending on external state. Cached failures/warnings and
original times remain in downloads; the table/UI disclose cache reuse. Uploads
persist and clear the cache. Runtime/helper code edits require restarting the app.
See performance-optimisation.md for same-estimator before/after commands/results.

The final follow-up package is `metaUI_0.1.2.9001.tar.gz`. Use that filename in
`R CMD check --no-manual` after building this version. For uploads, original input
and fitting effect/variance fields must agree with the built transformation;
re-prepare raw edited inputs when needed. Descriptive outlier z is recalculated
against the built dataset's fixed mean/SD, with disagreements reported. It is not
a source test statistic. Uploaded results are labelled persistently, and restored
filter exclusions are counted. Missing-value choices are saved; older workbooks
without those choices default to include missing values, explicitly reported.
Arbitrary scientifically mislabelled inputs cannot be validated from numbers
alone: the author's effect-scale/dependence contract remains necessary.
