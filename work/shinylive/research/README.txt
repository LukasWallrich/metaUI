Workstream B: avoiding meta in generated apps
30 October 2026; base d767048; meta 8.5.0, metafor 5.2.1.

Reproduction (from repository root)
  export R_LIBS_USER=/tmp/shinylive-speed/B/lib:/tmp/shinylive-explore/lib
  export R_LIBS_SITE=/tmp/shinylive-explore/emptysite
  Rscript --vanilla work/shinylive/research/meta-equivalence.R
  Rscript --vanilla work/shinylive/research/summarise-equivalence.R
All generated raw outputs, datasets and session information
are under /tmp/shinylive-speed/B/equivalence. The evidence/ folder here retains the
small comparison tables for review. Seed 20261030. The forced-nonconvergence case
sets maxiter=1 deliberately; other cases use package defaults.

Inventory (base d767048)
- R/analysis.R:9 and inst/template_code/analysis.R:9: default trim-and-fill code
  calls meta::trimfill(meta::metagen(... method.tau='ML', method.random.ci='HK',
  prediction=TRUE)). R/models.R uses that string and extracts TE.random,
  lower.random, upper.random, k (including filled studies).
- inst/examples/dannheim/models.R:18 repeats this editable trim-and-fill code.
  Line 51 adds the author source-reference metagen REML/HK row, with no ad hoc
  HK correction and a duplicate-study guard. Its reported fields are the same.
- R/template_app.R:1017: funnel meta_agg reactive calls metagen ML/HK with
  prediction=TRUE on study-level data. Line 1041 invokes metafor::funnel on a
  meta object: S3 dispatch reaches meta's registered funnel.meta method.
  Replacing its object changes the plotting method, too.
- R/template_app.R:1047: Egger calls meta::metabias(...k.min=3,method.bias='Egger').
  Table shows bias (labelled Intercept), residual sigma squared (labelled tau2),
  t, and rounded p. The audit also compares unrounded p.
- R/create_app.R:280-295: generated global.R calls requireNamespace on every
  declared Imports dependency, including meta. This eagerly loads meta even
  without a meta model. The following packageVersion('meta') check only reads
  metadata; it does not itself load the namespace.
- R/metaUI.R:60 is inside unused_imports(), a dummy declaration function that is
  never called by generated apps. It is not a runtime trigger.
- R/dmetar_contributions.R and inst/template_code/dmetar_contributions.R accept
  meta objects by class name, but use their fields or a TE/seTE/studlab data frame;
  they contain no meta calls or namespace loads. P-curve does not require meta.
- The package NAMESPACE does not import meta. No other installed direct metaUI
  dependency recursively depends on meta (Depends/Imports/LinkingTo scan).
  Editable author model code can, of course, explicitly call meta at any time.

Source-based interpretation
- meta::hetcalc delegates ML/REML tau estimation to metafor::rma.uni. For k>1,
  untruncated HK uses Q_RE/(k-1) divided by the sum of random weights and a
  t_(k-1) quantile. The matching metafor setting is test='knha', not test='t'
  or test='adhoc'. metafor floors a residual sum of squares <= machine epsilon
  to zero; meta calculates it directly. One-study behaviour also differs.
- meta trimfill defaults to ma.common=TRUE: trim about a common-effect centre,
  reflect extremes about the last trimmed centre, and refit ML/HK on ALL observed
  and filled effects. Both packages refit on filled data; using the original-data
  HK interval would be wrong. A direct random-effects metafor trimfill changes the
  trimming centres and the side-selection regression.
- meta chooses the side using Egger's multiplicative-dispersion LM (via metafor
  regtest(model='lm')); metafor trimfill on an ML object uses an ML meta-regression.
  A common-effect metafor trimfill then ML/HK refit is a closer candidate.
- meta L and metafor L0 use the same rank-sum expression for untied data, but meta
  uses rank()'s average ties and floor(max(0,L0+0.5)), whereas metafor uses first
  ties and round(L0). Thus the estimator label alone does not establish parity.
- meta warns and returns NULL for trimfill k<=2. The app then reports failed with
  reason 'Model extraction must return one value per field', retaining the warning
  'Minimal number of three studies for trim-and-fill method'. metafor rejects k=1
  but can fit k=2 when precision varies.
- meta's trimming loop stops at 50 iterations and returns the current result with
  no convergence error; metafor errors when its limit is exceeded (default 100;
  the matched candidate uses 50). Tau estimation failures are separate.
- meta's Egger implementation itself uses metafor regtest(model='lm'), but its
  guards, warning suppression, initial DL fit and insufficient-k return object
  need preserving. Using metafor's default regtest(model='rma') changes the test.

Documentation/source consulted (installed version source is authoritative here):
https://rdrr.io/cran/meta/man/trimfill.html
https://rdrr.io/cran/meta/src/R/trimfill.R
https://rdrr.io/cran/meta/man/metagen.html
https://wviechtb.github.io/metafor/reference/trimfill.html
https://wviechtb.github.io/metafor/reference/rma.uni.html
https://wviechtb.github.io/metafor/reference/regtest.html

Results and decision
324 datasets: 300 simulations (k=2..60, tau=0/.01/.2/1/3, both effect
signs, symmetric and selected tails, variance ranges exp(-18)..exp(6));
eight fixtures including raw/study-level helper and tiny, Dannheim, and three
correlation variants; 15 edge cases; one forced tau-iteration failure.
Meta imputed on the right in 130 simulated datasets and on the left in 132.
Five simulated k=2 datasets return NULL for trimfill, not numeric results.

For fixtures + simulations, maximum absolute differences:
                        estimate        lower CI        upper CI       k / k0
REML/HK                 2.22e-15        5.33e-15        5.33e-15          0
ML/HK                   2.66e-15        8.88e-15        8.88e-15          0
Direct ML trimfill      4.2344          4.13175         5.47561          27
FE trim then ML/HK      3.55e-15        1.71e-14        1.99e-14          0
Egger LM                0 (bias, residual variance, t, and p)
Finite paired outputs only; failures and missing outputs are separate, not passes.
summary.csv includes ALL edge cases; ordinary-summary.csv excludes edge and
forced cases. Difference columns are absolute differences, not rounded display
values. Every >1e-8 pair is in numeric-mismatches.csv, with a source-based cause.
Every warning/status/error-text difference is in behaviour-mismatches.csv.
Missing numeric values in both implementations are not asserted equivalent.

Across all 324 datasets:
- Direct trimfill has >1e-8 discrepancies in 211 datasets; max count difference 27,
  max tau2 difference 31.0143, and different inferred side in 36 paired datasets.
- FE then ML/HK has >1e-8 discrepancies in two degenerate identical-effect cases:
  identical-20: meta k=20,k0=0 versus metafor k=31,k0=11; identical-60 changes side.
  Its finite estimates/CIs agree within 1.99e-14, but warnings differ on 16 datasets
  and raw return status differs on 13. In sim-005 and sim-250 the omitted initial
  ML fit loses a local-maximum warning even though the final filled fit agrees.
- ML and REML/HK differ numerically only for zero-variance input, which is invalid
  under metaUI's prepared-data contract. All 300 simulations and fixtures match
  numerically. Warning differences occur for k=1, zero variance, and missing data.
  The k=1 extra metafor warning is app-visible; a wrapper would need to match it.
- Egger finite numbers match exactly in 309 datasets. Its insufficient-k object,
  equal-precision errors, perfect-fit warnings, and missing-data handling differ.
  Matching the numbers alone is insufficient. The deliberately forced tau failure
  errors in both; the candidate evaluates its base model before regtest to avoid
  regtest's nonstandard-evaluation error masking.
- Dannheim full-data source-reference max difference 1.11e-16; trimfill 2.22e-16;
  no imputation (k=12), hence this example alone would conceal trimfill differences.

No runtime estimator, author model file, package dependency, or generated code was
changed. Strict numerical AND warning/failure equivalence was not established.
No candidate app was built/exported: browser smoke, workbook comparison and paired
browser timing are conditional on an accepted replacement, so do not apply here.
Measured saving: none (no runtime change and no browser timing experiment).

Options for the parent workstream
1. Retain meta for trimfill and the author source-reference row. Loading it lazily
   for these default rows does not remove it from page-open-to-first-results time.
2. REML/HK and Egger are promising for a dedicated compatibility wrapper preserving
   guards, warnings, and app extraction behaviour, but replacing these alone leaves
   meta required for trimfill. This audit does not authorise silently changing it.
3. A local, attributed implementation of meta's exact trimfill algorithm using
   metafor only for tau estimation could be evaluated separately, preserving ties,
   side selection, iteration-limit returns and all warnings. That is additional
   scientific compatibility work; a direct metafor::trimfill substitution fails.

Validation: installed the unchanged package into /tmp/shinylive-speed/B/lib with
R CMD INSTALL --no-test-load -l. Requested installed-package test command:
FAIL 0, WARN 0, SKIP 1 (opt-in static export), PASS 1194.
The full audit finished evaluating every dataset and wrote both CSVs before fit
serialisation. Serialising full model formula environments proved unnecessarily
expensive under load; that save was stopped and the incomplete owned artifact
removed. The script now retains only plain numeric fit fields, avoiding captured
R environments. Results were summarised from the completed CSVs, with the forced
Egger failure re-evaluated after correcting NSE masking. No browser timing was run;
load average reached 116 during competing work.
