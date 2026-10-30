Requirements and output paths
Run from the repository root with meta 8.5.0, metafor and the metaUI dependencies
installed. /tmp/shinylive-explore/lib supplies the frozen meta oracle; do not install
there. Create /tmp/shinylive-speed/W2/lib and an empty site-library directory at
/tmp/shinylive-explore/emptysite. Install this branch only into W2/lib.
metafor-helpers-equivalence.R accepts an output directory as its first argument
or METAUI_EQUIVALENCE_OUT (default /tmp/shinylive-speed/W2/equivalence).
summarise-helpers.py accepts that input directory and a summary output directory
as its first two arguments, or METAUI_EQUIVALENCE_OUT and METAUI_SUMMARY_OUT;
the summary default is /tmp/shinylive-speed/W2/helper-summary.
verify-metafor-independent.R accepts an optional CSV output filename as its first
argument or METAUI_INDEPENDENT_OUT; by default it prints results only.
The committed evidence below is a point-in-time record; reruns write to scratch.

Metafor app migration — 30 October 2026

Reproduce from the repository root:
  export R_LIBS_USER=/tmp/shinylive-speed/W2/lib:/tmp/shinylive-explore/lib
  export R_LIBS_SITE=/tmp/shinylive-explore/emptysite
  Rscript --vanilla work/shinylive/research/metafor-helpers-equivalence.R
  python3 work/shinylive/research/summarise-helpers.py

The script regenerates all 324 original datasets (seed 20261030), evaluates the
final editable helpers, and compares with the frozen meta 8.5.0 oracle from audit
2988718. evidence/meta-8.5.0-baseline.csv retains the oracle's numeric values,
statuses, warnings and error strings. This avoids repeatedly fitting an unchanged
oracle under extreme shared-machine load. A first direct rerun of meta and the
helpers reached simulation 100 with no ordinary-data mismatches before switching
to the retained oracle. Source snapshots and session information are in
/tmp/shinylive-speed/M/equivalence. No missing paired values count as a numeric pass.

Design
- metaUI_univariate: metafor ML/REML, test='knha', without ad hoc HK correction.
  With one complete study, normal intervals and tau2=NA match meta's fallback.
- metaUI_egger: three-study guard, meta's equal-precision error, suppressed DL
  initialization and regtest(model='lm'), with the same returned bias, sigma, t, p.
- metaUI_trimfill: attributed adaptation of meta 8.5.0's L0 loop. Keep the initial
  ML fit (and warnings), infer side by Egger, trim around the common-effect mean,
  average-rank ties, floor(max(0,L0+.5)), stop and return at 50 iterations, reflect,
  then refit ML/HK on ALL observed and filled effects. k includes filled effects.
  k<=2 warns 'Minimal number of three studies for trim-and-fill method' and
  returns NULL; app extraction retains its original failed reason.
- metaUI_funnel: metafor canvas, observed study-level effects, ML summary line,
  summary-centred normal 95% pseudo-CI and zero-centred grey p<.05 contours.
  evidence/funnel-before.png and funnel-after.png show Dannheim. Axis ticks,
  margins and panel framing differ; statistical content is preserved.
- meta is Suggests. Declared Imports drive defaults, About and dependencies.csv;
  explicit author meta:: / library(meta) / require(meta) / requireNamespace(meta)
  calls add meta and its >=7.0-0 check. Author model files are inspected including
  their helper definitions. Custom About package lists remain author-controlled.

Ordinary-data numerical comparisons (308 simulations + fixtures):
See evidence/helper-ordinary-summary.csv for the recorded maximum absolute
differences, finite pair counts and quantities for each model. This file is the
numeric source of truth; figures are not duplicated here. Five k=2 simulations
correctly return NULL / an insufficient-k object for trimfill/Egger.
All side choices agree. See helper-ordinary-summary.csv for tau2 and pair counts.

Remaining accepted deviations (only direct helper calls on invalid inputs)
All quantities, warnings, statuses and error strings are enumerated in
helper-mismatches.csv; helper-all-comparisons.csv retains every comparison.
- missing: same finite numbers, k, k0, side and statuses; metafor emits the extra
  '1 study with NAs omitted from model fitting.' in ML/REML and the initial ML fit
  for trimfill/Egger. Meta silently omits at initial fit and later warns in its
  diagnostics. Helpers retain the latter '1 observation(s) dropped due to missing
  values' diagnostic warning too.
- zero-variance: meta converts zero SE to NA, reducing k to 2. Metafor retains k=3
  for univariate fits and warns about non-positive variances and unavailable Q/I2/H2.
  REML: meta estimate .3875, CI [-1.71359111496183,2.48859111496183], tau2 .025;
  helper .242839507419567, CI [-.327478813499985,.813157828339118], tau2 .0282256739697034.
  ML: meta .336368273212227, CI [-1.12482637711373,1.79756292353818], tau2 1.55854049522405e-6;
  helper .181345835457173, CI [-.24995295354515,.612644624459496], tau2 .00683388834409184.
  Meta trimfill returns NULL with its minimum-three warning; helper errors in DL
  initialization. Meta Egger returns an insufficient-k object; helper has the same
  DL error: 'DL estimator cannot be used when there are non-positive sampling
  variances in the data.' These inputs cannot enter generated fits: prepared-data
  validation rejects nonfinite effects and non-positive variances before fitting.
- No remaining differences for identical/tied effects, equal precision, k=1/2,
  or the forced tau-estimation failure. Regression tests also compare the local
  iteration-limit return against meta with n.iter.max=1 and 50.

Validation results and timing are recorded in validation.txt.
