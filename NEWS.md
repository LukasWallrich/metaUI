# metaUI 0.2.0

* Add author-defined effect-size computations with explicit scale/variance contracts,
  consistent row selection, comparisons, provenance and workbook restoration.
* Add opt-in deterministic Bayesian normal-normal analysis, posterior median and
  central credible intervals, disclosed priors and heterogeneity-prior sensitivity.
* Share applied selections through build-specific validated URLs; uploaded datasets
  remain workbook-only, and restoration waits for browser acknowledgement.
* Extend declarative authoring to schema version 2, retaining version 1 behavior.
* Make package examples runnable without launching an app and prepare release checks.
* Optimise p-curve without changing results and reuse compatible fitted objects.
  Disclose session-local exact-selection caching and its opt-out for custom code.
* Validate and persist uploads, restore picker/missing-value choices, widen slider
  ranges, recompute descriptive z against the built reference, and disclose all
  filter exclusions and uploaded provenance. Reject inconsistent scale inputs.
* Report z-curve estimator refusals/warnings; correct Egger residual variance;
  restore p-curve process options and show unidentified singleton variance splits.
* Add a CC-BY empirical source-reference example and generated-app/reference tests.
* Record measured before/after performance and clean hosted Linux checks.
* Remove `inst/extdata/barroso2021.csv` because its source redistribution terms
  could not be verified; tutorials now use the attributed CC-BY Dannheim example.
  This is an explicit data-path removal, not a silent replacement of that analysis.
* Check Linux, macOS and Windows in CI, with a real generated-app Chromium
  smoke test; keep the hidden download handler active for programmatic downloads.
* Generated apps explain their state: result tabs say "No results yet" before the
  first analysis, a results strip names the data and selection behind the shown
  results, and filter edits not yet analysed are flagged until Analyze data is
  clicked. Models that were not estimated are listed beside the estimate plot with
  their reasons. A collapsible note derives scale, declared direction and
  aggregation assumptions from the data contract. Model code, estimates and
  downloads are unchanged.
* Authors can declare one `primary_model` (in `generate_shiny()` or the JSON
  config). Apps show it first as the authors' primary analysis, label other rows as
  explorations, and report why it is unavailable instead of substituting another
  model. The Dannheim example declares its source-reference row.
* Slider bounds round outward to a round tick interval with an explicit integer
  grid, fixing overlapping end labels (Shiny's fractional tick counts) and
  non-integer bounds such as 2000.992; a small script hides any remaining grid-label
  collisions in narrow sidebars or after uploads.
* Fix pre-existing bugs found in a cross-view QA pass: the outlier violin plot failed when no
  outliers were drawn and moderation slopes were rounded to "-0.00". Correlation summary plots label the
  back-transformed r scale.
* Generated apps include editable `www/metaui.css` and `www/metaui.js` (responsive
  tables and plots, WCAG AA text contrast, visible keyboard focus including sliders,
  a skip link to results, compact empty/error states).
* Replace shinyBS filter popups with expandable inline help, supporting keyboard,
  touch and Escape while preserving author-provided HTML and clickable links. Newly generated apps no longer require shinyBS.
* Repeat the selected sample summary on the Sample tab with separate Shiny output
  IDs backed by the same reactive summary, preserving the welcome dialog.
* Show forest row-limit messages as HTML before creating the plot output;
  validate the configured limit as a positive finite whole number.
* Document console blocking on interactive launch and the build-only alternative.
* Add a synthetic correlation example with an independent Fisher-z reference fit.
* Forest plots show observed effects on the displayed scale, compare available
  multilevel/RVE summary intervals, and export PDF, PNG and CSV (both scales in the CSV).
* Add practical-equivalence interval assessment with a reader-chosen symmetric
  bound (no default), strict containment, threshold values and model limitations.
  Bound changes do not refit models; downloads record the assessment and bound.
* No public release or tag is created.

# metaUI 0.1.2.9000 (correctness candidate)

* Use genuine per-effect IDs for all multilevel nesting; display labels may repeat.
* Keep optional p/N rows under default preparation, validate required inputs, and
  report excluded rows and derivations. Summed N is no longer inferred.
* Require explicit correlation variance scales and directional bias assumptions.
  Fit COR via Fisher z with delta-method variance; preserve ZCOR without a second
  transform; back-transform model summaries once. Other panels retain fitting scale.
* Report model applicability, failures, warnings, aggregation, and fitting values.
  Correct REML heterogeneity output and remove meaningless multilevel tau2.
* Add JSON `build_app()` and versioned schema. Nonempty output is refused by
  default; preserve hand edits by building a fresh destination.
* Generated apps check dependencies without installing packages. Record build
  versions and bundled attribution; replace archived waffle dependency with a
  categorical count bar chart and use an original SVG favicon.
* Correct the tutorial's Wald p calculation and Fisher-z labels.
* Add independent statistical references, headless server tests, and read-only CI.
* Correct slider rounding so default bounds retain all eligible rows.
* Reuse exact compatible multilevel/RVE fits in diagnostics. Session-local,
  three-entry exact-input caching speeds repeated selections; `cache_hit` and
  original fit timings are retained. Custom models default to no caching;
  `options$fit_cache_entries = 0` disables it. No cross-reader cache.
* Deduplicate identical p-curve noncentrality solves without changing numerical
  results or selection rules; restore process options after p-curve calls.
* Keep uploaded data in session state, validate their contract, clear cached
  results on upload, and reject stale globals when loading author model files.
* Report singleton-study variance splits as unidentified, and fix model plot
  ordering and Plotly tooltip warnings.
* Add a tiny CC-BY Dannheim mental-health pool with exact member provenance,
  a source-reference REML/Hartung–Knapp fit, and labelled explorations.
* Verify Linux CI, including fresh dependencies; add current/previous R matrix.
* Existing launch_app=FALSE remains supported. Agent skill, public deployment,
  paper draft, and runtime rewrite remain deferred.

# metaUI 0.1.2 (under development)

## Minor enhancements
* generate_shiny() has gained an `options` argument to allow for more customization of the app. Currently it allows setting a Shiny theme, a limit for the number of rows in a forest plot, and a threshold for switching from checkboxes to selection lists based on the number of moderator levels, with sensible defaults.
* prepare_data gained an argument `arrange_filters` to allow for the ordering of filters in the app. It now defaults to ordering them in the order they are passed into the function, but this can be changed to alphabetical or no ordering (in which case the order of columns determines their position)
* Added the ability to create an app without any filters/moderators
* Now shows "(Missing)" level only where there are any missing values in that filter/moderator - unless

## Bug fixes
* Corrected random intercept specification in rma.mv (and added sparse = TRUE to speed up model fitting)
* Fixed bug in describing moderators where "Other"-category already existed
* Fixed creation of code to install required packages and added check to ensure that filters are factors or numeric (#28)
* Waffle plots in sample description no longer run out of colors in the presence of 11 categories.

# metaUI 0.1.1

* Fixed issue where k was not displayed for moderators with spaces in names
* Simplified implementation of `generate_shiny()` - it is now always saved, either to provided path or to temporary files, and launched from file. This should increase robustness. Also, removed unnecessary `app.R` so that `globals.R` does not need to be called explicitly.
* Added a `NEWS.md` file to track changes to the package.
* Added explicit option to include or exclude NAs on filters and report NA share in Sample descriptives
* Added `Reset` button to reset all filters
* Added option to add popups to filter, by specifying `filter_popups` in `generate_shiny()`
