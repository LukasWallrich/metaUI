# metaUI 0.2.0

This release consolidates all changes since 0.1.1, including the unreleased
0.1.2 development versions.

## Breaking changes

* Packages used only by generated apps (DT, ggplot2, meta, plotly, psych,
  puniform, readxl, robumeta, shinycssloaders, shinyjs, weightr, writexl and
  zcurve) moved from Imports to Suggests. `generate_shiny()` names any that are
  missing, with the command to install them.
* `generate_shiny()` refuses a nonempty `save_to_folder` unless
  `overwrite = TRUE`, and `build_app()` always requires a fresh output folder, so
  hand edits to a generated app are not overwritten.
* Generated apps check their dependencies at startup but no longer install
  packages; `dependencies.csv` records the versions used for the build.
* Correlations must declare their variance scale, and directional bias analyses an
  explicit direction. COR is fitted via Fisher's z with a delta-method variance,
  ZCOR is not transformed twice, and summaries are back-transformed once.
* Multilevel models nest effects within genuine per-effect IDs; display labels may
  now repeat. Summed sample sizes are no longer inferred.
* Generated apps no longer use shinyBS or waffle: filter popups became inline help
  and the waffle chart became a categorical count bar chart.

## New features

* The bundled Coles et al. (2019) facial-feedback data (`inst/extdata/coles2019.csv`)
  include the source's outcome-type column `paq` (affective judgement versus
  emotional experience), so apps can filter by it.

* `build_app()` creates apps headlessly from a JSON configuration with a versioned
  schema (version 2 adds computations, Bayesian analysis and design; version 1
  behaviour is retained).
* `design_app()` previews colours, fonts, type size, spacing and corners and exports
  R/JSON settings; `app_design()` validates settings and contrast. Pass them via
  `generate_shiny(design = ...)` or the JSON `design` field. Apps built without a
  design keep the default appearance.
* Authors can declare one `primary_model`. Apps show it first as the authors'
  primary analysis, label other rows as explorations, and report why it is
  unavailable instead of substituting another model.
* Author-defined effect-size computations, with explicit scale and variance
  contracts, consistent row selection, comparisons, provenance and workbook
  restoration.
* Opt-in Bayesian normal-normal analysis (requires the suggested bayesmeta
  package): posterior medians, central credible intervals, disclosed priors and
  heterogeneity-prior sensitivity.
* Readers can share applied selections through build-specific, validated URLs.
  Uploaded datasets remain workbook-only.
* Practical-equivalence assessment of intervals against a reader-chosen symmetric
  bound (no default). Changing the bound does not refit models; downloads record
  the assessment and bound.
* Forest plots show observed effects on the displayed scale, compare available
  multilevel/RVE summary intervals, and export to PDF, PNG and CSV.
* `generate_shiny()` gained `options` (Shiny theme, forest-plot row limit,
  checkbox/selection-list threshold); `prepare_data()` gained `arrange_filters`.
  Apps can be created without any filters or moderators.

## Generated app interface

* The interface is reorganised around selecting evidence, running models and
  reading results. A status bar leads with selected and excluded counts, filter
  changes not yet analysed are flagged until Analyze data is clicked, and result
  tabs say when no analysis has been run. Routine instructions and repeated
  explanations were cut or moved into disclosures; scientific warnings,
  provenance and model distinctions remain visible.
* Models that were not estimated are listed with their reasons. A collapsible note
  derives scale, declared direction and aggregation assumptions from the data
  contract. Model applicability, failures, warnings and fitting values are
  reported, and z-curve refusals and warnings are shown.
* Generated apps include editable `www/metaui.css` and `www/metaui.js`: responsive
  tables and plots, WCAG AA text contrast, visible keyboard focus, and a skip link
  to results.
* Uploads are validated, kept in session state and clear cached results. Downloads
  contain the full current data, applied selections, selected summary and
  provenance, and wait for pending restorations.
* Slider bounds round outward to a round tick interval so default bounds retain all
  eligible rows and end labels no longer overlap.
* The effect-size list rounds effect sizes to three decimals and other numeric
  columns to two, and shows URL/DOI columns as links. A "(Missing)" level appears
  only where a filter has missing values.

## Bug fixes

* Corrected the random-intercept specification in `rma.mv()` (now with
  `sparse = TRUE`), REML heterogeneity output, and Egger residual variance.
  Meaningless multilevel tau² is no longer shown; singleton-study variance splits
  are reported as unidentified.
* Optional p and N columns no longer drop rows under default preparation; required
  inputs are validated, and excluded rows and derivations are reported.
* Categorical filter levels containing a double quote no longer stop the app from
  starting.
* A non-converging random-effects model behind the funnel plot and Egger's test
  now gives a clear message instead of a raw error.
* The outlier violin plot is standardised against the built dataset and no longer
  fails when no outliers are drawn; moderation slopes are no longer rounded to
  "-0.00"; all moderation-table rows are shown.
* Correct the tutorial's Wald p calculation and Fisher-z labels.
* Fixed describing moderators where an "Other" category already existed, and
  checking that filters are factors or numeric (#28).

## Performance

* P-curve deduplicates identical noncentrality solves without changing results.
* Compatible multilevel/RVE fits are reused across diagnostics, and a session-local
  three-entry cache speeds repeated selections. Custom models default to no
  caching; `options$fit_cache_entries = 0` disables it. Measurements are in
  [validation/performance-optimisation.md](https://github.com/LukasWallrich/metaUI/blob/main/validation/performance-optimisation.md)
  in the source repository.

## Testing

* New examples: the CC-BY Dannheim mental-health pool with a source-reference
  REML/Hartung-Knapp model, and a synthetic correlation example with an
  independent Fisher-z reference fit.
* Independent statistical references, headless server tests and a generated-app
  browser test; CI checks Linux (current and previous R), macOS and Windows.

# metaUI 0.1.1

* Fixed issue where k was not displayed for moderators with spaces in names
* Simplified implementation of `generate_shiny()` - it is now always saved, either to provided path or to temporary files, and launched from file. This should increase robustness. Also, removed unnecessary `app.R` so that `globals.R` does not need to be called explicitly.
* Added a `NEWS.md` file to track changes to the package.
* Added explicit option to include or exclude NAs on filters and report NA share in Sample descriptives
* Added `Reset` button to reset all filters
* Added option to add popups to filter, by specifying `filter_popups` in `generate_shiny()`
