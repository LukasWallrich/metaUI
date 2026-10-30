# Reproduction code export

Generated apps offer two downloads beside the workbook: an R script and a Quarto
file. Both reproduce the selection behind the results shown (the last Analyze
data), namely its filters, missing-value choices, effect-size computation and
equivalence bound, and the model rows of the Summary tab. They share the gate of
the workbook download: the app must have analysed a selection with rows at least
once, and restorations must have finished.

The code reads the `dataset` sheet of the workbook from the same analysis, which
holds every row of the built or uploaded data, with explicit column types so that
identifiers keep their text. It checks that the workbook's `filters` sheet matches
the selection written into the code. It reads cells without trimming
whitespace and restores the app's factor levels, whose order model formulas can
depend on. It then applies the filters in the app's
order, the descriptive z-score range and the chosen computation. The model step
calls `metaUI_fit_models()`, the function the app calls. The code copies the app's
`models.R` unchanged, including author-defined rows, helper functions and
constants, and the definitions it reaches from `helpers.R`, `analysis.R`,
`computations.R`, `bayesian.R` and (only when a p-curve model is configured)
`dmetar_contributions.R`. The script
therefore follows the app's input checks, sign handling for directional models,
aggregation and back-transformation without needing metaUI. If `models.R` can no
longer be read or no longer yields the loaded specifications, the code contains
the loaded table instead and says that helper code is missing.

The header lists what is not reproduced: plots and diagnostic tabs, the
effect-computation comparison and the Bayesian prior sensitivity. It names the
author-defined rows, whose code may use random numbers or external state, and
notes that cached fits and other package versions can matter. The first code
section records the package versions of the running app and reports any
installed version that differs. A commented-out `groundhog::groundhog.library()`
call names the same CRAN packages with the latest publication date among the
installed versions; groundhog then installs the versions current on that date,
which match the app's versions where the app's library was up to date on that
date. The last section embeds the app's estimates,
intervals, k and status for every row and reports agreement within 1e-6 (or that
neither fitted a model, for an empty selection). The R script also accepts the
workbook path as its first command-line argument. The Quarto file folds the
copied functions and widens printed tables.

Files: `R/code_export.R` and its verbatim app copy `inst/template_code/code_export.R`
(checked by the template-sync test). `global.R` sources it and records the app
folder, from which the code reads `models.R` and the analysis files.

## Verification

All runs use a fresh `Rscript --vanilla` process in a directory containing only
the downloaded workbook and code. That process does not load metaUI.

- `tests/testthat/test-code-export.R` (85 expectations; the fresh-process runs are skipped on CRAN):
  - A synthetic app with a numeric filter with a missing value (missing values
    excluded), a categorical filter (three of four groups, with non-alphabetical
    levels and one level ending in a space), an alternative
    computation, a negative declared direction (p-uniform* is fitted on reflected
    effects), an author `models.R` with a helper function, a constant and a
    moderator model whose intercept depends on the factor level order, and a
    declared primary author row. The script selects the app's 9 of 16 effects and
    reproduces every row's status, estimate, interval, fitting-scale estimate and k
    within 1e-8. The same holds after replacing the data with an edited upload,
    whose script discloses the uploaded file. An empty selection after an earlier
    analysis yields a script that selects no rows, fits no models and reports
    agreement.
  - A correlation app (COR input with a Fisher-z alternative, Bayesian model
    enabled): the Quarto file's code (extracted with `knitr::purl`) reproduces all
    rows, including the r back-transformation and the Bayesian posterior median
    and credible interval. Where the Quarto CLI is installed, the file renders and
    the page reports agreement.
  - The Dannheim example: the exported script gives the source-reference
    REML/Hartung–Knapp row as −0.38356080 [−0.68781519, −0.07930642], k = 12, and
    agrees with all eight app rows (largest difference 4.6 × 10⁻¹², in the
    multilevel row; the others differ by at most 2.3 × 10⁻¹⁶). With the z-score
    range set to [−1, 1], it selects the app's 9 of 12 effects; its primary row,
    −0.13059630 [−0.22296964, −0.03822296], matches a direct
    `metafor::rma(method = "REML", test = "knha")` fit to those effects within 1e-8.
- `tools/browser-smoke.cjs` (CI browser job): in Chromium, the generated app's
  R script and Quarto downloads are saved, and the R script is run with `Rscript`
  against the downloaded workbook. It reproduces all seven rows for the built data
  (16 of 16 effects, with equivalence bound 1) and, after a workbook upload with
  restored filters, for the uploaded data (4 of 16 effects).

- groundhog (one manual run, 30 October 2026, not in CI): with the commented-out
  call enabled, the browser test's script installed the eight packages and their
  dependencies as of 16 September 2026 into an empty library (146 packages built
  from source, about 37 minutes on Linux), reported the one version that differed
  (readxl 1.5.0 from CRAN against the app's 1.5.0.1 from a binary repository) and
  agreed with all seven app rows.

Workbook values pass through xlsx, which can change the last binary digit of a
double; the observed differences above stay below 10⁻¹¹.

## Limits

The code reproduces the app's model rows with the same code and data, not an
independent analysis; `validation/reproduction.md` describes the independent
reference fits. A first analysis with no eligible rows has no workbook or code
download, as before. Results from other package versions are compared, not
guaranteed. The download does not include an `renv` lockfile or rendered HTML;
rendering would need Quarto on the app's server.
