## Release candidate: metaUI 0.2.0

This update fixes statistical/data contracts and adds opt-in Bayesian analysis,
author-supplied effect computations, shareable selections and accessible UI.

## Test environments

* Local Ubuntu 24.04, R 4.6.1: source/installed tests, offline vignettes, manual,
  R CMD check --as-cran.
* GitHub Actions: Linux release and oldrel-1, macOS release, Windows release,
  generated standalone Shiny app in Chromium. See the stacked PR checks.

## Notes

Packages used only by generated standalone Shiny apps are in Suggests.
generate_shiny() checks them before building and stops with an install command
if any is missing; tests and examples are skipped when they are absent.
Generated global.R checks them again, and dependencies.csv records versions.

The source/reference data licensing audit is bundled in inst/COPYRIGHTS; an input
with unverifiable redistribution terms was removed. Examples do not launch apps,
install dependencies, access the network or write to user directories.

This is preparation only; no CRAN submission has been made.

## CRAN incoming note

The manual cites original p-curve source URLs (p-curve.com/app4 and Supplement).
Their host returns HTTP 406 to automated checks. These are retained as original
source attribution; package examples/vignettes do not fetch them. Redirected links
were updated, and the Altman/Bland reference uses the Rd DOI macro.
