## Release submission: metaUI 0.2.0

This release consolidates all changes since 0.1.1: explicit statistical/data
contracts (including odds and risk ratios from log ratios or 2x2 counts, CHE
default multilevel models, small-sample corrections, and cluster-robust
moderation), opt-in Bayesian analysis via grid quadrature, leave-one-out
influence diagnostics, author-defined effect computations, build-time study
metadata, reproducible R/Quarto code export, shareable selection links,
experimental static (`shinylive`) export, an author design helper, and a
redesigned accessible UI.

## R CMD check results

0 errors | 0 warnings | 1 note (New submission; local `R CMD check --as-cran --no-manual` on Ubuntu 24.04, R 4.6.1).

## Test environments

* Local Ubuntu 24.04, R 4.6.1: `R CMD check --as-cran`, offline vignettes,
  full test suite.
* GitHub Actions: Ubuntu (R release and oldrel-1), macOS (R release), Windows
  (R release), plus generated standalone Shiny app tests in Chromium.

## Notes

* Packages used only by generated standalone Shiny apps are listed in `Suggests`.
  `generate_shiny()` checks them before building and stops with an install
  command if any are missing; examples and tests skip cleanly when they are
  absent. Generated `global.R` checks them again and `dependencies.csv` records
  versions.
* Bundled code, font, and dataset licences are documented in `inst/COPYRIGHTS`
  and `DESCRIPTION` (`Authors@R` and `Copyright: see file COPYRIGHTS`). The
  previous Barroso et al. (2021) CSV with unverifiable redistribution terms was
  replaced by a migration stub pointing to the licensed bundled examples.
* Examples do not launch apps, install packages, access the network, or write
  outside `tempdir()`.
* `man/pcurve.Rd` (internal helper adapted from `dmetar`) cites original
  `p-curve.com` attribution URLs, and `README.md` links to `https://groundhogr.com/`;
  those hosts return HTTP 406 to automated URL checkers (`curl`), though they
  load in a browser. Package code, examples, and vignettes do not fetch them.
  Two external `shinyapps.io` references in `README.md` occasionally exceed
  `urlchecker`'s 5-second timeout when cold-starting, but return HTTP 200.
