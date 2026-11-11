#!/bin/bash
# Runs the simulation validation from the package root; rerun-safe (skips finished conditions).
# Results go to validation/bayesian-grid/results/ (ignored by git); then run analyse.R.
cd "$(dirname "$0")/../.."
export R_PROFILE_USER=/dev/null
V=validation/bayesian-grid
BM='Filter(function(i) make_conditions()$k[i] <= 50, '
(timeout 3h Rscript $V/validate.R bayesmeta "${BM}seq(1, 412, 2))" > $V/bm-odd.log 2>&1) &
(timeout 3h Rscript $V/validate.R bayesmeta "${BM}seq(2, 412, 2))" > $V/bm-even.log 2>&1) &
(timeout 3h Rscript $V/validate.R grid all > $V/grid.log 2>&1; timeout 3h Rscript $V/validate.R ref all > $V/ref.log 2>&1) &
wait
