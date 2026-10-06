# Correlational meta-analysis example

Copy this directory into a fresh working folder and run `Rscript --vanilla build.R`.
It builds `correlation-app`; launch separately with `shiny::runApp("correlation-app")`.
The eight independent synthetic effects and their supplied raw-r variances are
original GPL-3-or-later examples, not empirical evidence or inferred variances.

`COR` requires `variance_scale = "r"`. Preparation transforms each r with `atanh`
and its supplied variance with the delta method `var_r / (1 - r^2)^2`. The build
prints an independent metafor multilevel REML reference fit on Fisher z; Summary
model estimates and interval endpoints are reported as r using `tanh` once.
Forest plots also display r and export both scales. Moderators, heterogeneity and
diagnostics retain the fitting scale (Fisher z); read the scale note before interpreting them. If your source already
supplies Fisher z and z-scale variance, use `ZCOR`, `variance_scale = "z"` and
those original values instead: no second transform is applied.

Direction is deliberately unspecified, so directional bias models are shown as
unsupported. The other default methods remain exploratory; successful fitting
alone does not establish that a bias adjustment is scientifically appropriate.
The app does not yet support odds ratios. Check `validation.json` before use.
