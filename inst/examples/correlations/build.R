# Run from a fresh copy of this directory. Synthetic data, GPL-3-or-later.
library(metaUI)
x <- data.frame(study = LETTERS[1:8], r = c(.12, .28, -.08, .35, .18, .42, .05, .24),
                var_r = c(.010, .008, .014, .006, .009, .005, .012, .007))
d <- prepare_data(x, "study", "r", variance = "var_r", es_type = "COR",
                  variance_scale = "r", direction = "unspecified")
# Independent reference: conversion follows the declared r-variance contract.
y <- atanh(x$r)
v <- x$var_r / (1 - x$r^2)^2
reference <- metafor::rma.mv(yi = y, V = v,
  random = ~ 1 | metaUI__study_id/metaUI__effect_id, data = d,
  method = "REML", test = "t", sparse = TRUE)
print(data.frame(fit_z = as.numeric(reference$b), r = tanh(as.numeric(reference$b)),
                 lower_r = tanh(reference$ci.lb), upper_r = tanh(reference$ci.ub)))
generate_shiny(d, "Synthetic correlations (not research evidence)",
  save_to_folder = "correlation-app", launch_app = FALSE,
  citation = "Original synthetic metaUI example; GPL-3-or-later.")
# Launch separately: shiny::runApp("correlation-app")
