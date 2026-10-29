# Synthetic visual-review fixture. Run with an empty output directory argument.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
suppressPackageStartupMessages(library(metaUI))
x <- data.frame(study = rep(LETTERS[1:8], each = 2), id = rep(1:2, 8),
  yi = c(.1,.1,.5,.2,-.2,.3,.4,.4,.6,.2,-.1,.1,.2,.3,.1,.8),
  vi = seq(.01,.04,length.out = 16), year = 2001:2016,
  population = factor(rep(c("Clinical", "Community"), each = 8)))
d <- prepare_data(x, "study", "yi", variance = "vi", es_id = "id",
  filters = c(Year = "year", Population = "population"))
generate_shiny(d, "Mental health interventions",
  primary_model = "Random-Effects Multilevel Model", launch_app = FALSE,
  save_to_folder = args[1], date = "2026-10-29")
