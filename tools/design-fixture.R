# Standalone fixtures for tools/design-browser.cjs; synthetic values only.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, !dir.exists(args[1]))
suppressPackageStartupMessages(library(metaUI))
x <- data.frame(study = rep(LETTERS[1:8], each = 2), id = rep(1:2, 8),
  yi = c(.1,.1,.5,.2,-.2,.3,.4,.4,.6,.2,-.1,.1,.2,.3,.1,.8),
  vi = seq(.01,.04,length.out = 16), year = 2001:2016,
  population = factor(rep(c("Clinical", "Community"), each = 8)))
d <- prepare_data(x, "study", "yi", variance = "vi", es_id = "id",
  filters = c(Year = "year", Population = "population"))
for (name in c("light", "dark")) {
  design <- if (name == "light") app_design(accent = "#69429A", font = "georgia", text_size = 18, spacing = "compact", radius = 0)
    else app_design(accent = "#90CAF9", text = "#F7F9FC", background = "#131C27", surface = "#202C3A", spacing = "compact", radius = 4)
  generate_shiny(d, "Mental health interventions", design = design,
    primary_model = "Random-Effects Multilevel Model", launch_app = FALSE,
    save_to_folder = file.path(args[1], name), date = "2026-10-29")
}
