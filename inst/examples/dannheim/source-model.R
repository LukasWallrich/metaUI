# Source methodological reference on rounded extracted g/SE; GPL-3-or-later.
# One author-composited effect per study, no averaging across outcome pools.
source_model <- tibble::tibble(
  name = "Source-reference: REML / Hartung-Knapp (rounded inputs)", aggregated = FALSE,
  es = "mod$TE.random", LCL = "mod$lower.random", UCL = "mod$upper.random", k = "mod$k",
  code = 'if (anyDuplicated(df$metaUI__study_id)) stop("Source-reference requires one effect per independent study.")
    meta::metagen(TE = df$metaUI__effect_size, seTE = df$metaUI__se,
    studlab = df$metaUI__study_id, sm = "SMD", common = FALSE, random = TRUE,
    method.tau = "REML", method.random.ci = "HK")'
)
models_to_run <- dplyr::bind_rows(models_to_run, source_model)
