# Compare the same inputs/estimators in fresh processes, before and after changes.
# Usage: Rscript --vanilla tools/measure-optimisation.R before|after output.csv [library]
args <- commandArgs(TRUE)
if (length(args) < 2L) stop("Need before|after and output.csv")
if (length(args) == 3L) .libPaths(c(args[3], .libPaths()))
suppressPackageStartupMessages(library(metaUI))
ns <- asNamespace("metaUI")
set.seed(20261006)
records <- list()
record <- function(k, operation, iteration, work) {
  warnings <- character(); reason <- ""; status <- "ok"
  started <- proc.time()[["elapsed"]]
  result <- tryCatch(withCallingHandlers(work(), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
  }), error = function(e) { status <<- "failed"; reason <<- conditionMessage(e); NULL })
  records[[length(records) + 1L]] <<- data.frame(version = args[1], k = k,
    operation = operation, iteration = iteration, seconds = proc.time()[["elapsed"]] - started,
    status = status, reason = reason, warnings = paste(unique(warnings), collapse = "; "))
  write.csv(do.call(rbind, records), args[2], row.names = FALSE)
  result
}
for (k in c(50, 200, 1000)) {
  x <- data.frame(study = rep(seq_len(k / 2), each = 2), id = rep(1:2, k / 2),
    yi = rep(rnorm(k / 2, .3, .18), each = 2) + rnorm(k, 0, .12), vi = runif(k, .01, .05))
  d <- prepare_data(x, "study", "yi", variance = "vi", es_id = "id", direction = "positive")
  models <- get_model_tibble()
  run <- if (args[1] == "after") get("metaUI_fit_cache", ns)(3) else function(df, models) get("metaUI_fit_models", ns)(df, models)
  for (iteration in 1:3) {
    fits <- record(k, "same_selection_seven_models", iteration, function() run(d, models))
    # Diagnostics access after summary, as in the generated app.
    record(k, "heterogeneity_after_summary", iteration, function() {
      if (args[1] == "after") get("metaUI_reuse_fit", ns)(fits, models,
        get("metaUI_code_multilevel", ns), d, function() get("metaUI_multilevel_fit", ns)(d))
      else metafor::rma.mv(metaUI__effect_size, V = metaUI__variance,
        random = ~1 | metaUI__study_id/metaUI__effect_id, data = d, test = "t", method = "REML", sparse = TRUE)
    })
    png(tempfile(), width = 900, height = 650)
    record(k, "pcurve_fit_and_plot", iteration, function() {
      selected <- get("metaUI_pcurve_data", ns)(d)
      get("pcurve", ns)(selected, effect.estimation = FALSE)
    })
    dev.off()
  }
}
writeLines(capture.output(sessionInfo()), paste0(args[2], ".session.txt"))
