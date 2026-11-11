# Bayesian fit latency (grid quadrature); see validation/bayesian-grid.md. Synthetic data only.
# Usage, from the package root: Rscript --vanilla tools/benchmark-bayesian.R output.csv
args <- commandArgs(TRUE)
if (length(args) != 1L) stop("Usage: benchmark-bayesian.R output.csv")
source("R/bayesian.R")
set.seed(90)
rows <- do.call(rbind, lapply(c(20, 100, 646, 2000), function(k) {
  s <- runif(k, .1, .4); y <- rnorm(k, .3, sqrt(.3^2 + s^2))
  df <- data.frame(metaUI__effect_size = y, metaUI__se = s, metaUI__study_id = seq_len(k))
  seconds <- vapply(1:5, function(i) system.time(metaUI_bayesian_fit(df, .5))[["elapsed"]], numeric(1))
  data.frame(k = k, median_seconds = stats::median(seconds), max_seconds = max(seconds))
}))
print(rows)
write.csv(rows, args[1], row.names = FALSE)
writeLines(capture.output(sessionInfo()), paste0(args[1], ".session.txt"))
