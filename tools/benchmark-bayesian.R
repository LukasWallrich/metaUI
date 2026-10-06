# Bayesian first-fit latency; see validation/bayesian-benchmark.md. Synthetic data only.
# Usage: Rscript --vanilla tools/benchmark-bayesian.R output.csv
args <- commandArgs(TRUE)
if (length(args) != 1L) stop("Usage: benchmark-bayesian.R output.csv")
fit_once <- function(k, limit = Inf) {
  s <- runif(k, .1, .3); y <- rnorm(k, .2, sqrt(.2^2 + s^2))
  started <- proc.time()[["elapsed"]]
  setTimeLimit(elapsed = limit, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf))
  status <- tryCatch({
    bayesmeta::bayesmeta(y = y, sigma = s, interval.type = "central",
      tau.prior = function(t) bayesmeta::dhalfnormal(t, scale = .5))
    "ok"
  }, error = function(e) conditionMessage(e))
  data.frame(k = k, seconds = proc.time()[["elapsed"]] - started, status = status)
}
rows <- list()
record <- function(row) {
  rows[[length(rows) + 1L]] <<- row
  write.csv(do.call(rbind, rows), args[1], row.names = FALSE)
}
set.seed(90); for (k in c(10, 50)) record(fit_once(k))
set.seed(91); record(fit_once(20))
set.seed(92); record(fit_once(200, limit = 120))
writeLines(capture.output(sessionInfo()), paste0(args[1], ".session.txt"))
