# Real datasets: Coles et al. (2019; 97 studies) and the Diversity app (646 studies), aggregated
# as the app does (GLS within-study average, correlation .6). Usage (from the package root): Rscript validation/bayesian-grid/real-data.R <with_bayesmeta: 0|1>
args <- commandArgs(TRUE); with_bm <- identical(args[1], "1")
here <- "validation/bayesian-grid"  # run from the package root
suppressPackageStartupMessages(library(metaUI))
source(file.path(here, "reference.R")); source("R/bayesian.R")
x <- read.csv(system.file("extdata", "coles2019.csv", package = "metaUI"))
x$study <- paste0(substr(x$title, 1, 60), " (", x$year, ")")
coles <- suppressMessages(prepare_data(x, "study", "yi", variance = "vi", es_id = "es_id", na.rm = "es_related"))
# The Diversity dataset is not in this repository; set METAUI_DIVERSITY_RDS to a generated app's dataset.rds.
div <- as.data.frame(readRDS(Sys.getenv("METAUI_DIVERSITY_RDS")))
sets <- list(Coles = metaUI:::metaUI_aggregate(coles, .6), Diversity = metaUI:::metaUI_aggregate(div, .6))
summ <- function(fit) c(mu_mean = fit$summary["mean", "mu"], mu_median = fit$qposterior(mu.p = .5),
  mu_q025 = fit$qposterior(mu.p = .025), mu_q05 = fit$qposterior(mu.p = .05), mu_q95 = fit$qposterior(mu.p = .95),
  mu_q975 = fit$qposterior(mu.p = .975), tau_mean = fit$summary["mean", "tau"], tau_median = fit$qposterior(tau.p = .5),
  tau_q025 = fit$qposterior(tau.p = .025), tau_q975 = fit$qposterior(tau.p = .975), mu_sd = fit$summary["sd", "mu"], tau_sd = fit$summary["sd", "tau"])
out <- list()
for (nm in names(sets)) {
  d <- sets[[nm]]; y <- d$metaUI__effect_size; s <- d$metaUI__se
  tau_scale <- if (d$metaUI__es_type[1] == "ZCOR") .25 else .5
  res <- list(k = length(y), es_type = d$metaUI__es_type[1], tau_scale = tau_scale)
  res$grid_seconds <- system.time(g <- metaUI_bayesian_grid(y, s, tau_scale))[["elapsed"]]
  res$grid <- summ(g); res$grid_n2 <- summ(metaUI_bayesian_grid(y, s, tau_scale, n = 4000L))
  res$ref_seconds <- system.time(res$ref <- reference_posterior(y, s, tau_scale))[["elapsed"]]
  if (with_bm && nm == "Coles") {
    res$bm_seconds <- system.time(b <- bayesmeta::bayesmeta(y = y, sigma = s, interval.type = "central",
      tau.prior = function(t) bayesmeta::dhalfnormal(t, scale = tau_scale)))[["elapsed"]]
    res$bayesmeta <- summ(b)
  }
  out[[nm]] <- res; print(res)
}
saveRDS(out, file.path(here, paste0("real-data", if (with_bm) "-bm", ".rds")))
