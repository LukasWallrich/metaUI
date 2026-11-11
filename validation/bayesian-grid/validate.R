# Usage (from the package root): Rscript validation/bayesian-grid/validate.R <part: ref|grid|bayesmeta> <ids or all> ; writes results/<part>-<id>.rds
args <- commandArgs(TRUE); part <- args[1]
here <- "validation/bayesian-grid"  # run from the package root
source(file.path(here, "conditions.R")); source(file.path(here, "reference.R")); source("R/bayesian.R")
conds <- make_conditions()
ids <- if (length(args) < 2 || args[2] == "all") conds$id else eval(parse(text = args[2]))
dir.create(file.path(here, "results"), showWarnings = FALSE)
summarise_fit <- function(fit) c(mu_mean = fit$summary["mean", "mu"], mu_median = fit$qposterior(mu.p = .5),
  mu_q025 = fit$qposterior(mu.p = .025), mu_q05 = fit$qposterior(mu.p = .05), mu_q95 = fit$qposterior(mu.p = .95),
  mu_q975 = fit$qposterior(mu.p = .975), tau_mean = fit$summary["mean", "tau"], tau_median = fit$qposterior(tau.p = .5),
  tau_q025 = fit$qposterior(tau.p = .025), tau_q975 = fit$qposterior(tau.p = .975), mu_sd = fit$summary["sd", "mu"], tau_sd = fit$summary["sd", "tau"])
run <- function(id) {
  out <- file.path(here, "results", paste0(part, "-", id, ".rds"))
  if (file.exists(out)) return(invisible())
  d <- make_data(conds[conds$id == id, ])
  t0 <- proc.time()[[3]]
  res <- tryCatch(switch(part,
    ref = list(ref = reference_posterior(d$y, d$sigma, d$tau_scale)),
    grid = {
      g <- metaUI_bayesian_grid(d$y, d$sigma, d$tau_scale); t1 <- proc.time()[[3]] - t0
      list(grid = summarise_fit(g), grid_seconds = t1, tail_mass = g$tail_mass, tau_range = range(g$grid$tau),
        grid_n2 = summarise_fit(metaUI_bayesian_grid(d$y, d$sigma, d$tau_scale, n = 4000L)),
        grid_cut2 = summarise_fit(metaUI_bayesian_grid(d$y, d$sigma, d$tau_scale, cut = 80)),
        grid_n_half = summarise_fit(metaUI_bayesian_grid(d$y, d$sigma, d$tau_scale, n = 1000L)))
    },
    bayesmeta = {
      b <- bayesmeta::bayesmeta(y = d$y, sigma = d$sigma, tau.prior = function(t) bayesmeta::dhalfnormal(t, scale = d$tau_scale),
        interval.type = "central")
      list(bayesmeta = summarise_fit(b))
    }), error = function(e) list(error = conditionMessage(e)))
  res$seconds <- proc.time()[[3]] - t0; res$id <- id
  saveRDS(res, out)
}
for (id in ids) { run(id); cat(part, id, "\n") }
