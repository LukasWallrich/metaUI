# Builds the tables for report.md from results/*.rds. Usage (from the package root): Rscript validation/bayesian-grid/analyse.R > validation/bayesian-grid/tables.md
here <- "validation/bayesian-grid"  # run from the package root
source(file.path(here, "conditions.R"))
conds <- make_conditions()
load_part <- function(part) {
  f <- list.files(file.path(here, "results"), paste0("^", part, "-"), full.names = TRUE)
  setNames(lapply(f, readRDS), vapply(f, function(x) sub(".*-(\\d+)\\.rds$", "\\1", x), ""))
}
ref <- load_part("ref"); grid <- load_part("grid"); bm <- load_part("bayesmeta")
errs <- lapply(list(reference = ref, grid = grid, bayesmeta = bm), function(p) Filter(function(x) !is.null(x$error), p))
q_mu <- c("mu_mean", "mu_median", "mu_q025", "mu_q05", "mu_q95", "mu_q975")
q_tau <- c("tau_mean", "tau_median", "tau_q025", "tau_q975")
long <- function(est_list, field, label) do.call(rbind, lapply(names(est_list), function(id) {
  r <- ref[[id]]$ref; e <- est_list[[id]][[field]]
  if (is.null(r) || is.null(e)) return(NULL)
  cond <- conds[conds$id == as.integer(id), ]
  q <- c(q_mu, q_tau)
  z <- cond$scale == "Fisher z"
  rep_ref <- ifelse(z & grepl("^mu", q), tanh(r[q]), r[q]); rep_est <- ifelse(z & grepl("^mu", q), tanh(e[q]), e[q])
  data.frame(id = as.integer(id), k = cond$k, scale = cond$scale, tau = cond$tau, se = cond$se, prior = cond$prior,
    outlier = cond$outlier, method = label, quantity = q, ref = r[q], est = e[q],
    abs = abs(e[q] - r[q]), rel = abs(e[q] - r[q]) / abs(r[q]),
    by_sd = abs(e[q] - r[q]) / ifelse(grepl("^mu", q), r["mu_sd"], r["tau_sd"]),
    rep_abs = abs(rep_est - rep_ref),
    # Agreement to 3 significant digits on the reported scale: within half a unit of the third digit.
    sig3 = abs(rep_est - rep_ref) <= .5 * 10^(floor(log10(abs(rep_ref))) - 2), row.names = NULL)
}))
d <- rbind(long(grid, "grid", "grid"), long(bm, "bayesmeta", "bayesmeta"))
fmt <- function(x) formatC(x, format = "e", digits = 1)
md <- function(df) { cat("|", paste(names(df), collapse = " | "), "|\n|", paste(rep("---", ncol(df)), collapse = " | "), "|\n")
  for (i in seq_len(nrow(df))) cat("|", paste(df[i, ], collapse = " | "), "|\n"); cat("\n") }
summ <- function(x, by) {
  s <- split(x, by, drop = TRUE)
  do.call(rbind, lapply(names(s), function(n) { y <- s[[n]]
    data.frame(group = n, datasets = length(unique(y$id)), max_abs = fmt(max(y$abs)), max_rel = fmt(max(y$rel[is.finite(y$rel)])),
      max_by_sd = fmt(max(y$by_sd)), max_abs_reported = fmt(max(y$rep_abs)), sig3_agree = sprintf("%.1f%%", 100 * mean(y$sig3))) }))
}
cat("Conditions run: ", nrow(conds), "; reference ", length(ref), ", grid ", length(grid), ", bayesmeta ", length(bm), ". Failures: ", paste(names(errs), lengths(errs), collapse = ", "), "\n\n", sep = "")
for (p in names(errs)) for (e in errs[[p]]) cat("- ", p, " failed on dataset ", e$id, ": ", e$error, "\n", sep = ""); cat("\n")
cat("### A. Accuracy against the adaptive-integration reference, all datasets\n\n")
md(summ(d, interaction(d$method, ifelse(grepl("^mu", d$quantity), "mu", "tau"), sep = " / ")))
cat("### B. By quantity (grid vs reference, all", length(unique(d$id[d$method == "grid"])), "datasets)\n\n")
g <- d[d$method == "grid", ]; md(summ(g, g$quantity))
cat("### C. By quantity (bayesmeta vs reference,", length(unique(d$id[d$method == "bayesmeta"])), "datasets)\n\n")
b <- d[d$method == "bayesmeta", ]; md(summ(b, b$quantity))
cat("### D. Grid vs reference by number of studies\n\n"); md(summ(g, sprintf("k = %03d", g$k)))
cat("### E. Grid vs reference by condition (k <= 50)\n\n")
gs <- g[g$k <= 50, ]
md(rbind(summ(gs, paste("true tau:", gs$tau)), summ(gs, paste("SEs:", gs$se)), summ(gs, paste("scale:", gs$scale)),
  summ(gs, sprintf("prior x %s", format(gs$prior))), summ(gs, paste("outlier:", gs$outlier))))
cat("### F. Grid vs bayesmeta directly (datasets with both)\n\n")
both <- intersect(names(Filter(function(x) is.null(x$error), bm)), names(grid))
gb <- do.call(rbind, lapply(both, function(id) { a <- grid[[id]]$grid; bb <- bm[[id]]$bayesmeta; r <- ref[[id]]$ref
  data.frame(q = c(q_mu, q_tau), diff_by_sd = abs(a[c(q_mu, q_tau)] - bb[c(q_mu, q_tau)]) / ifelse(grepl("^mu", c(q_mu, q_tau)), r["mu_sd"], r["tau_sd"]),
    abs = abs(a[c(q_mu, q_tau)] - bb[c(q_mu, q_tau)])) }))
md(do.call(rbind, lapply(split(gb, gb$q), function(y) data.frame(quantity = y$q[1], max_abs = fmt(max(y$abs)), max_by_sd = fmt(max(y$diff_by_sd))))))
cat("### G. Grid robustness: change from the default grid (n = 2000, cut = 40), max over all datasets, in posterior SDs\n\n")
rob <- do.call(rbind, lapply(names(grid), function(id) { x <- grid[[id]]; r <- ref[[id]]$ref; if (is.null(r)) return(NULL)
  sdv <- ifelse(grepl("^mu", c(q_mu, q_tau)), r["mu_sd"], r["tau_sd"]); qq <- c(q_mu, q_tau)
  data.frame(variant = c("n = 4000 (double density)", "cut = 80 (wider range)", "n = 1000 (half density)"),
    mu = c(max(abs(x$grid_n2[q_mu] - x$grid[q_mu]) / r["mu_sd"]), max(abs(x$grid_cut2[q_mu] - x$grid[q_mu]) / r["mu_sd"]), max(abs(x$grid_n_half[q_mu] - x$grid[q_mu]) / r["mu_sd"])),
    tau = c(max(abs(x$grid_n2[q_tau] - x$grid[q_tau]) / r["tau_sd"]), max(abs(x$grid_cut2[q_tau] - x$grid[q_tau]) / r["tau_sd"]), max(abs(x$grid_n_half[q_tau] - x$grid[q_tau]) / r["tau_sd"]))) }))
md(do.call(rbind, lapply(split(rob, rob$variant), function(y) data.frame(variant = y$variant[1], mu_max = fmt(max(y$mu)), tau_max = fmt(max(y$tau))))))
cat("### H. Grid range and dropped tail mass\n\n")
tm <- vapply(grid, `[[`, 0, "tail_mass"); tr <- t(vapply(grid, `[[`, c(0, 0), "tau_range"))
cat("Largest share of first-pass posterior mass outside the final grid:", fmt(max(tm)), "\n\n")
lo0 <- mean(tr[, 1] == 0); cat("Final grid starts at tau = 0 in ", sprintf("%.0f%%", 100 * lo0), " of datasets; elsewhere the posterior below the grid's start is < e^-40 of its peak.\n\n", sep = "")
cat("### I. Timing (seconds, this machine while other validation jobs ran)\n\n")
tim <- data.frame(id = as.integer(names(grid)), grid = vapply(grid, `[[`, 0, "grid_seconds"))
tim$k <- conds$k[match(tim$id, conds$id)]
bt <- data.frame(id = as.integer(names(bm)), bayesmeta = vapply(bm, `[[`, 0, "seconds")); tim <- merge(tim, bt, all.x = TRUE)
md(do.call(rbind, lapply(split(tim, tim$k), function(y) data.frame(k = y$k[1], grid_median = sprintf("%.3f", median(y$grid)), grid_max = sprintf("%.3f", max(y$grid)),
  bayesmeta_median = if (all(is.na(y$bayesmeta))) "not run" else sprintf("%.1f", median(y$bayesmeta, na.rm = TRUE)),
  bayesmeta_max = if (all(is.na(y$bayesmeta))) "" else sprintf("%.1f", max(y$bayesmeta, na.rm = TRUE))))))
cat("### J. Worst grid cases (largest error in posterior SDs)\n\n")
w <- g[order(-g$by_sd), ][1:8, c("id", "k", "scale", "tau", "se", "prior", "outlier", "quantity", "ref", "est", "by_sd")]
w$ref <- signif(w$ref, 6); w$est <- signif(w$est, 6); w$by_sd <- fmt(w$by_sd); md(w)
cat("### K. Worst bayesmeta cases\n\n")
w <- b[order(-b$by_sd), ][1:5, c("id", "k", "scale", "tau", "se", "prior", "outlier", "quantity", "ref", "est", "by_sd")]
w$ref <- signif(w$ref, 6); w$est <- signif(w$est, 6); w$by_sd <- fmt(w$by_sd); md(w)
saveRDS(d, file.path(here, "accuracy-long.rds"))
