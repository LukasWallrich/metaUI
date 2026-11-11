# Edge cases from the external review (sol, 2026-11-11): cancellation, unresolved narrow posterior,
# translation invariance. Compares the fixed grid (grid-fn.R) with v1 (grid-fn-v1.R) and the reference.
here <- "validation/bayesian-grid"  # run from the package root
source(file.path(here, "reference.R")); source(file.path(here, "grid-fn-v1.R")); v1 <- metaUI_bayesian_grid; source("R/bayesian.R")
show <- function(f) signif(c(f$qposterior(mu.p = c(.025, .5, .975)), f$qposterior(tau.p = c(.025, .5, .975)), mu_sd = f$summary["sd", "mu"]), 5)
cat("1. y = 0,10,10; sigma = 1e-9,1,1; scale .5  [mu 2.5/50/97.5, tau 2.5/50/97.5, mu sd]\n")
y <- c(0, 10, 10); s <- c(1e-9, 1, 1)
print(rbind(v1 = show(v1(y, s, .5)), fixed = show(metaUI_bayesian_grid(y, s, .5)),
  reference = signif(reference_posterior(y, s, .5)[c("mu_q025", "mu_median", "mu_q975", "tau_q025", "tau_median", "tau_q975", "mu_sd")], 5)))
cat("\n2. y = -1,1; sigma = 1e-14 x2; scale 1e-10 (prior and data in sharp conflict); grid density n = 2000 vs 20000\n")
y <- c(-1, 1); s <- rep(1e-14, 2)
print(rbind(v1 = show(v1(y, s, 1e-10)), v1_n20000 = show(v1(y, s, 1e-10, n = 20000L)),
  fixed = show(metaUI_bayesian_grid(y, s, 1e-10)), fixed_n20000 = show(metaUI_bayesian_grid(y, s, 1e-10, n = 20000L))))
cat("\n3. Translation: equal effects .2 vs 1e8 (+ .2 shift removed), SE .1, scale .5\n")
a <- metaUI_bayesian_grid(c(.2, .2), c(.1, .1), .5); b <- metaUI_bayesian_grid(c(1e8, 1e8), c(.1, .1), .5)
a1 <- v1(c(.2, .2), c(.1, .1), .5); b1 <- v1(c(1e8, 1e8), c(.1, .1), .5)
print(rbind(v1 = c(a1$summary["sd", "mu"], b1$summary["sd", "mu"]), fixed = c(a$summary["sd", "mu"], b$summary["sd", "mu"])))
cat("\n2b. Case 2 tau quantiles in posterior SDs of tau, relative to the fixed grid at n = 20000\n")
y <- c(-1, 1); s <- rep(1e-14, 2); r <- metaUI_bayesian_grid(y, s, 1e-10, n = 20000L)
sdt <- r$summary["sd", "tau"]; q <- function(f) f$qposterior(tau.p = c(.025, .5, .975))
print(rbind(v1 = (q(v1(y, s, 1e-10)) - q(r)) / sdt, fixed = (q(metaUI_bayesian_grid(y, s, 1e-10)) - q(r)) / sdt))
cat("tau posterior SD:", sdt, "; nodes with weight > 1e-6: v1", sum(v1(y, s, 1e-10)$grid$weight > 1e-6),
  " fixed", sum(metaUI_bayesian_grid(y, s, 1e-10)$grid$weight > 1e-6), "\n")
