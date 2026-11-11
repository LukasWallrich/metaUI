# Independent reference: adaptive Gauss-Kronrod (stats::integrate) over tau on many
# geometric segments, no shared code with the grid.
reference_posterior <- function(y, sigma, tau_scale) {
  v <- sigma^2
  lpost <- function(t) vapply(t, function(tt) {
    w <- 1 / (v + tt^2); m <- sum(w * y) / sum(w)
    .5 * sum(log(w)) - .5 * log(sum(w)) - .5 * sum(w * (y - m)^2) + dnorm(tt, 0, tau_scale, log = TRUE)
  }, numeric(1))
  cond <- function(t) vapply(t, function(tt) { w <- 1 / (v + tt^2); c(sum(w * y) / sum(w), 1 / sqrt(sum(w))) }, numeric(2))
  brk <- c(0, 10^seq(-7, 3, by = .1))
  c0 <- max(lpost(c(brk, (brk[-1] + brk[-length(brk)]) / 2)))
  dens <- function(t) exp(lpost(t) - c0)
  seg <- function(f, a, b) integrate(f, a, b, rel.tol = 1e-11, abs.tol = 0, subdivisions = 1000L, stop.on.error = FALSE)$value
  mass <- vapply(seq_len(length(brk) - 1), function(i) seg(dens, brk[i], brk[i + 1]), numeric(1))
  Z <- sum(mass)
  use <- which(mass > 1e-22 * Z)
  segs <- cbind(brk[use], brk[use + 1])
  integ <- function(f) sum(apply(segs, 1, function(ab) seg(f, ab[1], ab[2]))) / Z
  pmu <- function(q) integ(function(t) { cm <- cond(t); dens(t) * pnorm((q - cm[1, ]) / cm[2, ]) })
  ptau <- function(q) sum(apply(segs, 1, function(ab) if (ab[1] >= q) 0 else seg(dens, ab[1], min(ab[2], q)))) / Z
  ctr <- median(y)
  qmu <- function(a) uniroot(function(z) pmu(z) - a, ctr + c(-1, 1), extendInt = "upX", tol = 1e-12)$root
  qtau <- function(a) exp(uniroot(function(z) ptau(exp(z)) - a, log(range(segs[segs > 0])) + c(-1, 1), tol = 1e-12)$root)
  mean_mu <- integ(function(t) dens(t) * cond(t)[1, ])
  mean_tau <- integ(function(t) dens(t) * t)
  sd_mu <- sqrt(integ(function(t) { cm <- cond(t); dens(t) * (cm[2, ]^2 + cm[1, ]^2) }) - mean_mu^2)
  sd_tau <- sqrt(integ(function(t) dens(t) * t^2) - mean_tau^2)
  c(mu_mean = mean_mu, mu_median = qmu(.5), mu_q025 = qmu(.025), mu_q05 = qmu(.05), mu_q95 = qmu(.95), mu_q975 = qmu(.975),
    tau_mean = mean_tau, tau_median = qtau(.5), tau_q025 = qtau(.025), tau_q975 = qtau(.975), mu_sd = sd_mu, tau_sd = sd_tau)
}
