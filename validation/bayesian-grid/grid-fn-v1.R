# Posterior of the normal-normal random-effects model with a flat prior on mu and a
# half-normal prior on tau, by quadrature over tau. Given tau, mu is normal, so the
# posterior of mu is a normal mixture over the tau grid (as in bayesmeta's DIRECT method,
# but on a fixed fine grid). The grid is uniform in x = log(tau + tau0), which resolves
# both a peak near tau = 0 and a long right tail; a first pass locates the posterior
# mass and a second pass places n points over it.
metaUI_bayesian_grid <- function(y, sigma, tau_scale, n = 2000L, cut = 40) {
  k <- length(y); v <- sigma^2
  centre <- stats::median(y); yc <- y - centre
  profile <- function(tau) {
    sw <- swy <- swy2 <- sl <- numeric(length(tau))
    for (i in seq_len(k)) {
      w <- 1 / (tau^2 + v[i])
      sw <- sw + w; swy <- swy + w * yc[i]; swy2 <- swy2 + w * yc[i]^2; sl <- sl + log(w)
    }
    m <- swy / sw
    list(mean = m, sd = sqrt(1 / sw),
      lp = .5 * sl - .5 * log(sw) - .5 * pmax(swy2 - swy * m, 0) + stats::dnorm(tau, 0, tau_scale, log = TRUE))
  }
  tau0 <- min(sigma, tau_scale) / 10
  upper <- 10 * max(tau_scale, diff(range(y)), min(sigma))
  for (attempt in 1:20) {
    x <- seq(log(tau0), log(upper + tau0), length.out = 1000L)
    lx <- profile(exp(x) - tau0)$lp + x
    if (lx[length(lx)] < max(lx) - cut) break
    upper <- upper * 4
  }
  keep <- which(lx > max(lx) - cut)
  lo <- max(1L, min(keep) - 1L); hi <- min(length(x), max(keep) + 1L)
  dropped <- sum(exp(lx[-(lo:hi)] - max(lx))) / sum(exp(lx - max(lx)))
  x <- seq(x[lo], x[hi], length.out = n)
  tau <- exp(x) - tau0
  if (lo == 1L) tau[1] <- 0
  p <- profile(tau)
  dens <- exp(p$lp + x - max(p$lp + x))
  wt <- dens; wt[c(1, n)] <- wt[c(1, n)] / 2; wt <- wt / sum(wt)
  cdf_tau <- c(0, cumsum((dens[-1] + dens[-n]) / 2)); cdf_tau <- cdf_tau / cdf_tau[n]
  mu <- p$mean + centre; sd <- p$sd
  pmu <- function(q) vapply(q, function(z) sum(wt * stats::pnorm((z - mu) / sd)), numeric(1))
  bracket <- c(min(mu - 12 * sd), max(mu + 12 * sd))
  qmu <- function(prob) vapply(prob, function(a) stats::uniroot(function(z) pmu(z) - a, bracket,
    tol = 1e-12 * diff(bracket))$root, numeric(1))
  qtau <- function(prob) pmax(0, exp(stats::approx(cdf_tau, x, xout = prob, ties = "ordered")$y) - tau0)
  mean_mu <- sum(wt * mu); mean_tau <- sum(wt * tau)
  summary <- rbind(median = c(qtau(.5), qmu(.5)), mean = c(mean_tau, mean_mu),
    sd = c(sqrt(sum(wt * tau^2) - mean_tau^2), sqrt(sum(wt * (sd^2 + mu^2)) - mean_mu^2)),
    `95% lower` = c(qtau(.025), qmu(.025)), `95% upper` = c(qtau(.975), qmu(.975)))
  colnames(summary) <- c("tau", "mu")
  list(y = y, sigma = sigma, k = k, tau_scale = tau_scale, summary = summary,
    qposterior = function(tau.p = NA, mu.p = NA) if (!all(is.na(mu.p))) qmu(mu.p) else qtau(tau.p),
    pposterior = function(tau = NA, mu = NA) if (!all(is.na(mu))) pmu(mu) else
      stats::approx(x, cdf_tau, xout = log(tau + tau0), rule = 2)$y,
    grid = data.frame(tau = tau, weight = wt, mean = mu, sd = sd), tail_mass = dropped)
}
