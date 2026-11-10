# Deterministic two-level normal/normal model; all arguments use the fitting scale.
metaUI_bayesian_options <- function(options, scale) {
  if (is.null(options)) return(NULL)
  if (!is.list(options)) stop("Bayesian options must be a list, e.g. list(enabled = TRUE, tau_scale = 0.5).")
  if (length(options) && (is.null(names(options)) || any(!nzchar(names(options))) || anyDuplicated(names(options))))
    stop("Bayesian options must be uniquely named.")
  unknown <- setdiff(names(options), c("enabled", "tau_scale", "max_studies"))
  if (length(unknown)) stop("Unknown Bayesian option(s): ", paste(unknown, collapse = ", "), ". Allowed: enabled, tau_scale, max_studies.")
  if (is.null(options$enabled)) options$enabled <- TRUE
  if (!is.logical(options$enabled) || length(options$enabled) != 1L || is.na(options$enabled)) stop("Bayesian enabled must be TRUE or FALSE.")
  if (!options$enabled) return(NULL)
  # Unit-information SD / 4: 0.5 for SMD and for log odds and risk ratios (Roever et al., 2021), 0.25 for Fisher z.
  if (is.null(options$tau_scale)) options$tau_scale <- if (scale == "ZCOR") .25 else .5
  if (length(options$tau_scale) != 1L || !is.numeric(options$tau_scale) || !is.finite(options$tau_scale) || options$tau_scale <= 0)
    stop("Bayesian tau_scale must be positive and finite.")
  # An optional author cap on the number of studies; by default there is none.
  if (!is.null(options$max_studies) && (length(options$max_studies) != 1L || !is.numeric(options$max_studies) ||
      !is.finite(options$max_studies) || options$max_studies < 2 || options$max_studies != floor(options$max_studies)))
    stop("Bayesian max_studies must be an integer of at least 2, or omitted for no limit.")
  options
}

metaUI_bayesian_fit <- function(df, tau_scale, max_studies = Inf) {
  if (nrow(df) < 2L) stop("Bayesian analysis requires at least two studies.")
  if (nrow(df) > max_studies) stop("Bayesian analysis exceeds the configured study limit (", max_studies, "). Reduce the selection.")
  fit <- metaUI_bayesian_grid(df$metaUI__effect_size, df$metaUI__se, tau_scale)
  fit$labels <- as.character(df$metaUI__study_id)
  fit
}

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

metaUI_bayesian_spec <- function(options) {
  tibble::tibble(name = "Bayesian normal-normal model", aggregated = TRUE,
    es = "mod$qposterior(mu.p = .5)", LCL = "mod$qposterior(mu.p = .025)", UCL = "mod$qposterior(mu.p = .975)",
    LCL90 = "mod$qposterior(mu.p = .05)", UCL90 = "mod$qposterior(mu.p = .95)", k = "length(mod$y)", code = paste0("metaUI_bayesian_fit(df, tau_scale = ",
      format(options$tau_scale, digits = 17, decimal.mark = "."),
      if (!is.null(options$max_studies)) paste0(", max_studies = ", options$max_studies), ")"))
}

metaUI_bayesian_sensitivity <- function(df, options, primary = NULL) {
  if (is.null(options)) return(data.frame())
  dplyr::bind_rows(lapply(c(.5, 1, 2), function(multiplier) {
    s <- options$tau_scale * multiplier
    tryCatch({
      fit <- if (multiplier == 1 && !is.null(primary)) primary else metaUI_bayesian_fit(df, s, if (is.null(options$max_studies)) Inf else options$max_studies)
      mu <- fit$qposterior(mu.p = c(.025, .5, .975))
      mu <- metaUI_show(mu, df)
      data.frame(tau_prior_scale = s, median = mu[2], LCL = mu[1], UCL = mu[3],
        tau_median = fit$qposterior(tau.p = .5), status = "ok", reason = "")
    }, error = function(e) data.frame(tau_prior_scale = s, median = NA_real_, LCL = NA_real_, UCL = NA_real_,
      tau_median = NA_real_, status = "unsupported", reason = conditionMessage(e)))
  }))
}
