# Simulated validation datasets. Each condition is reproducible from its seed.
make_conditions <- function() {
  base <- expand.grid(k = c(2, 3, 5, 10, 20), tau = c("zero", "small", "large"), se = c("moderate", "extreme"),
    scale = c("SMD", "Fisher z"), prior = c(0.05, 0.5, 1, 2, 10), outlier = FALSE, stringsAsFactors = FALSE)
  outl <- expand.grid(k = c(5, 10, 20), tau = c("zero", "small"), se = "moderate", scale = c("SMD", "Fisher z"),
    prior = c(0.05, 1, 10), outlier = TRUE, stringsAsFactors = FALSE)
  k50 <- data.frame(k = 50, tau = c("zero", "large", "small", "small"), se = c("moderate", "moderate", "extreme", "moderate"),
    scale = c("SMD", "SMD", "SMD", "Fisher z"), prior = 1, outlier = FALSE)
  big <- expand.grid(k = c(100, 646), tau = c("zero", "small", "large"), se = c("moderate", "extreme"),
    scale = c("SMD", "Fisher z"), prior = c(0.05, 1, 10), outlier = FALSE, stringsAsFactors = FALSE)
  out <- rbind(base, outl, k50, big)
  out$id <- seq_len(nrow(out)); out$seed <- 1000 + out$id
  out
}
make_data <- function(cond) {
  set.seed(cond$seed); k <- cond$k
  smd <- cond$scale == "SMD"
  sigma <- if (cond$se == "extreme") exp(seq(log(.01), log(1), length.out = k))[sample(k)] else
    if (smd) runif(k, .1, .4) else 1 / sqrt(round(runif(k, 20, 300)) - 3)
  tau <- c(zero = 0, small = .5, large = 3)[[cond$tau]] * stats::median(sigma)
  mu <- if (smd) .3 else atanh(.2)
  y <- mu + rnorm(k, 0, tau) + rnorm(k, 0, sigma)
  if (cond$outlier) y[k] <- mu + 6 * sqrt(sigma[k]^2 + tau^2)
  prior <- cond$prior * (if (smd) .5 else .25)
  list(y = y, sigma = sigma, tau_scale = prior, true_tau = tau)
}
