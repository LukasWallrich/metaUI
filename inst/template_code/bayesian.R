# Deterministic two-level normal/normal model; all arguments use the fitting scale.
metaUI_bayesian_options <- function(options, scale) {
  if (is.null(options)) return(NULL)
  if (!is.list(options)) stop("Bayesian options must be a list, e.g. list(enabled = TRUE, tau_scale = 0.5, max_studies = 20).")
  if (length(options) && (is.null(names(options)) || any(!nzchar(names(options))) || anyDuplicated(names(options))))
    stop("Bayesian options must be uniquely named.")
  unknown <- setdiff(names(options), c("enabled", "tau_scale", "max_studies"))
  if (length(unknown)) stop("Unknown Bayesian option(s): ", paste(unknown, collapse = ", "), ". Allowed: enabled, tau_scale, max_studies.")
  if (is.null(options$enabled)) options$enabled <- TRUE
  if (!is.logical(options$enabled) || length(options$enabled) != 1L || is.na(options$enabled)) stop("Bayesian enabled must be TRUE or FALSE.")
  if (!options$enabled) return(NULL)
  if (is.null(options$tau_scale)) options$tau_scale <- if (scale == "SMD") .5 else .25
  if (length(options$tau_scale) != 1L || !is.numeric(options$tau_scale) || !is.finite(options$tau_scale) || options$tau_scale <= 0)
    stop("Bayesian tau_scale must be positive and finite.")
  if (is.null(options$max_studies)) options$max_studies <- 20L
  if (length(options$max_studies) != 1L || !is.numeric(options$max_studies) || !is.finite(options$max_studies) ||
      options$max_studies < 2 || options$max_studies > 50 || options$max_studies != floor(options$max_studies))
    stop("Bayesian max_studies must be an integer from 2 to 50.")
  if (options$max_studies > 20) warning("Bayesian fits block the Shiny process. Benchmarks took approximately 20 seconds at 20 studies and 97 seconds at 50; prior sensitivity/downloads can need two additional fits.")
  options
}

metaUI_bayesian_fit <- function(df, tau_scale, max_studies = 50L) {
  if (!requireNamespace("bayesmeta", quietly = TRUE)) stop("Bayesian analysis requires the optional bayesmeta package.")
  if (nrow(df) < 2L) stop("Bayesian analysis requires at least two studies.")
  if (nrow(df) > max_studies) stop("Bayesian analysis exceeds the configured study limit (", max_studies, "). Reduce the selection.")
  bayesmeta::bayesmeta(y = df$metaUI__effect_size, sigma = df$metaUI__se,
    labels = as.character(df$metaUI__study_id),
    tau.prior = function(t) bayesmeta::dhalfnormal(t, scale = tau_scale), interval.type = "central")
}

metaUI_bayesian_spec <- function(options) {
  tibble::tibble(name = "Bayesian normal-normal model", aggregated = TRUE,
    es = "mod$qposterior(mu.p = .5)", LCL = "mod$qposterior(mu.p = .025)", UCL = "mod$qposterior(mu.p = .975)",
    k = "length(mod$y)", code = paste0("metaUI_bayesian_fit(df, tau_scale = ",
      format(options$tau_scale, digits = 17, decimal.mark = "."), ", max_studies = ", options$max_studies, ")"))
}

metaUI_bayesian_sensitivity <- function(df, options, primary = NULL) {
  if (is.null(options)) return(data.frame())
  dplyr::bind_rows(lapply(c(.5, 1, 2), function(multiplier) {
    s <- options$tau_scale * multiplier
    tryCatch({
      fit <- if (multiplier == 1 && !is.null(primary)) primary else metaUI_bayesian_fit(df, s, options$max_studies)
      mu <- fit$qposterior(mu.p = c(.025, .5, .975))
      if (df$metaUI__es_type[1] == "ZCOR") mu <- tanh(mu)
      data.frame(tau_prior_scale = s, median = mu[2], LCL = mu[1], UCL = mu[3],
        tau_median = fit$qposterior(tau.p = .5), status = "ok", reason = "")
    }, error = function(e) data.frame(tau_prior_scale = s, median = NA_real_, LCL = NA_real_, UCL = NA_real_,
      tau_median = NA_real_, status = "unsupported", reason = conditionMessage(e)))
  }))
}
