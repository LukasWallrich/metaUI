# This file is copied verbatim into generated apps. Keep it independent of metaUI.

metaUI_code_multilevel <- "metaUI_multilevel_fit(df)"
metaUI_code_rve <- "metaUI_rve_fit(df)"

metaUI_multilevel_fit <- function(df) {
  metafor::rma.mv(df$metaUI__effect_size, V = df$metaUI__variance,
    random = ~ 1 | metaUI__study_id/metaUI__effect_id, data = df,
    test = "t", method = "REML", sparse = TRUE)
}

metaUI_rve_fit <- function(df) {
  robumeta::robu(metaUI__effect_size ~ 1, data = df,
    studynum = df$metaUI__study_id, var.eff.size = df$metaUI__variance, small = FALSE)
}

# One cache per server session. Exact keys include all data attributes and model
# code/configuration; no hash collisions, cross-user state, or unbounded history.
metaUI_fit_cache <- function(max_entries = 3L, limit = 3L) {
  if (length(max_entries) != 1L || !is.numeric(max_entries) || !is.finite(max_entries) ||
      max_entries < 0 || max_entries > limit || max_entries != as.integer(max_entries))
    stop("fit_cache_entries must be an integer from 0 to ", limit, ".")
  entries <- list()
  cached <- function(df, models, correlation = .6, aggregation = "aggregate") {
    started <- proc.time()[["elapsed"]]
    key <- list(data = df, models = models, correlation = correlation, aggregation = aggregation)
    match <- which(vapply(entries, function(x) identical(x$key, key), logical(1)))
    hit <- length(match) > 0L
    if (hit) {
      entry <- entries[[match[1]]]
      entries <<- c(list(entry), entries[-match[1]])
      result <- entry$value
    } else {
      result <- metaUI_fit_models(df, models, correlation, aggregation)
      if (max_entries > 0L) entries <<- utils::head(c(list(list(key = key, value = result)), entries), max_entries)
    }
    result$table$cache_hit <- hit
    result$cache_hit <- hit
    result$calculation_seconds <- proc.time()[["elapsed"]] - started
    result
  }
  attr(cached, "clear") <- function() entries <<- list()
  cached
}

metaUI_reuse_fit <- function(results, models, code, df, fallback) {
  if (!identical(results$fit_data, df)) return(fallback())
  idx <- which(models$code == code & !models$aggregated)
  for (i in idx) if (!is.null(results$fits[[i]]) && results$table$status[i] == "ok" && !results$table$reflected[i]) return(results$fits[[i]])
  fallback()
}

metaUI_heterogeneity <- function(mod, df) {
  # Needs replication both between and within studies.
  identified <- length(unique(df$metaUI__study_id)) > 1L && anyDuplicated(df$metaUI__study_id) > 0L
  data.frame(study_variance = if (identified) mod$sigma2[1] else NA_real_,
    effect_variance = if (identified) mod$sigma2[2] else NA_real_,
    total_variance = sum(mod$sigma2), Q = mod$QE, Q_p = mod$QEp,
    components = if (identified) "Study and within-study effects" else if (anyDuplicated(df$metaUI__study_id)) "Split not identified: only one study" else "Split not identified: one effect per study")
}

metaUI_aggregate <- function(df, correlation = .6, method = "aggregate") {
  if (!is.numeric(correlation) || length(correlation) != 1L || !is.finite(correlation) ||
      correlation < 0 || correlation >= 1) stop("Aggregation correlation must be in [0, 1).")
  if (!method %in% c("aggregate", "first")) stop("Unknown aggregation method.")
  groups <- split(seq_len(nrow(df)), as.character(df$metaUI__study_id))
  dplyr::bind_rows(lapply(groups, function(idx) {
    x <- df[idx, , drop = FALSE]
    out <- x[1, , drop = FALSE]
    if (method == "aggregate") {
      vi <- x$metaUI__variance
      covariance <- tcrossprod(sqrt(vi)) * (correlation + diag(1 - correlation, length(vi)))
      inverse <- chol2inv(chol(covariance))
      out$metaUI__variance <- 1 / sum(inverse)
      out$metaUI__effect_size <- sum(inverse %*% x$metaUI__effect_size) / sum(inverse)
      out$metaUI__se <- sqrt(out$metaUI__variance)
      # Neither a p-value nor a total sample size can be inferred from effect aggregation.
      out$metaUI__pvalue <- NA_real_
      out$metaUI__N <- NA_real_
    }
    out
  }))
}

# Identify the default estimators by their code, so renaming a model keeps its
# direction handling and input checks. Unrecognised code returns `fallback`.
metaUI_model_role <- function(spec, fallback = spec$name) {
  code <- gsub("\\s+", "", spec$code)
  roles <- c("Random-Effects Multilevel Model" = metaUI_code_multilevel,
             "Robust Variance Estimation" = metaUI_code_rve,
             "Trim-and-fill" = "meta::trimfill(meta::metagen(",
             "P-uniform star" = "puniform::puni_star(",
             "Hedges-Vevea Selection Model" = "weightr::weightfunct(",
             "P-Curve (first value)" = 'metaUI_pcurve_fit(df,"first")',
             "P-Curve (last value)" = 'metaUI_pcurve_fit(df,"last")',
             "Precision Effect Test" = "lm(metaUI__effect_size~sqrt(metaUI__variance),",
             "Precision Effect Estimate using Standard Error" = "lm(metaUI__effect_size~metaUI__variance,")
  hit <- names(roles)[vapply(roles, grepl, logical(1), x = code, fixed = TRUE)]
  if (length(hit)) hit[1] else fallback
}

metaUI_model_reason <- function(spec, df) {
  if (!nrow(df)) return("No eligible rows")
  role <- metaUI_model_role(spec)
  if (any(!is.finite(df$metaUI__effect_size)) || any(!is.finite(df$metaUI__variance) | df$metaUI__variance <= 0))
    return("Invalid required effect/variance inputs")
  if (role %in% c("P-uniform star", "Hedges-Vevea Selection Model") &&
      !df$metaUI__direction[1] %in% c("positive", "negative"))
    return("Requires explicit positive or negative effect direction")
  if (role %in% c("P-Curve (first value)", "P-Curve (last value)")) {
    if (df$metaUI__es_type[1] != "SMD") return("P-curve effect estimation only supported for SMD")
    selection <- if (role == "P-Curve (last value)") "last" else "first"
    issue <- tryCatch({metaUI_pcurve_data(df, selection, require_N = TRUE); NULL}, error = function(e) conditionMessage(e))
    if (!is.null(issue)) return(issue)
  }
  if (role %in% c("Precision Effect Test", "Precision Effect Estimate using Standard Error")) {
    predictor <- if (role == "Precision Effect Test") sqrt(df$metaUI__variance) else df$metaUI__variance
    if (length(unique(predictor)) < 2 || nrow(df) <= 2) return("Regression requires variable precision and positive residual degrees of freedom")
  }
  NULL
}

metaUI_fit_models <- function(df, models, correlation = .6, aggregation = "aggregate") {
  metaUI_validate_prepared(df)
  if (!nrow(df)) stop("No eligible rows remain after filtering.")
  df_agg <- metaUI_aggregate(df, correlation, aggregation)
  available_rows <- attr(df, "metaUI_runtime_rows")
  validation <- attr(df, "metaUI_validation")
  if (is.null(available_rows)) available_rows <- if (is.null(validation)) nrow(df) else validation$retained_rows
  preparation_excluded <- if (is.null(validation)) NA_integer_ else nrow(validation$exclusions)
  fits <- vector("list", nrow(models))
  rows <- lapply(seq_len(nrow(models)), function(i) {
    spec <- models[i, , drop = FALSE]
    x <- if (isTRUE(spec$aggregated)) df_agg else df
    reason <- metaUI_model_reason(spec, x)
    bayesian <- grepl("^metaUI_bayesian_fit\\(", spec$code)
    if (bayesian && nrow(x) < 2L) reason <- "Bayesian analysis requires at least two studies"
    warnings <- character()
    elapsed <- 0
    result <- c(es = NA_real_, LCL = NA_real_, UCL = NA_real_, k = NA_real_)
    status <- "unsupported"
    reflected <- FALSE
    if (is.null(reason)) {
      started <- proc.time()[["elapsed"]]
      reason <- tryCatch({
        env <- new.env(parent = environment(metaUI_fit_models))
        env$df <- x
        # Reflect the inputs only for explicitly directional, right-sided models.
        reflected <- metaUI_model_role(spec) %in% c("P-uniform star", "Hedges-Vevea Selection Model") && x$metaUI__direction[1] == "negative"
        if (reflected) env$df$metaUI__effect_size <- -env$df$metaUI__effect_size
        env$mod <- withCallingHandlers(eval(parse(text = spec$code), env), warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
        })
        candidate <- vapply(c("es", "LCL", "UCL", "k"), function(field) {
          value <- if (is.na(spec[[field]])) NA_real_ else as.numeric(eval(parse(text = spec[[field]]), env))
          if (length(value) != 1L) stop("Model extraction must return one value per field")
          value
        }, numeric(1))
        if (any(!is.finite(candidate[c("es", "k")])) || any(is.infinite(candidate[c("LCL", "UCL")])) ) stop("Nonfinite estimate, interval, or count")
        for (field in c("LCL", "UCL")) if (is.na(candidate[field]) && !is.na(spec[[field]]) && spec[[field]] != "NA") stop("Missing model interval")
        result <- candidate
        if (reflected) result[c("es", "LCL", "UCL")] <- c(-result["es"], -result["UCL"], -result["LCL"])
        status <- "ok"
        if (bayesian || (!spec$aggregated && !reflected && spec$code %in% c(metaUI_code_multilevel, metaUI_code_rve))) fits[[i]] <<- env$mod
        ""
      }, error = function(e) { status <<- "failed"; conditionMessage(e) })
      elapsed <- proc.time()[["elapsed"]] - started
    }
    row <- data.frame(Model = spec$name, es = result["es"], LCL = result["LCL"], UCL = result["UCL"],
                      k = result["k"], status = status, reason = reason,
                      warnings = paste(unique(warnings), collapse = "; "),
                      filtered_rows = max(0L, available_rows - nrow(df)), preparation_excluded_rows = preparation_excluded,
                      input_rows = nrow(df), analysis_rows = nrow(x),
                      collapsed_rows = nrow(df) - nrow(x), aggregated = spec$aggregated,
                      fit_seconds = elapsed, reflected = reflected, row.names = NULL)
    row$interval_type <- if (bayesian) "95% central credible interval (prior-dependent)" else
      if (is.na(metaUI_model_role(spec, NA_character_))) "Author-defined interval (type and coverage unknown)" else
      "Confidence interval (coverage model-dependent)"
    row$fit_es <- row$es; row$fit_LCL <- row$LCL; row$fit_UCL <- row$UCL
    if (df$metaUI__es_type[1] == "ZCOR") row[c("es", "LCL", "UCL")] <- lapply(row[c("es", "LCL", "UCL")], tanh)
    row
  })
  list(df_agg = df_agg, table = dplyr::bind_rows(rows), fits = fits, fit_data = df)
}

metaUI_validate_prepared <- function(df) {
  required <- c("metaUI__study_id", "metaUI__effect_id", "metaUI__effect_size", "metaUI__variance",
                "metaUI__se", "metaUI__es_z", "metaUI__es_type", "metaUI__display_scale", "metaUI__direction")
  if (!all(required %in% names(df))) stop("Prepared data columns are missing; run prepare_data() first.")
  if (!nrow(df)) return(invisible(TRUE))
  for (field in c("metaUI__effect_size", "metaUI__variance", "metaUI__se"))
    if (!is.numeric(df[[field]]) || any(!is.finite(df[[field]]))) stop("Invalid prepared numeric inputs: ", field)
  if (any(df$metaUI__variance <= 0 | df$metaUI__se <= 0) ||
      any(abs(df$metaUI__se^2 - df$metaUI__variance) > 1e-6 * pmax(abs(df$metaUI__variance), df$metaUI__se^2))) stop("Prepared SE/variance contract failed.")
  if (anyNA(df$metaUI__study_id) || anyNA(df$metaUI__effect_id) ||
      any(trimws(as.character(df$metaUI__study_id)) == "") || any(trimws(as.character(df$metaUI__effect_id)) == "") ||
      anyDuplicated(df[c("metaUI__study_id", "metaUI__effect_id")])) stop("Prepared IDs must be nonmissing and unique within study.")
  for (field in c("metaUI__es_type", "metaUI__display_scale", "metaUI__direction"))
    if (anyNA(df[[field]]) || length(unique(df[[field]])) != 1L) stop("Mixed or missing prepared scale/direction metadata.")
  metric <- df$metaUI__es_type[1]
  if (!metric %in% c("SMD", "ZCOR") || df$metaUI__display_scale[1] != if (metric == "ZCOR") "r" else "SMD") stop("Prepared scale contract failed.")
  if (!df$metaUI__direction[1] %in% c("unspecified", "positive", "negative")) stop("Invalid prepared direction.")
  invisible(TRUE)
}

metaUI_validate_upload <- function(df, built) {
  metaUI_validate_prepared(df)
  if (!nrow(df) || !all(names(built) %in% names(df))) stop("Uploaded data need all built app columns and at least one effect.")
  for (field in c("metaUI__es_type", "metaUI__display_scale", "metaUI__direction"))
    if (!identical(as.character(df[[field]][1]), as.character(built[[field]][1]))) stop("Uploaded scale/direction differs from the built app; build a fresh app for a new contract.")
  source_y <- df$metaUI__input_effect; source_v <- df$metaUI__input_variance
  if (!is.numeric(source_y) || !is.numeric(source_v) || any(!is.finite(source_y)) || any(!is.finite(source_v) | source_v <= 0)) stop("Invalid uploaded source effect/variance fields.")
  input_scale <- attr(built, "metaUI_validation")$input_scale
  expected_y <- source_y; expected_v <- source_v
  if (identical(input_scale, "COR")) {
    if (any(abs(source_y) >= 1)) stop("Uploaded COR source effects must be in (-1,1).")
    expected_y <- atanh(source_y); expected_v <- source_v / (1 - source_y^2)^2
  }
  if (any(abs(df$metaUI__effect_size - expected_y) > 1e-8 * pmax(1, abs(expected_y))) ||
      any(abs(df$metaUI__variance - expected_v) > 1e-6 * pmax(abs(expected_v), df$metaUI__variance)))
    stop("Uploaded source/fitting-scale fields disagree. Re-prepare original input with the declared transformation.")
  specs <- metaUI_computation_specs(built)
  metaUI_validate_alternatives(df, specs)
  if (length(specs)) {
    attr(df, "metaUI_alternatives") <- specs
    attr(df, "metaUI_input_scale") <- input_scale
    label <- attr(built, "metaUI_primary_label")
    attr(df, "metaUI_primary_label") <- if (is.null(label)) attr(built, "metaUI_validation")$primary_label else label
  }
  centre <- mean(built$metaUI__effect_size); spread <- stats::sd(built$metaUI__effect_size)
  degenerate <- !is.finite(spread) || spread == 0
  z <- if (degenerate) rep(0, nrow(df)) else (df$metaUI__effect_size - centre) / spread
  supplied <- df$metaUI__es_z
  disagreements <- if (!is.numeric(supplied)) nrow(df) else sum(!is.finite(supplied) | abs(supplied - z) > 1e-8 * pmax(1, abs(z)))
  df$metaUI__es_z <- z
  attr(df, "metaUI_upload_derivations") <- list(z_reference_mean = centre, z_reference_sd = spread,
    supplied_z_disagreements = disagreements, rule = if (degenerate) "Built SD unavailable/zero: descriptive z=0, outlier discrimination unavailable" else "Descriptive z=(fitting effect-built mean)/built SD; saved scale preserved")
  df
}

metaUI_pcurve_data <- function(df, selection = "first", require_N = FALSE) {
  metaUI_validate_prepared(df)
  if (!nrow(df)) stop("No eligible rows remain for p-curve.")
  if (df$metaUI__es_type[1] != "SMD") stop("P-curve is supported only for SMD.")
  direction <- df$metaUI__direction[1]
  if (!direction %in% c("positive", "negative")) stop("P-curve requires explicit effect direction; it is not inferred.")
  if (!selection %in% c("first", "last")) stop("Unknown p-curve selection rule.")
  groups <- split(seq_len(nrow(df)), as.character(df$metaUI__study_id))
  idx <- vapply(groups, function(x) if (selection == "first") x[1] else utils::tail(x, 1), integer(1))
  x <- df[idx, , drop = FALSE]
  if (direction == "negative") x$metaUI__effect_size <- -x$metaUI__effect_size
  excluded <- x$metaUI__effect_size <= 0
  x <- x[!excluded, , drop = FALSE]
  if (!nrow(x)) stop("No direction-consistent first/last effects remain for p-curve.")
  if (require_N && any(!is.finite(x$metaUI__N) | x$metaUI__N <= 2)) stop("P-curve effect estimation requires valid N > 2 for selected direction-consistent effects.")
  out <- data.frame(studlab = x$metaUI__study_id, TE = x$metaUI__effect_size,
                    seTE = x$metaUI__se, n = x$metaUI__N)
  attr(out, "direction_exclusions") <- sum(excluded)
  attr(out, "selection_exclusions") <- nrow(df) - length(idx)
  out
}

metaUI_pcurve_fit <- function(df, selection = "first") {
  x <- metaUI_pcurve_data(df, selection, require_N = TRUE)
  mod <- pcurve(x, effect.estimation = TRUE, N = x$n, dmin = 0, dmax = max(x$TE))
  if (df$metaUI__direction[1] == "negative") mod$dEstimate <- -mod$dEstimate
  mod
}

# Forest rows retain fitting-scale values and add the reader-facing scale.
# Effect intervals are normal sampling intervals, not shrunk predictions.
# Summaries are the unaggregated default multilevel/RVE fits, found by their code
# (`models` rows align with `estimates`) so renamed models are kept.
metaUI_forest_rows <- function(df, estimates, models = NULL) {
  df <- df[order(as.character(df$metaUI__study_id), as.character(df$metaUI__effect_id)), , drop = FALSE]
  label <- if ("metaUI__es_label" %in% names(df)) df$metaUI__es_label else df$metaUI__effect_id
  rows <- data.frame(label = paste(df$metaUI__study_id, label, sep = ": "),
    kind = "Selected effect", study = as.character(df$metaUI__study_id),
    effect_id = as.character(df$metaUI__effect_id), fit_es = df$metaUI__effect_size,
    fit_LCL = df$metaUI__effect_size - stats::qnorm(.975) * df$metaUI__se,
    fit_UCL = df$metaUI__effect_size + stats::qnorm(.975) * df$metaUI__se)
  rows$es <- rows$fit_es; rows$LCL <- rows$fit_LCL; rows$UCL <- rows$fit_UCL
  if (df$metaUI__display_scale[1] == "r")
    rows[c("es", "LCL", "UCL")] <- lapply(rows[c("es", "LCL", "UCL")], tanh)
  summary_rows <- if (!is.null(models) && nrow(models) == nrow(estimates))
    models$code %in% c(metaUI_code_multilevel, metaUI_code_rve) & !models$aggregated else
    estimates$Model %in% c("Random-Effects Multilevel Model", "Robust Variance Estimation")
  summaries <- estimates[summary_rows & estimates$status == "ok", ]
  if (nrow(summaries)) rows <- rbind(rows, data.frame(label = summaries$Model,
    kind = "Model summary", study = NA_character_, effect_id = NA_character_,
    fit_es = summaries$fit_es, fit_LCL = summaries$fit_LCL, fit_UCL = summaries$fit_UCL,
    es = summaries$es, LCL = summaries$LCL, UCL = summaries$UCL))
  rows$row <- rev(seq_len(nrow(rows)))
  rows
}

metaUI_forest_plot <- function(rows, scale) {
  ggplot2::ggplot(rows, ggplot2::aes(x = .data$es, y = .data$row)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey70", linetype = 2) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$LCL, xend = .data$UCL, yend = .data$row), colour = "#52616d") +
    ggplot2::geom_point(ggplot2::aes(shape = .data$kind), size = 2.8, colour = "#1c5a85", fill = "#1c5a85") +
    ggplot2::scale_shape_manual(values = c("Selected effect" = 16, "Model summary" = 23), guide = "none") +
    ggplot2::scale_y_continuous(breaks = rows$row, labels = rows$label) +
    ggplot2::labs(x = paste("Effect size (", scale, ")", sep = ""), y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
}

# Descriptive containment of existing intervals, without refitting or assuming
# custom-model coverage. Equality at a bound is deliberately inconclusive.
metaUI_interval_assessment <- function(estimates, bound = NULL, scale = "SMD", known_models = character()) {
  if (!is.null(bound) && (length(bound) != 1L || !is.numeric(bound) || !is.finite(bound) ||
      bound <= 0 || (scale == "r" && bound >= 1)))
    stop(if (scale == "r") "Enter a bound strictly between 0 and 1 for r." else "Enter a positive finite bound.")
  valid <- estimates$status == "ok" & is.finite(estimates$LCL) & is.finite(estimates$UCL) & estimates$LCL <= estimates$UCL
  threshold <- rep(NA_real_, nrow(estimates))
  threshold[valid] <- pmax(abs(estimates$LCL[valid]), abs(estimates$UCL[valid]))
  assessment <- rep("Not assessed", nrow(estimates))
  assessment[valid] <- "No bound chosen"
  if (!is.null(bound)) {
    assessment[valid] <- "Inconclusive"
    assessment[valid & estimates$LCL > -bound & estimates$UCL < bound] <- "Interval within bounds"
    assessment[valid & estimates$LCL > bound] <- "Exceeds positive bound"
    assessment[valid & estimates$UCL < -bound] <- "Exceeds negative bound"
  }
  bayesian <- if ("interval_type" %in% names(estimates)) grepl("credible", estimates$interval_type) else rep(FALSE, nrow(estimates))
  data.frame(Model = estimates$Model, bound = rep(if (is.null(bound)) NA_real_ else bound, nrow(estimates)),
    scale = rep(scale, nrow(estimates)), assessment = assessment, threshold = threshold,
    interval = ifelse(bayesian, "95% central credible interval (prior-dependent)", ifelse(estimates$Model %in% known_models, "Reported 95% CI", "Author-defined interval; error rate unknown")))
}
