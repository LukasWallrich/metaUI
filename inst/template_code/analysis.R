# This file is copied verbatim into generated apps. Keep it independent of metaUI.

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

metaUI_model_reason <- function(spec, df) {
  if (!nrow(df)) return("No eligible rows")
  if (any(!is.finite(df$metaUI__effect_size)) || any(!is.finite(df$metaUI__variance) | df$metaUI__variance <= 0))
    return("Invalid required effect/variance inputs")
  if (spec$name %in% c("P-uniform star", "Hedges-Vevea Selection Model") &&
      !df$metaUI__direction[1] %in% c("positive", "negative"))
    return("Requires explicit positive or negative effect direction")
  if (spec$name %in% c("P-Curve (first value)", "P-Curve (last value)")) {
    if (df$metaUI__es_type[1] != "SMD") return("P-curve effect estimation only supported for SMD")
    selection <- if (spec$name == "P-Curve (last value)") "last" else "first"
    issue <- tryCatch({metaUI_pcurve_data(df, selection, require_N = TRUE); NULL}, error = function(e) conditionMessage(e))
    if (!is.null(issue)) return(issue)
  }
  if (spec$name %in% c("Precision Effect Test", "Precision Effect Estimate using Standard Error")) {
    predictor <- if (spec$name == "Precision Effect Test") sqrt(df$metaUI__variance) else df$metaUI__variance
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
  rows <- lapply(seq_len(nrow(models)), function(i) {
    spec <- models[i, , drop = FALSE]
    x <- if (isTRUE(spec$aggregated)) df_agg else df
    reason <- metaUI_model_reason(spec, x)
    warnings <- character()
    elapsed <- 0
    result <- c(es = NA_real_, LCL = NA_real_, UCL = NA_real_, k = NA_real_)
    status <- "unsupported"
    if (is.null(reason)) {
      started <- proc.time()[["elapsed"]]
      reason <- tryCatch({
        env <- new.env(parent = environment(metaUI_fit_models))
        env$df <- x
        # Reflect the inputs only for explicitly directional, right-sided models.
        reflected <- spec$name %in% c("P-uniform star", "Hedges-Vevea Selection Model") && x$metaUI__direction[1] == "negative"
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
                      fit_seconds = elapsed, row.names = NULL)
    row$fit_es <- row$es; row$fit_LCL <- row$LCL; row$fit_UCL <- row$UCL
    if (df$metaUI__es_type[1] == "ZCOR") row[c("es", "LCL", "UCL")] <- lapply(row[c("es", "LCL", "UCL")], tanh)
    row
  })
  list(df_agg = df_agg, table = dplyr::bind_rows(rows))
}

metaUI_validate_prepared <- function(df) {
  required <- c("metaUI__study_id", "metaUI__effect_id", "metaUI__effect_size", "metaUI__variance",
                "metaUI__se", "metaUI__es_type", "metaUI__display_scale", "metaUI__direction")
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
