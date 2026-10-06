# Definitions are retained in preparation metadata and standalone app helpers.
metaUI_computation_specs <- function(df) {
  specs <- attr(df, "metaUI_alternatives")
  if (is.null(specs)) specs <- attr(df, "metaUI_validation")$alternatives
  if (is.null(specs)) list() else specs
}

metaUI_computations <- function(df) {
  specs <- metaUI_computation_specs(df)
  label <- attr(df, "metaUI_primary_label")
  if (is.null(label)) label <- attr(df, "metaUI_validation")$primary_label
  if (is.null(label)) label <- "As supplied"
  c(setNames("primary", label), setNames(vapply(specs, function(x) x$id, character(1)),
    vapply(specs, function(x) x$name, character(1))))
}

metaUI_select_computation <- function(df, id = "primary", contract = df) {
  for (field in c("metaUI_alternatives", "metaUI_validation", "metaUI_input_scale", "metaUI_primary_label"))
    attr(df, field) <- attr(contract, field)
  choices <- metaUI_computations(contract)
  if (length(id) != 1L || !is.character(id) || !id %in% choices) stop("Unknown effect-size computation.")
  if (!length(metaUI_computation_specs(df))) return(df)
  if (id == "primary") {
    scale <- attr(df, "metaUI_input_scale")
    if (is.null(scale)) scale <- attr(df, "metaUI_validation")$input_scale
    y <- df$metaUI__input_effect; v <- df$metaUI__input_variance
    if (identical(scale, "COR")) { v <- v / (1 - y^2)^2; y <- atanh(y) }
  } else {
    spec <- Filter(function(x) x$id == id, metaUI_computation_specs(df))[[1]]
    y <- df[[spec$fit_effect]]; v <- df[[spec$fit_variance]]
  }
  df$metaUI__effect_size <- y; df$metaUI__variance <- v; df$metaUI__se <- sqrt(v)
  attr(df, "metaUI_computation") <- id
  metaUI_validate_prepared(df)
  df
}

metaUI_validate_alternatives <- function(df, specs) {
  for (spec in specs) {
    columns <- unlist(spec[c("input_effect", "input_variance", "fit_effect", "fit_variance")], use.names = FALSE)
    if (!all(columns %in% names(df))) stop("Missing alternative computation columns: ", spec$name)
    for (field in columns)
      if (!is.numeric(df[[field]]) || any(!is.finite(df[[field]]))) stop("Invalid alternative inputs: ", spec$name)
    y <- df[[spec$input_effect]]; v <- df[[spec$input_variance]]
    if (any(v <= 0) || any(df[[spec$fit_variance]] <= 0)) stop("Invalid alternative variance: ", spec$name)
    if (spec$input_scale == "COR") {
      if (any(abs(y) >= 1)) stop("Alternative COR effects must be in (-1,1): ", spec$name)
      v <- v / (1 - y^2)^2; y <- atanh(y)
    }
    if (any(!is.finite(y)) || any(!is.finite(v))) stop("Invalid alternative fitting-scale inputs: ", spec$name)
    if (any(abs(y - df[[spec$fit_effect]]) > 1e-8 * pmax(1, abs(y))) ||
        any(abs(v - df[[spec$fit_variance]]) > 1e-6 * pmax(v, df[[spec$fit_variance]])))
      stop("Alternative source/fitting-scale fields disagree: ", spec$name)
  }
  invisible(TRUE)
}
