# Links contain only a built-dataset identity and validated reader selections.
metaUI_selection_query <- function(selections, dataset_key, bound = NULL) {
  payload <- list(dataset = dataset_key, filters = selections, sesoi = bound)
  encoded <- utils::URLencode(jsonlite::toJSON(payload, auto_unbox = TRUE, na = "null", null = "null"), reserved = TRUE)
  query <- paste0("?metaui=1&selection=", encoded)
  if (nchar(query, type = "bytes") > 6000L) stop("This selection is too large for a shareable URL; use the workbook download.")
  query
}

metaUI_parse_selection <- function(query, dataset_key, built, filters = list()) {
  if (!is.character(query) || length(query) != 1L || !nzchar(query)) return(NULL)
  if (nchar(query, type = "bytes") > 6000L) stop("Selection URL exceeds the supported length.")
  params <- shiny::parseQueryString(query)
  if (anyDuplicated(names(params)) || !setequal(names(params), c("metaui", "selection")) || !identical(params$metaui, "1"))
    stop("Unrecognised selection URL version or parameters; no filters were applied.")
  payload <- jsonlite::fromJSON(params$selection)
  if (!is.list(payload) || !setequal(names(payload), c("dataset", "filters", "sesoi")) ||
      !identical(payload$dataset, dataset_key)) stop("This link belongs to a different dataset/build or has an invalid payload.")
  records <- payload$filters
  if (!is.data.frame(records) || !setequal(names(records), c("id", "selection")) ||
      !is.character(records$id) || anyNA(records$id) || !is.atomic(records$selection))
    stop("Invalid saved selection records.")
  saved <- split(records, records$id)
  ids <- c("outliers_z_scores", vapply(filters, function(x) x$id, character(1)),
    vapply(Filter(function(x) x$type == "numeric", filters), function(x) paste0(x$id, "_include_NA"), character(1)))
  if (length(metaUI_computation_specs(built))) ids <- c(ids, "computation")
  if (!setequal(names(saved), ids)) stop("Missing or unknown saved filter IDs.")
  ranges <- c("outliers_z_scores", vapply(Filter(function(x) x$type == "numeric", filters), function(x) x$id, character(1)))
  for (id in ranges) {
    x <- suppressWarnings(as.numeric(saved[[id]]$selection))
    if (length(x) != 2L || any(!is.finite(x)) || x[1] > x[2] || !is.finite(diff(x)))
      stop("Invalid saved range: ", id)
    if (id == "outliers_z_scores") {
      values <- built$metaUI__es_z; digits <- 2
    } else {
      filter <- Filter(function(f) f$id == id, filters)[[1]]
      values <- built[[filter$col]]; values <- values[is.finite(values)]
      l <- log10(max(abs(values)))
      digits <- if (is.finite(l)) max(3, floor(l) + 2) else 3
    }
    limits <- c(signif_floor(min(values), digits), signif_ceiling(max(values), digits))
    if (x[1] < limits[1] - 1e-8 || x[2] > limits[2] + 1e-8) stop("Saved range is outside the built slider: ", id)
  }
  for (filter in filters) {
    if (filter$type == "numeric") {
      flag <- saved[[paste0(filter$id, "_include_NA")]]$selection
      if (length(flag) != 1L || is.na(flag) || !flag %in% c("TRUE", "FALSE")) stop("Invalid missing-value choice.")
    } else {
      x <- saved[[filter$id]]$selection
      if (length(x) == 1L && is.na(x)) x <- character()
      if (anyNA(x) || anyDuplicated(x) || any(!x %in% levels(built[[filter$col]]))) stop("Unknown or duplicated saved category.")
      saved[[filter$id]] <- data.frame(selection = as.character(x))
    }
  }
  if ("computation" %in% ids) {
    id <- saved$computation$selection
    if (length(id) != 1L || is.na(id) || !id %in% metaUI_computations(built)) stop("Unknown saved effect computation.")
  }
  bound <- payload$sesoi
  if (!is.null(bound)) metaUI_interval_assessment(data.frame(Model = character(), status = character(), LCL = numeric(), UCL = numeric()), bound, built$metaUI__display_scale[1])
  list(filters = saved, sesoi = bound)
}
