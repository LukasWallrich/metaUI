# Links contain only a built-dataset identity and validated reader selections, in a short
# URL-safe form: ?metaui=<data key>_<change>_<change>... Each change is a field number
# (metaUI_link_ids), a letter and values joined by "~": k = levels kept, x = levels left out,
# r = range, n = effects without a value included (1/0), c = effect computation; b = equivalence
# bound. Levels and computations are numbered in the built data, which the data key pins down.
metaUI_link_key <- function(dataset_key) substr(gsub("[^A-Za-z0-9]", "", dataset_key), 1L, 8L)

metaUI_link_ids <- function(built, filters = list()) {
  ids <- c("outliers_z_scores", vapply(filters, function(x) x$id, character(1)),
    vapply(Filter(function(x) x$type == "numeric", filters), function(x) paste0(x$id, "_include_NA"), character(1)))
  if (length(metaUI_computation_specs(built))) ids <- c(ids, "computation")
  ids
}

# Shortest decimal that reads back as the same number; no "+", which a URL would turn into a space.
metaUI_link_number <- function(x) {
  out <- vapply(x, function(v) {
    s <- formatC(v, digits = 15, format = "g")
    if (as.numeric(s) != v) s <- formatC(v, digits = 17, format = "g")
    s
  }, character(1))
  gsub("+", "", trimws(out), fixed = TRUE)
}

metaUI_selection_query <- function(selections, dataset_key, bound = NULL, built, filters = list()) {
  # Only what narrows the data is saved; anything left out means "everything" (metaUI_full_selection).
  saved <- split(as.character(selections$selection), selections$id)
  full <- metaUI_full_selection(built, filters)
  ids <- metaUI_link_ids(built, filters)
  ranges <- c("outliers_z_scores", vapply(Filter(function(x) x$type == "numeric", filters), function(x) x$id, character(1)))
  untouched <- vapply(names(saved), function(id) {
    if (is.null(full[[id]])) return(FALSE)
    if (id %in% ranges) return(length(saved[[id]]) == 2L && all(abs(as.numeric(saved[[id]]) - as.numeric(full[[id]])) <= 1e-8)) # Absolute: a relative tolerance would drop real limits on large values.
    setequal(saved[[id]], full[[id]])
  }, logical(1))
  saved <- saved[!untouched]
  if (length(setdiff(names(saved), ids))) stop("Unknown saved filter IDs.")
  parts <- vapply(names(saved), function(id) {
    x <- saved[[id]]
    field <- match(id, ids)
    filter <- Filter(function(f) f$id == id, filters)
    if (id %in% ranges) return(paste0(field, "r", paste(metaUI_link_number(as.numeric(x)), collapse = "~")))
    if (id == "computation") return(paste0(field, "c", match(x, metaUI_computations(built))))
    if (length(filter)) {
      lv <- levels(built[[filter[[1]]$col]])
      at <- match(x[!is.na(x)], lv) # NA: nothing chosen.
      if (anyNA(at)) stop("Unknown or duplicated saved category.")
      left <- setdiff(seq_along(lv), at)
      # Whichever list is shorter: the levels kept or the levels left out.
      return(if (length(left) < length(at)) paste0(field, "x", paste(left, collapse = "~")) else paste0(field, "k", paste(at, collapse = "~")))
    }
    paste0(field, "n", as.integer(identical(x, "TRUE")))
  }, character(1))
  if (!is.null(bound)) parts <- c(parts, paste0("b", metaUI_link_number(bound)))
  query <- paste0("?metaui=", paste(c(metaUI_link_key(dataset_key), parts), collapse = "_"))
  if (nchar(query, type = "bytes") > 6000L) stop("This selection is too large for a shareable URL; use the workbook download.")
  query
}

# What a link means by a filter it does not mention: nothing excluded, the primary computation.
metaUI_full_selection <- function(built, filters = list()) {
  full <- list()
  if (any(is.finite(built$metaUI__es_z))) {
    spec <- metaUI_slider_spec(built$metaUI__es_z, integer = FALSE)
    full$outliers_z_scores <- c(spec$min, spec$max)
  }
  for (filter in filters) {
    if (filter$type == "numeric") {
      full[[paste0(filter$id, "_include_NA")]] <- "TRUE"
      if (any(is.finite(built[[filter$col]]))) { spec <- metaUI_slider_spec(built[[filter$col]]); full[[filter$id]] <- c(spec$min, spec$max) }
    } else full[[filter$id]] <- levels(built[[filter$col]])
  }
  if (length(metaUI_computation_specs(built))) full$computation <- "primary"
  full
}

metaUI_parse_selection <- function(query, dataset_key, built, filters = list()) {
  if (!is.character(query) || length(query) != 1L || !nzchar(query)) return(NULL)
  if (nchar(query, type = "bytes") > 6000L) stop("Selection URL exceeds the supported length.")
  params <- shiny::parseQueryString(query)
  damaged <- function() stop("The link is incomplete or damaged.", call. = FALSE)
  if (!identical(names(params), "metaui")) damaged()
  parts <- strsplit(params$metaui, "_", fixed = TRUE)[[1]]
  if (!length(parts) || !grepl("^[A-Za-z0-9]{1,8}$", parts[1])) damaged()
  if (!identical(parts[1], metaUI_link_key(dataset_key))) stop("This link was made for a different version of this app's data.")
  parts <- parts[-1]
  if (!all(grepl("^([0-9]+[kxrnc]|b)[-0-9.e~]*$", parts))) damaged()
  ids <- metaUI_link_ids(built, filters)
  ranges <- c("outliers_z_scores", vapply(Filter(function(x) x$type == "numeric", filters), function(x) x$id, character(1)))
  bound <- NULL
  saved <- list()
  for (part in parts) {
    kind <- sub("^[0-9]*([kxrncb]).*$", "\\1", part)
    values <- strsplit(sub("^[0-9]*[kxrncb]", "", part), "~", fixed = TRUE)[[1]]
    if (kind == "b") {
      bound <- suppressWarnings(as.numeric(values))
      if (length(bound) != 1L || !is.finite(bound)) damaged()
      next
    }
    id <- ids[as.integer(sub("[kxrnc].*$", "", part))]
    if (length(id) != 1L || is.na(id)) stop("Unknown saved filter IDs.")
    if (!is.null(saved[[id]])) damaged()
    filter <- Filter(function(f) f$id == id, filters)
    selection <- if (kind == "r" && id %in% ranges) values
      else if (kind == "n" && grepl("_include_NA$", id) && length(values) == 1L && values %in% c("0", "1")) c("FALSE", "TRUE")[as.integer(values) + 1L]
      else if (kind == "c" && id == "computation") {
        at <- suppressWarnings(as.integer(values))
        if (length(at) != 1L || is.na(at) || at < 1L || at > length(metaUI_computations(built))) stop("Unknown saved effect computation.")
        unname(metaUI_computations(built)[at])
      } else if (kind %in% c("k", "x") && length(filter) && filter[[1]]$type != "numeric") {
        lv <- levels(built[[filter[[1]]$col]])
        at <- suppressWarnings(as.integer(values))
        if (anyNA(at) || anyDuplicated(at) || any(at < 1L | at > length(lv))) stop("Unknown or duplicated saved category.")
        if (kind == "k") lv[at] else lv[-at]
      } else damaged()
    saved[[id]] <- data.frame(id = rep(id, length(selection)), selection = as.character(selection))
  }
  full <- metaUI_full_selection(built, filters)
  for (id in setdiff(intersect(ids, names(full)), names(saved))) saved[[id]] <- data.frame(id = id, selection = as.character(full[[id]]))
  if (length(setdiff(ids, names(saved)))) stop("Missing saved filter IDs.")
  for (id in ranges) {
    x <- suppressWarnings(as.numeric(saved[[id]]$selection))
    if (length(x) != 2L || any(!is.finite(x)) || x[1] > x[2] || !is.finite(diff(x)))
      stop("Invalid saved range: ", id)
    # Same bounds as the built sliders (metaUI_slider_spec), skipped if no finite values.
    spec <- if (id == "outliers_z_scores") {
      if (any(is.finite(built$metaUI__es_z))) metaUI_slider_spec(built$metaUI__es_z, integer = FALSE)
    } else {
      filter <- Filter(function(f) f$id == id, filters)[[1]]
      if (any(is.finite(built[[filter$col]]))) metaUI_slider_spec(built[[filter$col]])
    }
    if (!is.null(spec) && (x[1] < spec$min - 1e-8 || x[2] > spec$max + 1e-8)) stop("Saved range is outside the built slider: ", id)
  }
  for (filter in filters) if (filter$type != "numeric")
    saved[[filter$id]] <- data.frame(selection = as.character(saved[[filter$id]]$selection))
  if (!is.null(bound)) metaUI_interval_assessment(data.frame(Model = character(), status = character(), LCL = numeric(), UCL = numeric()), bound, built$metaUI__display_scale[1])
  list(filters = saved, sesoi = bound)
}

# Equivalence bound saved in a workbook; NA if none was chosen or recorded.
metaUI_saved_bound <- function(path, sheets = readxl::excel_sheets(path)) {
  if (!"equivalence" %in% sheets) return(NA_real_)
  saved <- unique(suppressWarnings(as.numeric(readxl::read_xlsx(path, "equivalence")[["bound"]])))
  if (length(saved) == 1L && is.finite(saved)) saved else NA_real_
}
