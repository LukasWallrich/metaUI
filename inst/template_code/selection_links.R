# Links contain only a built-dataset identity and validated reader selections, readable as
# written: ?metaui=<data key>&country=Kuwait,Qatar&industry-sector!=Other&year=1990,2010.
# A filter is named by its label; "name=a,b" keeps those levels and "name!=a,b" keeps all others
# (whichever is shorter); a range gives its two ends; "name-missing=no" leaves out effects without
# a value; z, computation and bound hold the z-score range, the effect computation and the
# equivalence bound. Anything not mentioned is left as built; other parameters are ignored.
metaUI_link_key <- function(dataset_key) substr(gsub("[^A-Za-z0-9]", "", dataset_key), 1L, 8L)

metaUI_link_slug <- function(x) gsub("^-+|-+$", "", gsub("[^a-z0-9]+", "-", tolower(x)))

# Link name for each saved field, keyed by its input ID.
metaUI_link_names <- function(built, filters = list()) {
  names <- c(outliers_z_scores = "z")
  taken <- c("metaui", "z", "computation", "bound")
  for (filter in filters) {
    name <- metaUI_link_slug(sub("^metaUI__filter_", "", filter$col))
    if (!nzchar(name)) name <- "filter"
    base <- name; i <- 1L
    while (name %in% taken || paste0(name, "-missing") %in% taken) { i <- i + 1L; name <- paste0(base, "-", i) }
    taken <- c(taken, name, paste0(name, "-missing"))
    names[[filter$id]] <- name
    if (filter$type == "numeric") names[[paste0(filter$id, "_include_NA")]] <- paste0(name, "-missing")
  }
  if (length(metaUI_computation_specs(built))) names[["computation"]] <- "computation"
  names
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

# Level text in a link: spaces as "+", and anything that would end a value (",", "&", "+") escaped.
metaUI_link_text <- function(x) gsub("%20", "+", vapply(x, utils::URLencode, character(1), reserved = TRUE, repeated = TRUE, USE.NAMES = FALSE), fixed = TRUE)
metaUI_link_untext <- function(x) vapply(gsub("+", " ", x, fixed = TRUE), utils::URLdecode, character(1), USE.NAMES = FALSE)

metaUI_computation_names <- function(built) {
  choices <- metaUI_computations(built)
  stats::setNames(make.unique(metaUI_link_slug(names(choices)), sep = "-"), choices)
}

metaUI_selection_query <- function(selections, dataset_key, bound = NULL, built, filters = list()) {
  # Only what narrows the data is saved; anything left out means "everything" (metaUI_full_selection).
  saved <- split(as.character(selections$selection), selections$id)
  full <- metaUI_full_selection(built, filters)
  link_names <- metaUI_link_names(built, filters)
  ranges <- c("outliers_z_scores", vapply(Filter(function(x) x$type == "numeric", filters), function(x) x$id, character(1)))
  untouched <- vapply(names(saved), function(id) {
    if (is.null(full[[id]])) return(FALSE)
    if (id %in% ranges) return(length(saved[[id]]) == 2L && all(abs(as.numeric(saved[[id]]) - as.numeric(full[[id]])) <= 1e-8)) # Absolute: a relative tolerance would drop real limits on large values.
    setequal(saved[[id]], full[[id]])
  }, logical(1))
  saved <- saved[!untouched]
  if (length(setdiff(names(saved), names(link_names)))) stop("Unknown saved filter IDs.")
  ids <- intersect(names(link_names), names(saved))
  parts <- vapply(ids, function(id) {
    x <- saved[[id]]
    name <- link_names[[id]]
    filter <- Filter(function(f) f$id == id, filters)
    if (id %in% ranges) return(paste0(name, "=", paste(metaUI_link_number(as.numeric(x)), collapse = ",")))
    if (id == "computation") return(paste0(name, "=", metaUI_computation_names(built)[[x]]))
    if (length(filter)) {
      lv <- levels(built[[filter[[1]]$col]])
      kept <- x[!is.na(x)] # NA: nothing chosen.
      if (!all(kept %in% lv)) stop("Unknown or duplicated saved category.")
      left <- setdiff(lv, kept)
      # Whichever list is shorter: the levels kept or the levels left out.
      return(if (length(left) < length(kept)) paste0(name, "!=", paste(metaUI_link_text(left), collapse = ","))
        else paste0(name, "=", paste(metaUI_link_text(lv[lv %in% kept]), collapse = ",")))
    }
    paste0(name, "=", if (identical(x, "TRUE")) "yes" else "no")
  }, character(1))
  if (!is.null(bound)) parts <- c(parts, paste0("bound=", metaUI_link_number(bound)))
  query <- paste0("?", paste(c(paste0("metaui=", metaUI_link_key(dataset_key)), parts), collapse = "&"))
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
  damaged <- function() stop("The link is incomplete or damaged.", call. = FALSE)
  # Read by hand: a decoded value could no longer tell a separating comma from one in a level.
  pairs <- strsplit(sub("^\\?", "", query), "&", fixed = TRUE)[[1]]
  pairs <- pairs[nzchar(pairs)]
  if (!length(pairs) || !all(grepl("=", pairs, fixed = TRUE))) damaged()
  keys <- tolower(metaUI_link_untext(sub("=.*$", "", pairs)))
  values <- sub("^[^=]*=", "", pairs)
  if (keys[1] != "metaui" || !grepl("^[A-Za-z0-9]{1,8}$", values[1])) damaged()
  # A link made before the data were updated is applied as far as it still fits: categories that
  # are gone are dropped, ranges are trimmed to the new sliders, and each change is reported.
  stale <- !identical(values[1], metaUI_link_key(dataset_key))
  adjusted <- character()
  keys <- keys[-1]; values <- values[-1]
  excluded <- endsWith(keys, "!")
  keys <- sub("!$", "", keys)
  if (anyDuplicated(keys)) damaged()
  link_names <- metaUI_link_names(built, filters)
  ranges <- c("outliers_z_scores", vapply(Filter(function(x) x$type == "numeric", filters), function(x) x$id, character(1)))
  bound <- NULL
  saved <- list()
  for (i in seq_along(keys)) {
    items <- if (nzchar(values[i])) strsplit(values[i], ",", fixed = TRUE)[[1]] else character()
    if (keys[i] == "bound" && !excluded[i]) {
      bound <- suppressWarnings(as.numeric(items))
      if (length(bound) != 1L || !is.finite(bound)) damaged()
      next
    }
    id <- names(link_names)[link_names == keys[i]]
    if (!length(id)) next # Not ours, such as a tracking tag a site added.
    filter <- Filter(function(f) f$id == id, filters)
    selection <- if (excluded[i] && !(length(filter) && filter[[1]]$type != "numeric")) damaged()
      else if (id %in% ranges) items
      else if (grepl("_include_NA$", id) && length(items) == 1L && items %in% c("yes", "no")) c("FALSE", "TRUE")[(items == "yes") + 1L]
      else if (id == "computation") {
        at <- match(items, metaUI_computation_names(built))
        if (length(at) != 1L || is.na(at)) {
          if (!stale) stop("Unknown saved effect computation.")
          adjusted <- c(adjusted, "its effect computation no longer exists, so the primary one is used")
          next
        }
        unname(metaUI_computations(built)[at])
      } else if (length(filter)) {
        lv <- levels(built[[filter[[1]]$col]])
        x <- tryCatch(metaUI_link_untext(items), error = function(e) damaged())
        if (anyDuplicated(x)) damaged()
        if (any(!x %in% lv)) {
          if (!stale) stop("Unknown or duplicated saved category.")
          label <- sub("^metaUI__filter_", "", filter[[1]]$col)
          gone <- x[!x %in% lv]
          x <- x[x %in% lv]
          if (!excluded[i]) adjusted <- c(adjusted, if (length(x)) paste0(label, ": ", paste(gone, collapse = ", "), " no longer in the data")
            else paste0(label, ": none of its categories remain, so all are included"))
          if (!excluded[i] && !length(x)) next
        }
        if (excluded[i]) setdiff(lv, x) else x
      } else damaged()
    saved[[id]] <- data.frame(id = rep(id, length(selection)), selection = as.character(selection))
  }
  full <- metaUI_full_selection(built, filters)
  for (id in setdiff(intersect(names(link_names), names(full)), names(saved))) saved[[id]] <- data.frame(id = id, selection = as.character(full[[id]]))
  if (length(setdiff(names(link_names), names(saved)))) stop("Missing saved filter IDs.")
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
    if (!is.null(spec) && (x[1] < spec$min - 1e-8 || x[2] > spec$max + 1e-8)) {
      if (!stale) stop("Saved range is outside the built slider: ", id)
      label <- if (id == "outliers_z_scores") "z-score" else sub("^metaUI__filter_", "", Filter(function(f) f$id == id, filters)[[1]]$col)
      x <- c(max(x[1], spec$min), min(x[2], spec$max))
      if (x[1] > x[2]) {
        x <- c(spec$min, spec$max)
        adjusted <- c(adjusted, paste0(label, ": its range no longer overlaps the data, so the full range is used"))
      } else adjusted <- c(adjusted, paste0(label, ": its range was trimmed to the data"))
      saved[[id]]$selection <- as.character(x)
    }
  }
  for (filter in filters) if (filter$type != "numeric")
    saved[[filter$id]] <- data.frame(selection = as.character(saved[[filter$id]]$selection))
  if (!is.null(bound)) metaUI_interval_assessment(data.frame(Model = character(), status = character(), LCL = numeric(), UCL = numeric()), bound, built$metaUI__display_scale[1])
  list(filters = saved, sesoi = bound, stale = stale, adjusted = adjusted)
}

# Equivalence bound saved in a workbook; NA if none was chosen or recorded.
metaUI_saved_bound <- function(path, sheets = readxl::excel_sheets(path)) {
  if (!"equivalence" %in% sheets) return(NA_real_)
  saved <- unique(suppressWarnings(as.numeric(readxl::read_xlsx(path, "equivalence")[["bound"]])))
  if (length(saved) == 1L && is.finite(saved)) saved else NA_real_
}
