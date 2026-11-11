#' Import data from a file and return a data frame
#'
#' You may want to use the [metafor::escalc()] function in the metafor package to calculate effect sizes and their variances in preparation for this function.
#'
#' HTML escapes in text and factor columns, such as `&amp;` or `&#8211;` left by
#' reference managers and web exports, are decoded (to `&` and an en dash) so that
#' apps show plain text; a message names the columns changed.
#'
#' @param data path to the .csv file to read OR a data frame
#' @param study_label Character. Name of the field to use as the id/study label
#' @param es_field Character. Name of the field to use as the effect size
#' @param sample_size Character. Name of the field to use as the sample size
#' @param variance Character. Name of the field to use as the sampling variances.
#' @param se Character. Name of the field to use as the standard error for the effect size.
#' @param pvalue Character. Name of the field to use as the p-value for the effect size.
#' @param filters Character. List of fields to use as filters - can be named if different labels should be displayed. Labels must be distinct, contain no backticks, and stay distinct when spaces and symbols are replaced by underscores (as in Shiny input IDs); a numeric filter needs at least one value.
#' @param url Character. Field with URLs or DOIs to link to. DOIs can be in the format "10.1234/5678" or full links. Defaults to NA.
#' @param article_label Character. Field with article labels. Only used to report number of references in addition to number of independent samples.
#' @param es_type Character. Input scale: SMD, COR (raw r), ZCOR (Fisher z), OR (log odds ratio) or RR (log risk ratio). COR needs r-scale variance and uses delta-method variance conversion; ZCOR needs z-scale variance. OR and RR take effects and variances on the log scale (`variance_scale = "log"`), as [metafor::escalc()] returns them, or 2x2 counts (`counts`); models are fitted on the log scale and results shown as ratios.
#' @param es_label Character. Label for individual effect sizes - only needed when there are multiple effect sizes per study/sample. Defaults to NA. Defaults to NA, in that case, multiple effect sizes are simply numbered.
#' @param na.rm Should rows with any missing values be removed? Can be TRUE, FALSE or "es_related" -
#' the last is the default and drops rows with missing values for any of the variables used in the standard meta-analysis models, namely
#' `study_label`, `es_field`, `variance`, and `se`. Optional p/N never remove rows under this default. Setting this to TRUE also drops rows with a missing value in any filter (categorical
#' filters then get no `(Missing)` level) or in the p-value or sample size when these are given; other columns of `data` are ignored. This might often be unnecessary. Conversely, setting this to FALSE might lead to issues in the model - unless you post-process the data
#' or change the models and analyses to be included in the app. Invalid required inputs with FALSE cause an error.
#' @param arrange_filters Character. How should the filters be arranged in the app? Options are "given" by the `filters` argument, "alphabetical" (by label) or "leave" (as they are in the dataset). Defaults to "given".
#' @param keep_missing_level Logical. Should a `(Missing)`-level be kept even for filters that do not have missing values? Might be advisable when you expect users to upload new data with missing values. Defaults to FALSE.
#'
#' @param categorical_filters Explicitly convert named filter columns to factors with radix-sorted levels. Useful for CSV/JSON builds.
#' @param es_id Optional column with unique effect IDs within each study. Defaults to input row IDs.
#' @param variance_scale Required for COR (r), ZCOR (z), and OR or RR given as log ratios (log); optional SMD for SMD.
#' @param counts For OR and RR: named character vector mapping 2x2 count columns, as in [metafor::escalc()]:
#'   `ai` and `ci` (events in the first and second group) with either `n1i` and `n2i` (group totals) or `bi` and `di`
#'   (non-events). The log ratio and its variance are computed with `metafor::escalc()` and its defaults: 0.5 is added
#'   to every cell of a table that has a zero cell, and tables without events (or without non-events) in both groups are kept.
#'   Leave `es_field`, `variance` and `se` empty. The count columns become editable in downloaded workbooks.
#'   Without `sample_size`, N is the total of the four cells; with it, N is checked against that total and
#'   rows that differ are named in a warning (N may legitimately count, say, everyone randomised).
#' @param double_zero For `counts` only: what to do with tables that have no events in either group, or only
#'   events (a "double-zero" table, `ai = ci = 0` or `bi = di = 0`). `"keep"` (the default, as in
#'   [metafor::escalc()]) adds 0.5 to every cell of such a table. A table without events then gives a ratio set
#'   mainly by the group sizes, with a large variance; for risk ratios, a table with events in everyone gives a
#'   ratio near 1 whose variance can be small. `"exclude"` leaves both kinds out with a reason, as
#'   `escalc(drop00 = TRUE)` does. The app's Methods state the choice.
#' @param direction Explicit direction for one-sided bias models: unspecified, positive, or negative.
#' @param primary_label Label for the primary computation when alternatives are provided.
#' @param alternatives Named list of up to five author-defined effect computations. Each entry supplies es_field, variance or se, es_type, variance_scale when required, and a nonempty justification. Every computation must retain the same effects and converge on the same fitting scale.
#' @return tibble with the data from the file/input reformatted for metaUI
#' @export
#' @examples
#' raw <- data.frame(study = letters[1:4], d = c(.1, .3, -.1, .4), vi = rep(.02, 4))
#' prepare_data(raw, "study", "d", variance = "vi")
prepare_data <- function(data, study_label, es_field = NULL, se = NULL, pvalue = NULL, sample_size = NULL, variance = NULL, filters = character(),
                         url = NA, es_type = "SMD", article_label = NA, es_label = NA, na.rm = "es_related",
                         arrange_filters = c("given", "alphabetical", "leave"), keep_missing_level = FALSE, es_id = NULL,
                         variance_scale = NULL, direction = c("unspecified", "positive", "negative"),
                         categorical_filters = character(), alternatives = list(), primary_label = "As supplied", counts = NULL, double_zero = c("keep", "exclude")) {
  # The p-value and N columns the author gave; internal placeholders added later do not count for na.rm.
  supplied_pn <- c(pvalue, sample_size)
  source <- list(kind = if (is.character(data)) "CSV" else "data.frame",
                 file = if (is.character(data)) basename(data) else NULL,
                 md5 = if (is.character(data)) unname(tools::md5sum(data)) else NULL)
  if (is.character(data)) {
    # Read the file
    data <- read.csv(data, stringsAsFactors = FALSE)
  }
  check_removed_example(data)
  raw_data <- data # undecoded, as alternatives are prepared (and decoded) from it again
  data <- decode_text_entities(data)
  alternative_base <- list(study_label = study_label, es_field = es_field, se = se,
    pvalue = pvalue, sample_size = sample_size, variance = variance, filters = filters,
    url = url, es_type = es_type, article_label = article_label, es_label = es_label,
    na.rm = na.rm, arrange_filters = arrange_filters, keep_missing_level = keep_missing_level,
    es_id = es_id, variance_scale = variance_scale, direction = direction,
    categorical_filters = categorical_filters)
  if (!is.list(alternatives) || length(alternatives) > 5L ||
      (length(alternatives) && (is.null(names(alternatives)) || anyNA(names(alternatives)) || any(!nzchar(trimws(names(alternatives)))) ||
        anyDuplicated(names(alternatives)) || "As supplied" %in% names(alternatives))))
    stop("alternatives must be a uniquely named list of at most five computations.")

  if (!is.character(primary_label) || length(primary_label) != 1L || is.na(primary_label) || !nzchar(trimws(primary_label)) || primary_label %in% names(alternatives)) stop("primary_label must be a unique nonempty string.")
  direction <- match.arg(direction)
  if (!es_type %in% c("SMD", "COR", "ZCOR", "OR", "RR"))
    stop("Supported metrics are SMD, COR, ZCOR, OR and RR; other metrics require custom analysis.")
  derived <- character()
  count_cells <- NULL
  count_N_mismatches <- NA_integer_
  if (!is.null(counts)) {
    if (!es_type %in% c("OR", "RR")) stop("counts are used only for es_type OR or RR.")
    if (!is.null(es_field) || !is.null(variance) || !is.null(se) || !is.null(variance_scale))
      stop("With counts, leave es_field, variance, se and variance_scale empty: the effects are computed from the counts.")
    double_zero <- match.arg(double_zero)
    counts <- metaUI_check_counts(counts, data)
    count_cells <- metaUI_count_effects(data, counts, es_type)
    data$metaUI_input_yi <- count_cells$yi
    data$metaUI_input_vi <- count_cells$vi
    es_field <- "metaUI_input_yi"; variance <- "metaUI_input_vi"; variance_scale <- "log"
    derived <- c(derived, paste0("log ", if (es_type == "OR") "odds" else "risk", " ratio and variance from 2x2 counts with metafor::escalc(measure = \"", es_type,
      "\"): 0.5 added to every cell of tables with a zero cell; tables with no events (or only events) in both groups ", if (double_zero == "keep") "kept" else "excluded"))
    if (is.null(sample_size)) {
      data$metaUI_input_N <- count_cells$total
      sample_size <- "metaUI_input_N"
      derived <- c(derived, "N = total of the 2x2 counts")
    } else if (is.numeric(data[[sample_size]])) {
      differs <- which(is.finite(data[[sample_size]]) & is.finite(count_cells$total) & abs(data[[sample_size]] - count_cells$total) > 1e-8)
      if (length(differs)) warning("Sample size (", sample_size, ") differs from the total of the 2x2 counts in ", length(differs),
        if (length(differs) == 1L) " row" else " rows", " (rows ", paste(utils::head(differs, 10), collapse = ", "), if (length(differs) > 10L) ", ..." else "",
        "). Check whether N counts the same people; leave sample_size empty to use the count total.", call. = FALSE)
      count_N_mismatches <- length(differs)
    }
  } else if (!missing(double_zero)) stop("double_zero applies only with counts.")
  if (is.null(es_field)) stop("Supply es_field, or counts for OR and RR.")
  if (es_type %in% c("OR", "RR") && !identical(variance_scale, "log"))
    stop(es_type, " takes log ratios: set variance_scale = 'log' for effects and variances on the log scale (as metafor::escalc returns them), or supply 2x2 counts.")
  if (es_type == "COR" && !identical(variance_scale, "r"))
    stop("COR requires variance_scale = 'r' with supplied r-scale variance. No N-based variance is inferred.")
  if (es_type == "ZCOR" && !identical(variance_scale, "z"))
    stop("ZCOR requires variance_scale = 'z'.")
  if (es_type == "SMD" && !is.null(variance_scale) && variance_scale != "SMD")
    stop("SMD requires SMD-scale variance.")
  if (is.null(variance) && is.null(se)) stop("Supply variance or se on the declared input scale.")
  mappings <- c(study_label, es_field, se, pvalue, sample_size, variance, filters, es_id, unname(counts))
  if (any(!mappings %in% names(data))) stop("Mapped column not found: ", paste(setdiff(mappings, names(data)), collapse = ", "))
  if (anyDuplicated(mappings)) stop("Column mappings must be distinct.")
  original_rows <- seq_len(nrow(data))
  effect_ids_input <- if (is.null(es_id)) original_rows else data[[es_id]]
  labels_input <- NULL
  if (!is.na(es_label)) {
    if (!es_label %in% names(data)) stop("Mapped label column not found: ", es_label)
    labels_input <- data[[es_label]]
  }
  if (is.null(se)) {
    data$metaUI_input_se <- sqrt(data[[variance]])
    se <- "metaUI_input_se"
    derived <- c(derived, "SE = sqrt(supplied variance)")
  }
  if (is.null(variance)) {
    data$metaUI_input_variance <- data[[se]]^2
    variance <- "metaUI_input_variance"
    derived <- c(derived, "variance = supplied SE^2")
  }
  for (field in c(es_field, se, variance, pvalue, sample_size))
    if (!is.numeric(data[[field]])) stop("Numeric input required: ", field)
  if (is.null(pvalue)) { data$metaUI_input_p <- NA_real_; pvalue <- "metaUI_input_p" }
  if (is.null(sample_size)) { data$metaUI_input_N <- NA_real_; sample_size <- "metaUI_input_N" }
  consistent <- is.finite(data[[se]]) & is.finite(data[[variance]])
  if (any(abs(data[[se]][consistent]^2 - data[[variance]][consistent]) >
          1e-6 * pmax(abs(data[[variance]][consistent]), data[[se]][consistent]^2)))
    stop("SE and variance disagree on the declared input scale.")
  if (any(!categorical_filters %in% filters)) stop("categorical_filters must name mapped filter columns.")
  for (field in categorical_filters) {
    if (!is.character(data[[field]]) && !is.logical(data[[field]]) && !is.factor(data[[field]])) stop("Categorical conversion requires text, logical, or factor input: ", field)
    data[[field]] <- factor(data[[field]], levels = sort(unique(as.character(data[[field]][!is.na(data[[field]])])), method = "radix"))
    derived <- c(derived, paste0("Explicit categorical filter: ", field, " (radix-sorted levels)"))
  }
  for (i in seq_along(filters)) {
    if(!is.factor(data[[filters[i]]]) && !is.numeric(data[[filters[i]]])) {
      stop("All filters/moderators must be factors or numeric. This check failed first for ", filters[i],
        if (is.character(data[[filters[i]]]) || is.logical(data[[filters[i]]])) paste0(". To use a text column as categories, name it in categorical_filters, e.g. categorical_filters = \"", filters[i], "\".") else "")
    }
  }

  if (!is.null(names(filters))) {
    filter_names <- names(filters) %>%
      dplyr::na_if("") %>%
      dplyr::coalesce(filters) %>%
      setNames(filters)
    names(filters) <- NULL
  } else {
    filter_names <- filters %>% setNames(filters)
  }
  if (length(filters)) metaUI_check_filter_columns(paste0("metaUI__filter_", filter_names), data[filters])

  # Under na.rm = TRUE, a row missing any filter, p-value or N it was given counts as incomplete;
  # this is recorded before categorical NAs become "(Missing)". Unmapped columns do not count.
  missing_mapped <- if (length(c(filters, supplied_pn))) !stats::complete.cases(data[c(filters, supplied_pn)]) else rep(FALSE, nrow(data))

  # Set NA level for categorical filters ...
  for (i in seq_along(filters)) {
    if(is.factor(data[[filters[i]]])) {
      data[[filters[i]]] <- na_to_level(data[[filters[i]]], "(Missing)")
      # ... if there are any NA
      if (!keep_missing_level) {
        data[[filters[i]]] <- droplevels(data[[filters[i]]])
      }
    }
  }

  # Arrange the filters
  arrange_filters <- match.arg(arrange_filters)
  if (!arrange_filters == "leave") {
    if (arrange_filters == "alphabetical") {
      filters <- filters[order(filter_names[filters], method = "radix")]
    }
    data <- data %>% dplyr::select(dplyr::all_of(filters), dplyr::everything())
  }

  # Rename the fields
  data <- data %>%
    dplyr::rename(
      "metaUI__study_id" = !!rlang::sym(study_label), "metaUI__effect_size" = !!rlang::sym(es_field),
      "metaUI__se" = !!rlang::sym(se),
      "metaUI__pvalue" = !!rlang::sym(pvalue),
      "metaUI__N" = !!rlang::sym(sample_size), "metaUI__variance" = !!rlang::sym(variance)
    ) %>%
    dplyr::rename_with(~ if (length(.x)) paste0("metaUI__filter_", filter_names[.x]) else character(), .cols = dplyr::all_of(filters)) %>%
    dplyr::mutate(metaUI__es_type = !!es_type)
  # Counts keep stable names, so that an uploaded workbook can recompute the effects from them.
  if (!is.null(counts)) data <- dplyr::rename(data, !!!rlang::syms(stats::setNames(unname(counts), paste0("metaUI__", names(counts)))))

  if (!is.na(url)) {
    data <- data %>%
      dplyr::rename("metaUI__url" = !!rlang::sym(url)) %>%
      dplyr::mutate(metaUI__url = ifelse(grepl("^10\\.", .data$metaUI__url), paste0("https://doi.org/", .data$metaUI__url), .data$metaUI__url))
  }
  if (!is.na(article_label)) {
    data <- data %>%
      dplyr::rename("metaUI__article_label" = !!rlang::sym(article_label))
  }

  if (!is.na(es_label)) {
    # A display label may share the study/ID column already renamed above.
    if (es_label %in% names(data) && !startsWith(es_label, "metaUI__")) data[[es_label]] <- NULL
    data$metaUI__es_label <- labels_input
  } else {
    data <- data %>%
      dplyr::group_by(.data$metaUI__study_id) %>%
      dplyr::mutate(metaUI__es_label = dplyr::row_number()) %>%
      dplyr::ungroup()
  }
  # Labels are presentation only: equal labels/values must not collapse effects.
  if (is.null(es_id)) {
    data$metaUI__effect_id <- original_rows
    derived <- c(derived, "effect ID = original input row (labels are not IDs)")
  } else {
    ids <- effect_ids_input
    if (anyNA(ids) || any(trimws(as.character(ids)) == "") ||
        anyDuplicated(data.frame(study = data$metaUI__study_id, id = ids)))
      stop("Effect IDs must be nonmissing and unique within study.")
    data$metaUI__effect_id <- ids
  }
  reason <- rep("", nrow(data))
  add_reason <- function(bad, text) {
    reason[bad] <<- ifelse(reason[bad] == "", text, paste(reason[bad], text, sep = "; "))
  }
  add_reason(is.na(data$metaUI__study_id) | trimws(as.character(data$metaUI__study_id)) == "", "missing study ID")
  add_reason(!is.finite(data$metaUI__effect_size), "nonfinite effect")
  add_reason(!is.finite(data$metaUI__variance) | data$metaUI__variance <= 0, "invalid variance")
  add_reason(!is.finite(data$metaUI__se) | data$metaUI__se <= 0, "invalid SE")
  if (es_type == "COR") add_reason(is.finite(data$metaUI__effect_size) & abs(data$metaUI__effect_size) >= 1, "COR outside (-1, 1)")
  if (!is.null(count_cells)) add_reason(count_cells$invalid, "missing, negative, fractional or impossible counts")
  if (!is.null(count_cells) && double_zero == "exclude") add_reason(count_cells$double_zero, "no events (or only events) in both groups; excluded by double_zero = \"exclude\"")
  if (!(na.rm[1] %in% c("es_related", FALSE, TRUE))) stop('`na.rm` must be TRUE, FALSE or "es_related"')
  if (identical(na.rm, FALSE) && any(reason != "")) stop("Invalid required model inputs; use na.rm = 'es_related' to exclude with a report.")
  if (identical(na.rm, TRUE)) add_reason(missing_mapped, "missing filter, p-value or N")
  exclusions <- data.frame(row = original_rows[reason != ""], reason = reason[reason != ""])
  data <- data[reason == "", , drop = FALSE]
  if (!nrow(data)) stop("No eligible rows remain.")
  # Levels, including "(Missing)", that only excluded rows had.
  if (!keep_missing_level) for (field in grep("^metaUI__filter_", names(data), value = TRUE))
    if (is.factor(data[[field]])) data[[field]] <- droplevels(data[[field]])
  data$metaUI__input_effect <- data$metaUI__effect_size
  data$metaUI__input_variance <- data$metaUI__variance
  data$metaUI__display_scale <- c(SMD = "SMD", COR = "r", ZCOR = "r", OR = "OR", RR = "RR")[[es_type]]
  if (es_type == "COR") {
    # Delta-method propagation of supplied r variance; not the N-based 1/(N-3) rule.
    data$metaUI__variance <- data$metaUI__variance / (1 - data$metaUI__effect_size^2)^2
    data$metaUI__effect_size <- atanh(data$metaUI__effect_size)
    data$metaUI__se <- sqrt(data$metaUI__variance)
    data$metaUI__es_type <- "ZCOR"
    derived <- c(derived, "z = atanh(r); var(z) = var(r)/(1-r^2)^2 (delta method)")
  }
  data$metaUI__direction <- direction
  z <- as.numeric(scale(data$metaUI__effect_size))
  if (all(is.na(z))) z <- rep(0, nrow(data))
  data$metaUI__es_z <- z
  invalid_p <- !is.na(data$metaUI__pvalue) & (!is.finite(data$metaUI__pvalue) | data$metaUI__pvalue < 0 | data$metaUI__pvalue > 1)
  invalid_n <- !is.na(data$metaUI__N) & (!is.finite(data$metaUI__N) | data$metaUI__N <= 0)
  data$metaUI__pvalue[invalid_p] <- NA_real_
  data$metaUI__N[invalid_n] <- NA_real_
  attr(data, "metaUI_validation") <- list(
    # Effects computed from counts have no source column of their own; derivations record how.
    source = source, mapping = list(study = study_label, effect = if (is.null(counts)) es_field,
      variance = if (variance %in% c("metaUI_input_variance", "metaUI_input_vi")) NULL else variance,
      SE = if (se == "metaUI_input_se") NULL else se,
      effect_id = es_id, label = es_label, p = if (pvalue == "metaUI_input_p") NULL else pvalue,
      N = if (sample_size == "metaUI_input_N") NULL else sample_size, filters = filters, counts = counts,
      double_zero = if (!is.null(counts)) double_zero),
    input_rows = length(original_rows), retained_rows = nrow(data), exclusions = exclusions,
    input_scale = es_type, fitting_scale = data$metaUI__es_type[1],
    display_scale = data$metaUI__display_scale[1], direction = direction, derivations = derived,
    optional_inputs = list(missing_p = sum(is.na(data$metaUI__pvalue)), missing_N = sum(is.na(data$metaUI__N)),
                           invalid_p = sum(invalid_p), invalid_N = sum(invalid_n), N_differs_from_counts = count_N_mismatches))
  specs <- lapply(seq_along(alternatives), function(i) {
    spec <- alternatives[[i]]
    required <- c("es_field", "es_type", "justification")
    allowed <- c(required, "variance", "se", "variance_scale")
    if (!is.list(spec) || !all(required %in% names(spec)) ||
        length(setdiff(names(spec), allowed)) ||
        !any(c("variance", "se") %in% names(spec)) ||
        !is.character(spec$justification) || length(spec$justification) != 1L || is.na(spec$justification) || !nzchar(trimws(spec$justification)))
      stop("Alternative must declare effect, variance/SE, scale and justification: ", names(alternatives)[i])
    args <- alternative_base
    args$se <- NULL; args$variance <- NULL; args$variance_scale <- NULL; args$counts <- NULL; args$double_zero <- NULL
    for (key in setdiff(names(spec), "justification")) args[[key]] <- spec[[key]]
    kept <- raw_data[original_rows[reason == ""], , drop = FALSE]
    args$na.rm <- FALSE
    if (is.null(es_id)) {
      kept$metaUI_original_effect_id <- original_rows[reason == ""]
      args$es_id <- "metaUI_original_effect_id"
    }
    alt <- tryCatch(suppressMessages(do.call(prepare_data, c(list(data = kept), args))), error = function(e)
      stop("Alternative ", names(alternatives)[i], " is invalid for retained effects (original rows ",
        paste(original_rows[reason == ""], collapse = ", "), "): ", conditionMessage(e)))
    if (!identical(as.character(alt$metaUI__study_id), as.character(data$metaUI__study_id)) ||
        !identical(as.character(alt$metaUI__effect_id), as.character(data$metaUI__effect_id)))
      stop("Alternative computations must retain exactly the same effects: ", names(alternatives)[i])
    if (!identical(alt$metaUI__es_type[1], data$metaUI__es_type[1]))
      stop("Alternative computations must use the same fitting scale: ", names(alternatives)[i])
    prefix <- paste0("metaUI__alt_", i, "_")
    columns <- setNames(paste0(prefix, c("input_effect", "input_variance", "fit_effect", "fit_variance")),
      c("input_effect", "input_variance", "fit_effect", "fit_variance"))
    data[[columns[["input_effect"]]]] <<- alt$metaUI__input_effect
    data[[columns[["input_variance"]]]] <<- alt$metaUI__input_variance
    data[[columns[["fit_effect"]]]] <<- alt$metaUI__effect_size
    data[[columns[["fit_variance"]]]] <<- alt$metaUI__variance
    c(list(id = paste0("alt", i), name = names(alternatives)[i],
      justification = spec$justification, input_scale = spec$es_type,
      mapping = spec[setdiff(names(spec), "justification")]), as.list(columns))
  })
  if (length(specs)) {
    attr(data, "metaUI_validation")$primary_label <- primary_label
    attr(data, "metaUI_primary_label") <- primary_label
    attr(data, "metaUI_validation")$alternatives <- specs
    attr(data, "metaUI_alternatives") <- specs
    attr(data, "metaUI_input_scale") <- es_type
    metaUI_validate_alternatives(data, specs)
  }
  # Fingerprint after alternative columns are added, so they are covered too.
  attr(data, "metaUI_validation")$fingerprint <- metaUI_data_fingerprint(data)
  if (nrow(exclusions)) warning(nrow(exclusions), " rows excluded; see attr(data, 'metaUI_validation')$exclusions.")
  data
}

# Content hash of the prepared columns, independent of row names and attributes.
metaUI_data_fingerprint <- function(data) {
  columns <- lapply(as.list(data)[sort(names(data))], function(x) as.vector(x))
  path <- tempfile(); on.exit(unlink(path))
  writeBin(serialize(columns, NULL, version = 2), path)
  unname(tools::md5sum(path))
}

# Older tutorials read inst/extdata/barroso2021.csv. That file is now a stub
# whose single row explains the removal, so stop with that message.
check_removed_example <- function(data) {
  if (is.data.frame(data) && identical(names(data), c("metaUI_removed_dataset", "message")) &&
      nrow(data) == 1L && identical(as.character(data$metaUI_removed_dataset), "barroso2021"))
    stop(as.character(data$message[1]), call. = FALSE)
  invisible(data)
}


# Recode missing values (and an explicit NA level) to a named last level
na_to_level <- function(f, level) {
  if (anyNA(levels(f))) levels(f)[is.na(levels(f))] <- level
  if (!level %in% levels(f)) levels(f) <- c(levels(f), level)
  f[is.na(f)] <- level
  f
}

# Text exported from reference managers or web pages often keeps HTML escapes, which
# apps would show verbatim ("Smith &amp; Lee"). Decodes them in text and factor
# columns and reports the columns changed.
decode_text_entities <- function(data) {
  changed <- character()
  for (name in names(data)) {
    x <- data[[name]]
    if (!is.character(x) && !is.factor(x)) next
    values <- if (is.factor(x)) levels(x) else x
    decoded <- metaUI_decode_entities(values)
    if (identical(decoded, values)) next
    if (is.factor(x)) levels(x) <- decoded else x <- decoded
    data[[name]] <- x
    changed <- c(changed, name)
  }
  if (length(changed)) message("Decoded HTML escapes such as '&amp;' in: ", paste(changed, collapse = ", "), ".")
  data
}


# Filter labels become column names, Shiny IDs and model-formula terms, so they must be distinct
# after IDs are made safe, and free of backticks. A numeric filter needs a value to give its slider a range.
metaUI_check_filter_columns <- function(cols, values = NULL) {
  if (!length(cols)) return(invisible(TRUE))
  labels <- sub("^metaUI__filter_", "", cols)
  if (anyDuplicated(labels)) stop("Filter labels must be distinct; repeated: ", paste(unique(labels[duplicated(labels)]), collapse = ", "), ".")
  if (any(grepl("`", labels, fixed = TRUE))) stop("Filter labels cannot contain backticks: ", paste(labels[grepl("`", labels, fixed = TRUE)], collapse = ", "), ".")
  ids <- metaUI_filter_id(cols)
  clash <- ids %in% ids[duplicated(ids)]
  if (any(clash)) stop("These filter labels differ only in spaces or symbols, which the app cannot tell apart: ",
    paste0("\"", labels[clash], "\"", collapse = ", "), ". Rename one of them.")
  if (!is.null(values)) for (i in seq_along(cols)) {
    x <- values[[i]]
    if (is.numeric(x) && !any(is.finite(x))) stop("Numeric filter \"", labels[i], "\" has no values. Leave it out until it is coded.")
  }
  invisible(TRUE)
}

# Count columns for OR and RR: ai and ci with n1i and n2i (group totals) or bi and di (non-events).
metaUI_check_counts <- function(counts, data) {
  if (!is.character(counts) || is.null(names(counts)) || anyDuplicated(names(counts)) ||
      !(setequal(names(counts), c("ai", "ci", "n1i", "n2i")) || setequal(names(counts), c("ai", "bi", "ci", "di"))))
    stop("counts must name columns as c(ai = , n1i = , ci = , n2i = ) (events and totals per group) or c(ai = , bi = , ci = , di = ) (events and non-events).")
  if (any(!counts %in% names(data))) stop("Count column not found: ", paste(setdiff(counts, names(data)), collapse = ", "))
  for (col in counts) if (!is.numeric(data[[col]])) stop("Numeric input required: ", col)
  counts[intersect(c("ai", "bi", "n1i", "ci", "di", "n2i"), names(counts))]
}
