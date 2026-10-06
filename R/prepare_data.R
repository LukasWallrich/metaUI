#' Import data from a file and return a data frame
#'
#' You may want to use the [metafor::escalc()] function in the metafor package to calculate effect sizes and their variances in preparation for this function.
#'
#' @param data path to the .csv file to read OR a data frame
#' @param study_label Character. Name of the field to use as the id/study label
#' @param es_field Character. Name of the field to use as the effect size
#' @param sample_size Character. Name of the field to use as the sample size
#' @param variance Character. Name of the field to use as the sampling variances.
#' @param se Character. Name of the field to use as the standard error for the effect size.
#' @param pvalue Character. Name of the field to use as the p-value for the effect size.
#' @param filters Character. List of fields to use as filters - can be named if different labels should be displayed
#' @param url Character. Field with URLs or DOIs to link to. DOIs can be in the format "10.1234/5678" or full links. Defaults to NA.
#' @param article_label Character. Field with article labels. Only used to report number of references in addition to number of independent samples.
#' @param es_type Character. Input scale: SMD, COR (raw r), or ZCOR (Fisher z). COR needs r-scale variance and uses delta-method variance conversion; ZCOR needs z-scale variance.
#' @param es_label Character. Label for individual effect sizes - only needed when there are multiple effect sizes per study/sample. Defaults to NA. Defaults to NA, in that case, multiple effect sizes are simply numbered.
#' @param na.rm Should rows with any missing values be removed? Can be TRUE, FALSE or "es_related" -
#' the last is the default and drops rows with missing values for any of the variables used in the standard meta-analysis models, namely
#' `study_label`, `es_field`, `variance`, and `se`. Optional p/N never remove rows under this default. Setting this to TRUE also drops rows with missing values on any of the filters
#' etc, which might often be unnecessary. Conversely, setting this to FALSE might lead to issues in the model - unless you post-process the data
#' or change the models and analyses to be included in the app. Invalid required inputs with FALSE cause an error.
#' @param arrange_filters Character. How should the filters be arranged in the app? Options are "given" by the `filters` argument, "alphabetical" or "leave" (as they are in the dataset). Defaults to "given".
#' @param keep_missing_level Logical. Should a `(Missing)`-level be kept even for filters that do not have missing values? Might be advisable when you expect users to upload new data with missing values. Defaults to FALSE.
#'
#' @param categorical_filters Explicitly convert named filter columns to factors with radix-sorted levels. Useful for CSV/JSON builds.
#' @param es_id Optional column with unique effect IDs within each study. Defaults to input row IDs.
#' @param variance_scale Required for COR (r) and ZCOR (z); optional SMD for SMD.
#' @param direction Explicit direction for one-sided bias models: unspecified, positive, or negative.
#' @param primary_label Label for the primary computation when alternatives are provided.
#' @param alternatives Named list of up to five author-defined effect computations. Each entry supplies es_field, variance or se, es_type, variance_scale when required, and a nonempty justification. Every computation must retain the same effects and converge on the same fitting scale.
#' @return tibble with the data from the file/input reformatted for metaUI
#' @export
#' @examples
#' raw <- data.frame(study = letters[1:4], d = c(.1, .3, -.1, .4), vi = rep(.02, 4))
#' prepare_data(raw, "study", "d", variance = "vi")
prepare_data <- function(data, study_label, es_field, se = NULL, pvalue = NULL, sample_size = NULL, variance = NULL, filters = character(),
                         url = NA, es_type = "SMD", article_label = NA, es_label = NA, na.rm = "es_related",
                         arrange_filters = c("given", "alphabetical", "leave"), keep_missing_level = FALSE, es_id = NULL,
                         variance_scale = NULL, direction = c("unspecified", "positive", "negative"),
                         categorical_filters = character(), alternatives = list(), primary_label = "As supplied") {
  source <- list(kind = if (is.character(data)) "CSV" else "data.frame",
                 file = if (is.character(data)) basename(data) else NULL,
                 md5 = if (is.character(data)) unname(tools::md5sum(data)) else NULL)
  if (is.character(data)) {
    # Read the file
    data <- read.csv(data, stringsAsFactors = FALSE)
  }

  raw_data <- data
  alternative_base <- list(study_label = study_label, es_field = es_field, se = se,
    pvalue = pvalue, sample_size = sample_size, variance = variance, filters = filters,
    url = url, es_type = es_type, article_label = article_label, es_label = es_label,
    na.rm = na.rm, arrange_filters = arrange_filters, keep_missing_level = keep_missing_level,
    es_id = es_id, variance_scale = variance_scale, direction = direction,
    categorical_filters = categorical_filters)
  if (!is.list(alternatives) || length(alternatives) > 5L ||
      (length(alternatives) && (is.null(names(alternatives)) || any(!nzchar(names(alternatives))) ||
        anyDuplicated(names(alternatives)) || "As supplied" %in% names(alternatives))))
    stop("alternatives must be a uniquely named list of at most five computations.")

  if (!is.character(primary_label) || length(primary_label) != 1L || !nzchar(trimws(primary_label)) || primary_label %in% names(alternatives)) stop("primary_label must be a unique nonempty string.")
  direction <- match.arg(direction)
  if (!es_type %in% c("SMD", "COR", "ZCOR"))
    stop("Supported metrics are SMD, COR and ZCOR; other metrics require custom analysis.")
  if (es_type == "COR" && !identical(variance_scale, "r"))
    stop("COR requires variance_scale = 'r' with supplied r-scale variance. No N-based variance is inferred.")
  if (es_type == "ZCOR" && !identical(variance_scale, "z"))
    stop("ZCOR requires variance_scale = 'z'.")
  if (es_type == "SMD" && !is.null(variance_scale) && variance_scale != "SMD")
    stop("SMD requires SMD-scale variance.")
  if (is.null(variance) && is.null(se)) stop("Supply variance or se on the declared input scale.")
  mappings <- c(study_label, es_field, se, pvalue, sample_size, variance, filters, es_id)
  if (any(!mappings %in% names(data))) stop("Mapped column not found: ", paste(setdiff(mappings, names(data)), collapse = ", "))
  if (anyDuplicated(mappings)) stop("Column mappings must be distinct.")
  original_rows <- seq_len(nrow(data))
  effect_ids_input <- if (is.null(es_id)) original_rows else data[[es_id]]
  labels_input <- NULL
  if (!is.na(es_label)) {
    if (!es_label %in% names(data)) stop("Mapped label column not found: ", es_label)
    labels_input <- data[[es_label]]
  }
  derived <- character()
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
  unmapped_optional <- c(if (is.null(pvalue)) "metaUI__pvalue", if (is.null(sample_size)) "metaUI__N")
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
      stop("All filters/moderators must be factors or numeric. This check failed first for ", filters[i])
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

  # Set NA level for categorical filters ...
  for (i in seq_along(filters)) {
    if(is.factor(data[[filters[i]]])) {
      data[[filters[i]]] <- data[[filters[i]]] %>%
       forcats::fct_na_value_to_level("(Missing)")
      # ... if there are any NA
      if (!keep_missing_level) {
        data[[filters[i]]] <- data[[filters[i]]] %>%
          forcats::fct_drop()
      }
    }
  }

  # Arrange the filters
  arrange_filters <- match.arg(arrange_filters)
  if (!arrange_filters == "leave") {
    if (arrange_filters == "alphabetical") {
      filters <- sort(filters)
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
  if (!(na.rm[1] %in% c("es_related", FALSE, TRUE))) stop('`na.rm` must be TRUE, FALSE or "es_related"')
  if (identical(na.rm, FALSE) && any(reason != "")) stop("Invalid required model inputs; use na.rm = 'es_related' to exclude with a report.")
  if (identical(na.rm, TRUE)) add_reason(!stats::complete.cases(data[setdiff(names(data), unmapped_optional)]), "missing mapped or other input")
  exclusions <- data.frame(row = original_rows[reason != ""], reason = reason[reason != ""])
  data <- data[reason == "", , drop = FALSE]
  if (!nrow(data)) stop("No eligible rows remain.")
  data$metaUI__input_effect <- data$metaUI__effect_size
  data$metaUI__input_variance <- data$metaUI__variance
  data$metaUI__display_scale <- if (es_type %in% c("COR", "ZCOR")) "r" else "SMD"
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
    source = source, mapping = list(study = study_label, effect = es_field, variance = variance, SE = se,
      effect_id = es_id, label = es_label, p = if (pvalue == "metaUI_input_p") NULL else pvalue,
      N = if (sample_size == "metaUI_input_N") NULL else sample_size, filters = filters),
    input_rows = length(original_rows), retained_rows = nrow(data), exclusions = exclusions,
    input_scale = es_type, fitting_scale = data$metaUI__es_type[1],
    display_scale = data$metaUI__display_scale[1], direction = direction, derivations = derived,
    optional_inputs = list(missing_p = sum(is.na(data$metaUI__pvalue)), missing_N = sum(is.na(data$metaUI__N)),
                           invalid_p = sum(invalid_p), invalid_N = sum(invalid_n)))
  specs <- lapply(seq_along(alternatives), function(i) {
    spec <- alternatives[[i]]
    required <- c("es_field", "es_type", "justification")
    allowed <- c(required, "variance", "se", "variance_scale")
    if (!is.list(spec) || !all(required %in% names(spec)) ||
        length(setdiff(names(spec), allowed)) ||
        !any(c("variance", "se") %in% names(spec)) ||
        !is.character(spec$justification) || length(spec$justification) != 1L || !nzchar(trimws(spec$justification)))
      stop("Alternative must declare effect, variance/SE, scale and justification: ", names(alternatives)[i])
    args <- alternative_base
    args$se <- NULL; args$variance <- NULL; args$variance_scale <- NULL
    for (key in setdiff(names(spec), "justification")) args[[key]] <- spec[[key]]
    kept <- raw_data[original_rows[reason == ""], , drop = FALSE]
    args$na.rm <- FALSE
    if (is.null(es_id)) {
      kept$metaUI_original_effect_id <- original_rows[reason == ""]
      args$es_id <- "metaUI_original_effect_id"
    }
    alt <- tryCatch(do.call(prepare_data, c(list(data = kept), args)), error = function(e)
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
  if (nrow(exclusions)) warning(nrow(exclusions), " rows excluded; see attr(data, 'metaUI_validation')$exclusions.")
  data
}
