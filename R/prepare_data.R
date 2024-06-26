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
#' @param es_type Character. Type of effect size. Defaults to "SMD" (standardized mean difference). Check the [metafor::escalc()] function for other options.
#' @param es_label Character. Label for individual effect sizes - only needed when there are multiple effect sizes per study/sample. Defaults to NA. Defaults to NA, in that case, multiple effect sizes are simply numbered.
#' @param na.rm Should rows with any missing values be removed? Can be TRUE, FALSE or "es_related" -
#' the last is the default and drops rows with missing values for any of the variables used in the standard meta-analysis models, namely
#' `es_field`, `sample_size`, `variance`, `se` and `pvalue`. Setting this to TRUE also drops rows with missing values on any of the filters
#' etc, which might often be unnecessary. Conversely, setting this to FALSE might lead to issues in the model - unless you post-process the data
#' or change the models and analyses to be included in the app.
#' @param arrange_filters Character. How should the filters be arranged in the app? Options are "given" by the `filters` argument, "alphabetical" or "leave" (as they are in the dataset). Defaults to "given".
#' @param keep_missing_level Logical. Should a `(Missing)`-level be kept even for filters that do not have missing values? Might be advisable when you expect users to upload new data with missing values. Defaults to FALSE.
#'
#' @return tibble with the data from the file/input reformatted for metaUI
#' @export
#' @examples
#' \dontrun{
#' import_data("my_meta.csv", "study_id", "cohens_d", c("Country" = "country", "Year" = "year"))
#' }
prepare_data <- function(data, study_label, es_field, se, pvalue, sample_size, variance, filters,
                         url = NA, es_type = "SMD", article_label = NA, es_label = NA, na.rm = "es_related",
                         arrange_filters = c("given", "alphabetical", "leave"), keep_missing_level = FALSE) {
  if (is.character(data)) {
    # Read the file
    data <- read.csv(data, stringsAsFactors = FALSE)
  }

  if (es_type != "SMD") warning("metaUI is currently optimised for datasets that contain mean differences (e.g., Cohen's d). ",
                                "Other effect size types may work, but internal conversions (e.g., r-to-z) are not yet supported. ",
                                "If you need such an extension, please open an issue: https://github.com/LukasWallrich/metaUI/issues")

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
    dplyr::rename_with(~ paste0("metaUI__filter_", filter_names[.x]), .cols = dplyr::all_of(filters)) %>%
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
    data <- data %>%
      dplyr::rename("metaUI__es_label" = !!rlang::sym(es_label))
  } else {
    data <- data %>%
      dplyr::group_by(.data$metaUI__study_id) %>%
      dplyr::mutate(metaUI__es_label = dplyr::row_number()) %>%
      dplyr::ungroup()
  }
  data$metaUI__es_z <- c(scale(data$metaUI__effect_size))

  if (!(na.rm[1] %in% c("es_related", FALSE, TRUE))) stop('`na.rm` must be TRUE, FALSE or "es_related"')

  if (na.rm[1] == "es_related") {
    data <- data %>%
      tidyr::drop_na(.data$metaUI__effect_size, .data$metaUI__se, .data$metaUI__variance, .data$metaUI__pvalue, .data$metaUI__N)
  } else if (na.rm[1] == TRUE) {
    data <- data %>% tidyr::drop_na()
  }

  # Return the data frame
  return(data)
}
