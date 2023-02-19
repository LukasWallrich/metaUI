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
#' @param article_label Character. Field with article labels. Only used to report number of references in addition to number of independent samples. Defaults to NA.
#' @param es_type Character. Type of effect size. Defaults to "SMD" (standardized mean difference). Check the [metafor::escalc()] function for other options.
#' @param es_label Character. Label for individual effect sizes - only needed when there are multiple effect sizes per study. Defaults to NA.
#'
#' @return tibble with the data from the file reformatted for metaUI
#' @export
#' @examples
#' \dontrun{
#' import_data("my_meta.csv", "study_id", "cohens_d", c("Country" = "country", "Year" = "year"))
#' }
import_data <- function(data, study_label, es_field, se, pvalue, sample_size, variance, filters, url = NA, es_type = "SMD", article_label = NA, es_label = NA) {
  if (is.character(data)) {
    # Read the file
    data <- read.csv(data, stringsAsFactors = FALSE)
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
      dplyr::mutate(metaUI__url = ifelse(grepl("^10\\.", metaUI__url), paste0("https://doi.org/", metaUI__url), metaUI__url))
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
      dplyr::group_by(metaUI__study_id) %>%
      dplyr::mutate(metaUI__es_label = dplyr::row_number()) %>%
      dplyr::ungroup()
  }
  data$metaUI__es_z <- c(scale(data$metaUI__effect_size))

  # Return the data frame
  return(data)
}
