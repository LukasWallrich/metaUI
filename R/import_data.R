
#' Import data from a file and return a data frame
#'
#' @param data path to the .csv file to read OR a data frame
#' @param study_label Character. Name of the field to use as the id/study label
#' @param es_field Character. Name of the field to use as the effect size
#' @param sample_size Character. Name of the field to use as the sample size
#' @param variance Character. Name of the field to use as the error variance (can be NA)
#' @param filters Character. List of fields to use as filters
#'
#' @return tibble with the data from the file reformatted for metaUI
#'
#' @examples
#' \dontrun{
#' import_data("my_meta.csv", "study_id", "cohens_d", c("country", "year"))
#' }

import_data <- function(file, study_label, es_field, sample_size, variance, filters) {

    if (is.character(df)) {
    # Read the file
    df <- read.csv(file, stringsAsFactors = FALSE)
    }
    # Rename the fields
    df <- df %>%
        dplyr::rename("metaUI__study_id" = !!study_label, "metaUI__effect_size" = !!es_field,
            "metaUI__N" = !!sample_size, "metaUI__variance" = !!variance) %>%
        dplyr::rename_with(~ paste0("metaUI__filter_", .x), .cols = dplyr::all_of(filters))

    # Return the data frame
    return(df)
}