# Function to round numbers up and down to a given number of significant digits
signif_ceiling <- function(x, digits = 2) {
  if (x == 0) {
    return(0)
  }
  else {
    return(signif(ceiling(x * 10^digits) / 10^digits, digits))
  }
}

signif_floor <- function(x, digits = 2) {
  if (x == 0) {
    return(0)
  }
  else {
    return(signif(floor(x * 10^digits) / 10^digits, digits))
  }
}

#' Format p-value in line with APA standard (no leading 0)
#'
#' Formats p-value in line with APA standard, returning it without leading 0 and
#' as < .001 and > .99 when it is extremely small or large.
#'
#' Taken from timesaveR package, copyright also Lukas Wallrich, 2022
#'
#' @param p_value Numeric, or a vector of numbers
#' @param digits Number of significant digits, defaults to 3
#' @param include_equal Should precise p-values by prefixed with =? Useful for in-text reporting, less so in tables

fmt_p <- function(p_value, digits = 3, include_equal = TRUE) {
  fmt <- paste0("%.", digits, "f")
  fmt_p <- function(x, include_equal) {
    paste0(ifelse(include_equal, "= ", ""), sprintf(fmt, x)) %>%
      stringr::str_replace(" 0.", " .")
  }
  exact <- !(p_value < 10^(-digits) | p_value > .99)
  exact[is.na(exact)] <- TRUE
  out <- p_value
  out[exact] <- purrr::map_chr(out[exact], fmt_p, include_equal)
  out[!exact] <- paste("<", sprintf(paste0("%.", digits, "f"), 10^(-digits)) %>%
                         stringr::str_replace("0.", "."))
  large <- p_value > .99
  out[large] <- "> .99"
  out[p_value > 1] <- "> 1 (!!)"
  out[is.na(p_value)] <- NA
  attributes(out) <- attributes(p_value)
  out
}


summarise_numeric <- function(x, name) {
  if (length(x) == 0) {
    return(NULL)
  }
  tibble::tribble(
    ~Var, ~`Mean (SD)`, ~`Min`, ~`Max`, ~`Share missing`,
    name, paste0(round(mean(x, na.rm = TRUE), 2), " (", round(sd(x, na.rm = TRUE), 2), ")"),
    round(min(x, na.rm = TRUE), 2), round(max(x, na.rm = TRUE), 2), paste0(round(mean(is.na(x))*100, 2), " %")
  )  %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))  %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "Statistic", values_to = "Value")
}

summarise_categorical <- function(x, name) {
  if (length(x) == 0) {
    return(NULL)
  }
  counts <- tibble::tibble(x = x)  %>% dplyr::count(x, sort = TRUE, name = "Count", .drop = FALSE)

  out <- dplyr::bind_rows(
    counts  %>% dplyr::slice(1:10),
    tibble::tibble(x = "Other", Count = sum(counts$Count) - sum(counts$Count[1:10])) %>% tidyr::drop_na())  %>%
    dplyr::mutate(Percentage = paste0(round(.data$Count / sum(.data$Count) * 100, 1), "%"))

  names(out)[1] <- name

  out
}

#' Format confidence interval based on the bounds
#'
#' Constructs a confidence intervals from upper and lower bounds,
#' placing them in between square brackets
#'
#' Taken from timesaveR package, copyright also Lukas Wallrich, 2022
#'
#' @param lower Lower bound(s) of confidence interval(s). Numeric, or a vector of numbers
#' @param upper Lower bound(s) of confidence interval(s). Numeric, or a vector of numbers
#' @param digits Number of significant digits, defaults to 2

fmt_ci <- function(lower, upper, digits = 2) {
  if (!(length(lower) == length(upper))) stop("lower and upper must have the same length.")
  out <- paste0("[", round_(lower, digits), ", ", round_(upper, digits), "]")
  out[is.na(lower) | is.na(upper)] <- NA
  out
}

#' Round function that returns trailing zeroes
#'
#' Particularly when creating tables, it is often desirable to keep
#' all numbers to the same width. `round()` and similar functions drop
#' trailing zeros - this version keeps them and thus rounds 1.201 to 1.20
#' rather than 1.2 when 2 digits are requested.
#'
#' Taken from timesaveR package, copyright also Lukas Wallrich, 2022
#'
#' @param x Numeric vector to be rounded
#' @param digits Number of significant digits
#' @return Character vector of rounded values, with trailing zeroes as needed to show `digits` figures after the decimal point

round_ <- function(x, digits = 2) {
  fmt <- paste0("%.", digits, "f")
  out <- sprintf(fmt, x)
  attributes(out) <- attributes(x)
  out[is.na(x)] <- NA
  out
}

# Thanks to https://stackoverflow.com/a/41194093/10581449
allglobal <- function() {
  if (identical(parent.frame(), globalenv())) return(FALSE)
  lss <- ls(envir = parent.frame())
  my_assign <- function(name, value, envir = 1L) assign(name, value, pos = envir)
  for (i in lss) {
    my_assign(i, get(i, envir = parent.frame()))
  }
}

escape_quotes <- function(input_str) {
  output_str <- gsub("'", "&apos;", input_str)
  output_str <- gsub("\"", "&quot;", output_str)
  return(output_str)
}
