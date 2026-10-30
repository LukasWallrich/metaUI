# Ensure this is aligned in /R and /inst as it is used by both this package and the Shiny apps. Locally ensured with git pre-commit hook

# Slider bounds rounded outward to a round tick interval, so the default range keeps
# every value. Shiny derives a fractional tick count from (max - min) / step, which
# makes the last grid labels collide; `ticks` is the exact number of grid intervals.
metaUI_slider_spec <- function(x, integer = NULL) {
  x <- x[is.finite(x)]
  if (!length(x)) stop("A slider needs at least one finite value.")
  if (is.null(integer)) integer <- all(x == round(x))
  lo <- min(x); hi <- max(x)
  if (lo == hi) {
    pad <- if (integer) 1 else max(abs(lo) * .1, .1)
    lo <- lo - pad; hi <- hi + pad
  }
  magnitude <- 10^floor(log10(hi - lo))
  candidates <- sort(unique(as.vector(c(1, 2, 2.5, 5) %o% (magnitude * 10^(-2:1)))))
  if (integer) candidates <- candidates[candidates >= 1 & candidates == round(candidates)]
  for (tick in candidates) {
    digits <- max(0, 1 - floor(log10(tick)))
    min_value <- round(floor(lo / tick) * tick, digits)
    max_value <- round(ceiling(hi / tick) * tick, digits)
    if (min_value > lo) min_value <- round(min_value - tick, digits)
    if (max_value < hi) max_value <- round(max_value + tick, digits)
    ticks <- round((max_value - min_value) / tick)
    # Longer labels need fewer grid intervals to stay legible in a narrow sidebar.
    labels <- format(round(min_value + tick * 0:ticks, digits), scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
    chars <- max(nchar(labels))
    if (ticks <= if (chars <= 3) 10 else if (chars == 4) 8 else if (chars == 5) 6 else 4) break
  }
  # Fine steps (at least ~50 positions over the data) that divide the tick interval.
  step <- if (integer) 1 else {
    steps <- round(tick / c(10, 20, 50, 100), digits + 2)
    steps[c(steps <= (hi - lo) / 50, TRUE)][1]
  }
  list(min = min_value, max = max_value, step = step, ticks = ticks)
}

# sliderInput() with the bounds, step and integer grid from metaUI_slider_spec()
metaUI_slider_input <- function(inputId, label, spec, value = c(spec$min, spec$max)) {
  slider <- shiny::sliderInput(inputId, label, min = spec$min, max = spec$max,
    value = value, step = spec$step, sep = "")
  htmltools::tagQuery(slider)$find("input")$removeAttrs("data-grid-num")$
    addAttrs(`data-grid-num` = spec$ticks)$allTags()
}

#' Format p-value in line with APA standard (no leading 0)
#'
#' Formats p-value in line with APA standard, returning it without leading 0 and
#' as < .001 and > .99 when it is extremely small or large.
#'
#' @param p_value Numeric, or a vector of numbers
#' @param digits Number of significant digits, defaults to 3
#' @param include_equal Should precise p-values by prefixed with =? Useful for in-text reporting, less so in tables
#' @source Taken from timesaveR package, copyright also Lukas Wallrich (2023)
#' @noRd

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

# Count categories in x, return top 10 and return everything else as Other (merging with existing Other if it exists)
summarise_categorical <- function(x, name) {
  if (length(x) == 0) {
    return(NULL)
  }

  counts <- tibble::tibble(!!rlang::sym(name) := x) %>%
    dplyr::count(!!rlang::sym(name), sort = TRUE, .drop = FALSE) %>%
    dplyr::rename(Count = n)

  if (nrow(counts) > 10) {

  top_categories <- counts %>%
    dplyr::filter(!!rlang::sym(name) != "Other") %>%
    dplyr::slice_max(order_by = Count, n = 9, with_ties = FALSE)

  other_count <- sum(counts$Count) - sum(top_categories$Count)

  if ("Other" %in% counts[[name]]) {
    other_count <- other_count + counts %>% dplyr::filter(!!rlang::sym(name) == "Other") %>% dplyr::pull(Count)
  }

  final_counts <- if (other_count > 0) {
    dplyr::bind_rows(top_categories, tibble::tibble(!!rlang::sym(name) := "Other", Count = other_count))
  } else {
    top_categories
  }

  } else {
    final_counts <- counts
  }

  final_counts %>%
    dplyr::mutate(Percentage = paste0(round(Count / sum(Count) * 100, 1), "%"))
}


#' Format confidence interval based on the bounds
#'
#' Constructs a confidence intervals from upper and lower bounds,
#' placing them in between square brackets
#'
#' @param lower Lower bound(s) of confidence interval(s). Numeric, or a vector of numbers
#' @param upper Lower bound(s) of confidence interval(s). Numeric, or a vector of numbers
#' @param digits Number of significant digits, defaults to 2
#' @source Taken from timesaveR package, copyright also Lukas Wallrich (2023)
#' @noRd

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
#' @param x Numeric vector to be rounded
#' @param digits Number of significant digits
#' @return Character vector of rounded values, with trailing zeroes as needed to show `digits` figures after the decimal point
#' @source Taken from timesaveR package, copyright also Lukas Wallrich (2023)
#' @noRd

round_ <- function(x, digits = 2) {
  fmt <- paste0("%.", digits, "f")
  out <- sprintf(fmt, x)
  attributes(out) <- attributes(x)
  out[is.na(x)] <- NA
  out
}

escape_quotes <- function(input_str) {
  output_str <- gsub("'", "&apos;", input_str)
  output_str <- gsub("\"", "&quot;", output_str)
  return(output_str)
}

# Reader-facing study details from the authors' study information (see fetch_study_metadata()).
# Returns HTML strings, one per row of `info`, built with htmltools so every value is escaped.
metaUI_study_details <- function(info) {
  vapply(seq_len(nrow(info)), function(i) {
    get <- function(name) {
      value <- if (name %in% names(info)) info[[name]][i] else NA
      if (is.na(value) || !nzchar(value)) NULL else as.character(value)
    }
    link <- if (!is.null(get("doi"))) paste0("https://doi.org/", get("doi")) else get("url")
    authors <- get("authors")
    if (!is.null(authors)) {
      names <- strsplit(authors, "; ", fixed = TRUE)[[1]]
      if (length(names) > 10) authors <- paste0(paste(names[1:10], collapse = "; "), "; and ", length(names) - 10, " more")
    }
    # Authors (year). Title. Journal, volume(issue), pages. DOI link -- values escaped, punctuation controlled here.
    esc <- function(x) htmltools::htmlEscape(x)
    stop_ <- function(x) if (grepl("[.?!]$", x)) x else paste0(x, ".")
    venue <- paste0(get("volume"), if (!is.null(get("issue"))) paste0("(", get("issue"), ")"))
    reference <- paste0(
      if (!is.null(authors)) paste0(esc(authors), " "),
      if (!is.null(get("year"))) paste0("(", esc(get("year")), "). "),
      if (!is.null(get("title"))) paste0("<span class=\"metaui-study-title\">", esc(stop_(get("title"))), "</span> "),
      if (!is.null(get("journal"))) paste0("<i>", esc(get("journal")), "</i>",
        if (nzchar(venue)) paste0(", ", esc(venue)), if (!is.null(get("pages"))) paste0(", ", esc(get("pages"))), ". "),
      if (!is.null(link)) as.character(htmltools::tags$a(href = link, target = "_blank", rel = "noopener", link)))
    reference <- htmltools::tags$p(class = "metaui-study-ref", htmltools::HTML(reference))
    abstract <- get("abstract")
    licence <- get("abstract_licence")
    licence_label <- if (is.null(licence)) NULL else if (grepl("creativecommons\\.org/licenses/", licence))
      toupper(gsub("^.*/licenses/([a-z-]+)/([0-9.]+).*$", "CC \\1 \\2", licence))
      else if (grepl("publicdomain/zero", licence)) "CC0" else if (grepl("publicdomain/mark", licence)) "Public Domain Mark" else licence
    source <- paste(c(get("metadata_source"), if (!is.null(get("retrieved"))) paste("retrieved", get("retrieved"))), collapse = ", ")
    source <- if (nzchar(source)) paste0("Source: ", esc(source), "; formatting removed.") else ""
    body <- if (!is.null(abstract)) paste0(
      "<h4 class=\"metaui-study-heading\">Abstract</h4>",
      paste0("<p>", esc(strsplit(abstract, "\n\n", fixed = TRUE)[[1]]), "</p>", collapse = ""),
      "<p class=\"metaui-note\">",
      if (!is.null(licence_label)) paste0("Licence: ", as.character(htmltools::tags$a(href = licence, target = "_blank", rel = "noopener", licence_label)), ". ")
      else paste0(esc(if (is.null(get("abstract_note"))) "Included by the app authors." else get("abstract_note")), " "),
      source, "</p>")
    else paste0("<p class=\"metaui-note\">", esc(paste(c(get("abstract_note"), if (!is.null(get("metadata_source")))
      paste0("Reference from ", get("metadata_source"), if (!is.null(get("retrieved"))) paste0(", retrieved ", get("retrieved")), ".")), collapse = " ")), "</p>")
    body <- htmltools::HTML(body)
    as.character(htmltools::tags$div(class = "metaui-study-info", reference, body))
  }, character(1))
}

# Plain one-line reference for hover titles.
metaUI_study_reference <- function(info) {
  vapply(seq_len(nrow(info)), function(i) {
    parts <- vapply(c("authors", "year", "title", "journal"), function(name) {
      value <- if (name %in% names(info)) info[[name]][i] else NA
      if (is.na(value)) "" else as.character(value)
    }, character(1))
    if (nzchar(parts[["authors"]])) parts[["authors"]] <- sub("^([^;]+);[^;]+;.*$", "\\1 et al.", parts[["authors"]])
    parts[["authors"]] <- trimws(paste0(parts[["authors"]], if (nzchar(parts[["year"]])) paste0(" (", parts[["year"]], ")")))
    parts <- parts[c("authors", "title", "journal")]
    parts <- parts[nzchar(parts)]
    paste(ifelse(grepl("[.?!]$", parts), parts, paste0(parts, ".")), collapse = " ")
  }, character(1))
}

# Effect-list cells: escaped study labels, as disclosure buttons where study details exist,
# and the details HTML for the hidden column that metaui.js shows as a child row.
metaUI_study_cells <- function(labels, ids, info, details_html, reference_text) {
  index <- match(as.character(ids), info$study)
  label <- vapply(seq_along(labels), function(i) if (is.na(index[i])) htmltools::htmlEscape(labels[i]) else
    as.character(htmltools::tags$button(type = "button", class = "metaui-study-toggle", `aria-expanded` = "false",
      title = reference_text[index[i]], labels[i])), character(1))
  list(label = label, details = ifelse(is.na(index), "", details_html[index]))
}
