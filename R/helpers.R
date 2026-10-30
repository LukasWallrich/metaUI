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

# APA-style author list from "Family, Given; ..." (fetch_study_metadata()'s format):
# initials for given names, "&" before the last author, and 19 + ". . ." + last beyond 20.
# Names in another format are kept as written.
metaUI_apa_authors <- function(authors) {
  names <- strsplit(authors, "; ", fixed = TRUE)[[1]]
  names <- vapply(names, function(n) {
    parts <- strsplit(n, ", ", fixed = TRUE)[[1]]
    if (length(parts) != 2L) return(n)
    initials <- vapply(strsplit(parts[2], "\\s+")[[1]], function(g) if (grepl(".", g, fixed = TRUE)) g else
      paste0(substr(strsplit(g, "-", fixed = TRUE)[[1]], 1, 1), ".", collapse = "-"), character(1))
    paste0(parts[1], ", ", paste(initials, collapse = " "))
  }, character(1), USE.NAMES = FALSE)
  n <- length(names)
  if (n > 20) return(paste0(paste(names[1:19], collapse = ", "), ", . . . ", names[n]))
  if (n == 1) names else paste0(paste(names[-n], collapse = ", "), ", & ", names[n])
}

# Family name of the first author ("Family, Given" or an organisation as written).
metaUI_first_family <- function(authors) {
  names <- strsplit(authors, "; ", fixed = TRUE)[[1]]
  family <- sub(",.*$", "", names)
  if (length(names) == 1) family else if (length(names) == 2) paste(family, collapse = " & ") else paste(family[1], "et al.")
}

metaUI_copy_icon <- '<svg viewBox="0 0 16 16" width="14" height="14" aria-hidden="true" focusable="false"><rect x="5.5" y="5.5" width="8.5" height="8.5" rx="1.5" fill="none" stroke="currentColor" stroke-width="1.5"/><path d="M10.5 5.5V3.5A1.5 1.5 0 0 0 9 2H3.5A1.5 1.5 0 0 0 2 3.5V9a1.5 1.5 0 0 0 1.5 1.5h2" fill="none" stroke="currentColor" stroke-width="1.5"/></svg>'

# Reader-facing study details from the authors' study information (see fetch_study_metadata()).
# Returns HTML strings, one per row of `info`; every value is escaped.
metaUI_study_details <- function(info) {
  vapply(seq_len(nrow(info)), function(i) {
    get <- function(name) {
      value <- if (name %in% names(info)) info[[name]][i] else NA
      if (is.na(value) || !nzchar(value)) NULL else as.character(value)
    }
    link <- if (!is.null(get("doi"))) paste0("https://doi.org/", get("doi")) else get("url")
    # APA 7: Authors (year). Title. Journal, volume(issue), pages. DOI link -- punctuation controlled here.
    esc <- function(x) htmltools::htmlEscape(x)
    stop_ <- function(x) if (grepl("[.?!]$", x)) x else paste0(x, ".")
    pages <- if (!is.null(get("pages"))) gsub("([0-9])-([0-9])", "\\1\u2013\\2", get("pages"))
    venue <- paste0(if (!is.null(get("volume"))) paste0("<i>", esc(get("volume")), "</i>"),
      if (!is.null(get("issue"))) paste0("(", esc(get("issue")), ")"))
    reference <- paste0(
      if (!is.null(get("authors"))) paste0(esc(metaUI_apa_authors(get("authors"))), " "),
      if (!is.null(get("year"))) paste0("(", esc(get("year")), "). "),
      if (!is.null(get("title"))) paste0("<span class=\"metaui-study-title\">", esc(stop_(get("title"))), "</span> "),
      if (!is.null(get("journal"))) paste0("<i>", esc(get("journal")), "</i>",
        if (length(venue) && nzchar(venue)) paste0(", ", venue), if (!is.null(pages)) paste0(", ", esc(pages)), ". "),
      if (!is.null(link)) as.character(htmltools::tags$a(href = link, target = "_blank", rel = "noopener", link)))
    reference <- paste0("<p class=\"metaui-study-ref\"><span class=\"metaui-study-cite\">", reference, "</span> ",
      "<button type=\"button\" class=\"metaui-copy-ref\" aria-label=\"Copy reference\" title=\"Copy reference\">",
      metaUI_copy_icon, "</button><span class=\"metaui-copy-status\" role=\"status\"></span></p>")
    abstract <- get("abstract")
    licence <- get("abstract_licence")
    # Crossref gives a licence URL; OpenAlex a code such as "cc-by".
    licence_label <- if (is.null(licence)) NULL else if (grepl("creativecommons\\.org/licenses/", licence))
      toupper(gsub("^.*/licenses/([a-z-]+)/([0-9.]+).*$", "CC \\1 \\2", licence))
      else if (grepl("publicdomain/zero", licence) || licence == "cc0") "CC0"
      else if (grepl("publicdomain/mark", licence) || licence == "public-domain") "Public Domain"
      else if (grepl("^cc-", licence)) toupper(sub("^cc-", "CC ", licence)) else licence
    licence_html <- if (is.null(licence_label)) NULL else if (grepl("^https?://", licence))
      as.character(htmltools::tags$a(href = licence, target = "_blank", rel = "noopener", licence_label))
      else paste0(esc(licence_label), " (as recorded by OpenAlex)")
    sources <- unique(c(get("abstract_source"), get("metadata_source")))
    source <- if (length(sources) == 2) paste0("abstract from ", sources[1], ", reference from ", sources[2]) else sources
    source <- paste(c(source, if (!is.null(get("retrieved"))) paste("retrieved", get("retrieved"))), collapse = ", ")
    body <- if (!is.null(abstract)) paste0(
      "<h4 class=\"metaui-study-heading\">Abstract</h4>",
      paste0("<p>", esc(strsplit(abstract, "\n\n", fixed = TRUE)[[1]]), "</p>", collapse = ""),
      "<p class=\"metaui-note\">",
      if (!is.null(licence_html)) paste0("Licence: ", licence_html, ". ")
      else paste0(esc(if (is.null(get("abstract_note"))) "Included by the app authors." else get("abstract_note")), " "),
      if (nzchar(source)) paste0("Source: ", esc(source), "; formatting removed."), "</p>")
    else paste0("<p class=\"metaui-note\">", esc(paste(c(get("abstract_note"), if (!is.null(get("metadata_source")))
      paste0("Reference from ", get("metadata_source"), if (!is.null(get("retrieved"))) paste0(", retrieved ", get("retrieved")), ".")), collapse = " ")), "</p>")
    paste0("<div class=\"metaui-study-info\">", reference, body, "</div>")
  }, character(1))
}

# Plain one-line reference for hover titles: "Family et al. (year). Title. Journal."
metaUI_study_reference <- function(info) {
  vapply(seq_len(nrow(info)), function(i) {
    parts <- vapply(c("authors", "year", "title", "journal"), function(name) {
      value <- if (name %in% names(info)) info[[name]][i] else NA
      if (is.na(value)) "" else as.character(value)
    }, character(1))
    if (nzchar(parts[["authors"]])) parts[["authors"]] <- metaUI_first_family(parts[["authors"]])
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
