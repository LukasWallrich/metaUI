#' Look up study metadata from DOIs at build time
#'
#' Queries the free Crossref REST API once per distinct DOI and returns one row
#' per study for `generate_shiny(study_info = ...)`. Run this while authoring:
#' generated apps read the stored result and never contact Crossref. DOIs that are
#' missing, malformed, unknown to Crossref or unreachable are reported in the
#' `status` column and a message; nothing is guessed or matched by title.
#'
#' Bibliographic fields are always kept. Crossref abstracts are deposited by
#' publishers and are not covered by Crossref's open metadata terms, so with the
#' default `abstracts = "open_licence"` an abstract is kept only when its Crossref
#' record declares a CC BY, CC BY-SA, CC0 or public-domain-mark licence for the version of
#' record (or an unspecified version) that has started by the lookup date.
#' `abstracts = "all"` keeps every deposited abstract: the app authors then take
#' responsibility for the right to republish them, and the app says so.
#'
#' @param x A dataset from [prepare_data()] with a `url` mapping, or a data frame
#'   with the columns named by `study` and `doi`.
#' @param study,doi Column names used when `x` is not a prepared dataset.
#' @param mailto Contact email sent to Crossref, as its etiquette requests, to use
#'   the "polite" pool. Defaults to the `METAUI_MAILTO` environment variable;
#'   leave it unset to query anonymously.
#' @param abstracts `"open_licence"` (default), `"none"`, or `"all"`; see Details.
#' @param cache Optional CSV path. Rows already retrieved under the same abstract
#'   policy are reused and only the remaining DOIs are queried; the result is
#'   written back. Review or edit it like any other input file.
#' @param pause Seconds between requests (Crossref's public pool allows a few per second).
#' @returns A data frame with one row per study: `study`, `doi`, `status`
#'   (`found`, `no_doi`, `invalid_doi`, `not_found`, `conflicting_dois` or `error`),
#'   `title`, `authors`, `year`, `journal`, `volume`, `issue`, `pages`, `abstract`,
#'   `abstract_licence`, `abstract_note`, `metadata_source`, `abstract_policy` and `retrieved`.
#' @export
#' @examples
#' \dontrun{
#' raw <- data.frame(study = c("Smith 2020", "Lee 2021"), d = c(.2, .4), vi = c(.02, .03),
#'                   doi = c("10.1371/journal.pone.0287911", "10.1037/0033-2909.112.1.155"))
#' app_data <- prepare_data(raw, "study", "d", variance = "vi", url = "doi")
#' info <- fetch_study_metadata(app_data, cache = "study-metadata.csv")
#' generate_shiny(app_data, "Example", study_info = info, save_to_folder = "app")
#' }
fetch_study_metadata <- function(x, study = "study", doi = "doi", mailto = Sys.getenv("METAUI_MAILTO"),
                                 abstracts = c("open_licence", "none", "all"), cache = NULL, pause = 0.2) {
  abstracts <- match.arg(abstracts)
  if (!is.data.frame(x)) stop("x must be a prepared dataset or a data frame.")
  if ("metaUI__study_id" %in% names(x)) {
    if (!"metaUI__url" %in% names(x)) stop("The prepared dataset has no url mapping; pass prepare_data(url = ...) or a study/DOI data frame.")
    pairs <- data.frame(study = as.character(x$metaUI__study_id), doi = as.character(x$metaUI__url))
  } else {
    if (!all(c(study, doi) %in% names(x))) stop("Columns not found: ", paste(setdiff(c(study, doi), names(x)), collapse = ", "))
    pairs <- data.frame(study = as.character(x[[study]]), doi = as.character(x[[doi]]))
  }
  pairs$doi <- metaUI_normalise_doi(pairs$doi)
  pairs <- unique(pairs[!is.na(pairs$study), , drop = FALSE])
  studies <- unique(pairs$study)
  dois <- vapply(studies, function(s) {
    d <- unique(stats::na.omit(pairs$doi[pairs$study == s]))
    if (length(d) > 1L) "<conflict>" else if (length(d)) d else NA_character_
  }, character(1), USE.NAMES = FALSE)

  previous <- if (!is.null(cache) && file.exists(cache))
    utils::read.csv(cache, colClasses = "character", na.strings = "") else NULL
  today <- format(Sys.Date())
  lookup <- unique(dois[!is.na(dois) & dois != "<conflict>" & metaUI_valid_doi(dois)])
  records <- list()
  for (d in lookup) {
    hit <- previous[!is.na(previous$doi) & previous$doi == d & previous$status == "found" &
                    previous$abstract_policy == abstracts, , drop = FALSE]
    if (!is.null(hit) && nrow(hit)) { records[[d]] <- as.list(hit[1, ]); next }
    if (length(records)) Sys.sleep(pause)
    records[[d]] <- metaUI_crossref_record(d, mailto, abstracts, today)
  }

  fields <- c("title", "authors", "year", "journal", "volume", "issue", "pages",
              "abstract", "abstract_licence", "abstract_note", "metadata_source", "retrieved")
  out <- lapply(seq_along(studies), function(i) {
    d <- dois[i]
    row <- if (is.na(d)) list(status = "no_doi") else if (d == "<conflict>") list(status = "conflicting_dois")
      else if (!metaUI_valid_doi(d)) list(status = "invalid_doi") else records[[d]]
    c(list(study = studies[i], doi = if (identical(d, "<conflict>")) NA_character_ else d, status = row$status),
      stats::setNames(lapply(fields, function(f) if (is.null(row[[f]])) NA_character_ else as.character(row[[f]])), fields),
      list(abstract_policy = abstracts))
  })
  out <- do.call(rbind, lapply(out, as.data.frame, stringsAsFactors = FALSE))
  if (!is.null(cache)) utils::write.csv(out, cache, row.names = FALSE, na = "")

  problems <- out[out$status != "found", c("study", "doi", "status"), drop = FALSE]
  kept <- sum(!is.na(out$abstract) & nzchar(out$abstract))
  message(sum(out$status == "found"), " of ", nrow(out), " studies matched in Crossref; ",
    kept, " abstracts kept (policy: ", abstracts, ").")
  if (nrow(problems)) message("Not matched (no metadata added):\n",
    paste0("  ", problems$study, " [", problems$status, "] ", ifelse(is.na(problems$doi), "", problems$doi), collapse = "\n"))
  out
}

# Bare lower-case DOI from "10.x/y", "doi:10.x/y" or a doi.org URL; anything else is kept for reporting.
metaUI_normalise_doi <- function(x) {
  x <- trimws(as.character(x))
  x[!is.na(x) & !nzchar(x)] <- NA
  x <- sub("^(https?://(dx\\.)?doi\\.org/|doi:\\s*)", "", x, ignore.case = TRUE)
  x[!is.na(x)] <- vapply(x[!is.na(x)], utils::URLdecode, character(1), USE.NAMES = FALSE)
  ifelse(grepl("^10\\.", x), tolower(x), x)
}

metaUI_valid_doi <- function(x) !is.na(x) & grepl("^10\\.[0-9]{4,9}/\\S+$", x)

# One Crossref work as a flat list; never throws, so one failure cannot stop the batch.
metaUI_crossref_record <- function(doi, mailto, abstracts, today) {
  query <- if (nzchar(mailto)) paste0("?mailto=", utils::URLencode(mailto, reserved = TRUE)) else ""
  url <- paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = TRUE), query)
  agent <- paste0("metaUI/", utils::packageVersion("metaUI"),
    " (https://github.com/LukasWallrich/metaUI", if (nzchar(mailto)) paste0("; mailto:", mailto), ")")
  json <- tryCatch(metaUI_http_get(url, agent), error = function(e) e)
  if (inherits(json, "error")) {
    status <- if (grepl("404", conditionMessage(json))) "not_found" else "error"
    return(list(status = status))
  }
  metaUI_parse_crossref(jsonlite::fromJSON(json, simplifyVector = FALSE)$message, abstracts, today)
}

# Separate so tests can replace the network call.
metaUI_http_get <- function(url, agent) {
  path <- tempfile(fileext = ".json"); on.exit(unlink(path))
  # libcurl reports the HTTP status (e.g. 404) as a warning before failing.
  withCallingHandlers(
    utils::download.file(url, path, quiet = TRUE, method = "libcurl", headers = c(`User-Agent` = agent)),
    warning = function(w) stop(conditionMessage(w), call. = FALSE))
  paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

metaUI_parse_crossref <- function(m, abstracts, today) {
  first <- function(x) if (length(x)) metaUI_plain_text(x[[1]]) else NA_character_
  authors <- vapply(m$author, function(a) {
    if (!is.null(a$family)) paste0(a$family, if (!is.null(a$given)) paste0(", ", a$given)) else if (!is.null(a$name)) a$name else NA_character_
  }, character(1))
  year <- m$issued$`date-parts`[[1]][[1]]
  licence <- metaUI_open_licence(m$license, today)
  abstract <- if (is.null(m$abstract)) NA_character_ else metaUI_jats_text(m$abstract)
  note <- if (is.na(abstract)) "No abstract deposited in Crossref."
    else if (abstracts == "none") "Abstract not requested."
    else if (abstracts == "all") if (is.na(licence)) "Included by the app authors; no open licence recorded in Crossref." else NA_character_
    else if (is.na(licence)) "Abstract not shown: no open licence (CC BY, CC BY-SA, CC0) recorded in Crossref." else NA_character_
  keep <- !is.na(abstract) && (abstracts == "all" || (abstracts == "open_licence" && !is.na(licence)))
  list(status = "found", title = first(m$title),
    authors = if (length(authors)) paste(stats::na.omit(authors), collapse = "; ") else NA_character_,
    year = if (is.null(year)) NA_character_ else as.character(year),
    journal = first(m$`container-title`),
    volume = if (is.null(m$volume)) NA_character_ else m$volume,
    issue = if (is.null(m$issue)) NA_character_ else m$issue,
    pages = if (is.null(m$page)) NA_character_ else m$page,
    abstract = if (keep) abstract else NA_character_,
    abstract_licence = if (keep) licence else NA_character_, abstract_note = note,
    metadata_source = "Crossref", retrieved = today)
}

# URL of a CC BY, CC BY-SA, CC0 or public-domain-mark licence for the version of record
# (or an unspecified version) whose start date has passed. NC/ND variants are not accepted.
metaUI_open_licence <- function(licences, today) {
  for (l in licences) {
    started <- is.null(l$start$`date-time`) || substr(l$start$`date-time`, 1, 10) <= today
    if (isTRUE(l$`content-version` %in% c("vor", "unspecified")) && started &&
        grepl("^https?://(www\\.)?creativecommons\\.org/(licenses/by(-sa)?/|publicdomain/(zero|mark)/)", l$URL))
      return(l$URL)
  }
  NA_character_
}

# Plain text from Crossref strings: markup removed, entities decoded.
metaUI_plain_text <- function(x) {
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("&lt;", "<", gsub("&gt;", ">", gsub("&quot;", "\"", gsub("&#39;|&apos;", "'", x))))
  for (code in unique(regmatches(x, gregexpr("&#x?[0-9A-Fa-f]+;", x))[[1]])) {
    n <- sub("^&#(x?)([0-9A-Fa-f]+);$", "\\2", code)
    value <- if (grepl("^&#x", code)) strtoi(n, 16L) else as.integer(n)
    x <- gsub(code, intToUtf8(value), x, fixed = TRUE)
  }
  x <- gsub("&amp;", "&", x)
  trimws(gsub("[[:space:]]+", " ", x))
}

# JATS abstract to paragraphs separated by blank lines; a lone "Abstract" heading is dropped.
metaUI_jats_text <- function(x) {
  parts <- strsplit(gsub("</(jats:)?(p|title)>", "\n\n", x), "\n\n", fixed = TRUE)[[1]]
  parts <- vapply(parts, metaUI_plain_text, character(1), USE.NAMES = FALSE)
  parts <- parts[nzchar(parts)]
  if (length(parts) && tolower(parts[1]) == "abstract") parts <- parts[-1]
  if (length(parts)) paste(parts, collapse = "\n\n") else NA_character_
}

study_info_fields <- c("study", "doi", "url", "status", "title", "authors", "year", "journal", "volume",
  "issue", "pages", "abstract", "abstract_licence", "abstract_note", "metadata_source", "abstract_policy", "retrieved")

# Checks author-supplied or fetched study information against the prepared dataset.
validate_study_info <- function(info, dataset) {
  if (is.character(info) && length(info) == 1L) info <- utils::read.csv(info, colClasses = "character", na.strings = "")
  if (!is.data.frame(info) || !"study" %in% names(info)) stop("study_info must be a data frame (or CSV) with a `study` column.")
  unknown <- setdiff(names(info), study_info_fields)
  if (length(unknown)) stop("Unknown study_info columns: ", paste(unknown, collapse = ", "),
    ". Allowed: ", paste(study_info_fields, collapse = ", "))
  info[] <- lapply(info, function(x) { x <- as.character(x); x[!is.na(x) & !nzchar(trimws(x))] <- NA; x })
  if (anyNA(info$study) || anyDuplicated(info$study)) stop("study_info needs one row per study, with nonmissing study IDs.")
  unmatched <- setdiff(info$study, as.character(dataset$metaUI__study_id))
  if (length(unmatched)) stop("study_info rows do not match any study ID: ", paste(utils::head(unmatched, 10), collapse = ", "))
  if ("doi" %in% names(info)) info$doi <- metaUI_normalise_doi(info$doi)
  # Keep rows with something to show; a DOI alone only if the lookup found it (or none was run).
  shown <- intersect(c("title", "authors", "year", "journal", "url", "abstract"), names(info))
  found_doi <- if ("doi" %in% names(info)) !is.na(info$doi) & (if ("status" %in% names(info)) info$status %in% c(NA, "found") else TRUE) else FALSE
  info <- info[rowSums(!is.na(info[shown])) > 0 | found_doi, , drop = FALSE]
  rownames(info) <- NULL
  info
}

# Provenance for validation.json and the About page.
study_info_report <- function(info, dataset) {
  if (is.null(info)) return(NULL)
  col <- function(name) if (name %in% names(info)) info[[name]] else rep(NA_character_, nrow(info))
  list(studies = length(unique(dataset$metaUI__study_id)), with_details = nrow(info),
    with_abstract = sum(!is.na(col("abstract"))),
    sources = sort(unique(stats::na.omit(col("metadata_source")))),
    abstract_policy = sort(unique(stats::na.omit(col("abstract_policy")))),
    retrieved = sort(unique(stats::na.omit(col("retrieved")))),
    note = "Retrieved by the authors when building; the app does not query external services. Abstracts are author responsibility unless an open licence is shown.")
}
