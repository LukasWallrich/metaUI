#' Look up study metadata from DOIs at build time
#'
#' Queries the free Crossref REST API once per distinct DOI and returns one row
#' per study for `generate_shiny(study_info = ...)`. Run this while authoring:
#' generated apps read the stored result and never contact external services. DOIs
#' that are missing, malformed, unknown to Crossref or unreachable are reported in
#' the `status` column and a message; nothing is guessed or matched by title.
#'
#' Bibliographic fields are always kept. Abstracts are not covered by Crossref's
#' open metadata terms, so with the default `abstracts = "open_licence"` an abstract
#' is kept only when the article is published under a Creative Commons licence (CC
#' BY, with or without SA, ND or NC terms), CC0 or the Public Domain Mark. The licence is taken from the Crossref record (for the
#' version of record, or an unspecified version, that has started by the lookup
#' date) or, failing that, from the published-version locations that OpenAlex
#' records. When Crossref has no abstract, OpenAlex's abstract is used. OpenAlex
#' is asked only when Crossref leaves the abstract or its licence open.
#'
#' NonCommercial (NC) licences permit use that is not primarily intended for
#' commercial advantage or monetary compensation, which covers academic apps. If
#' the app is used commercially, set `abstracts = "exclude_nc"` to drop them. `abstracts = "all"`
#' keeps every abstract found: the app authors then take responsibility for the
#' right to republish them, and the app says so.
#'
#' Single-work lookups by DOI are free on both services and need no key. Crossref
#' asks for a contact address: set `METAUI_MAILTO` (e.g. in `~/.Renviron`). An
#' OpenAlex key is optional and only raises the rate limit; create a free account
#' at <https://openalex.org>, copy the key from Settings > API key and set
#' `OPENALEX_API_KEY`. The key is sent
#' as a request header and is not stored in the result or the app.
#'
#' @param x A dataset from [prepare_data()] with a `url` mapping, or a data frame
#'   with the columns named by `study` and `doi`.
#' @param study,doi Column names used when `x` is not a prepared dataset.
#' @param mailto Contact email sent to Crossref, as its etiquette requests, to use
#'   the "polite" pool. Defaults to the `METAUI_MAILTO` environment variable;
#'   leave it unset to query anonymously.
#' @param abstracts `"open_licence"` (default), `"exclude_nc"`, `"none"`, or
#'   `"all"`; see Details.
#' @param openalex Use OpenAlex for licences and abstracts that Crossref lacks.
#' @param openalex_key Optional OpenAlex API key. Defaults to the
#'   `OPENALEX_API_KEY` environment variable.
#' @param cache Optional CSV path. Rows already retrieved under the same abstract
#'   policy are reused and only the remaining DOIs are queried; the result is
#'   written back. Review or edit it like any other input file.
#' @param pause Seconds between requests (both services allow several per second).
#' @returns A data frame with one row per study: `study`, `doi`, `status`
#'   (`found`, `no_doi`, `invalid_doi`, `not_found`, `conflicting_dois` or `error`),
#'   `title`, `authors` (`"Family, Given; ..."`), `year`, `journal`, `volume`,
#'   `issue`, `pages`, `abstract`, `abstract_licence` (a licence URL from Crossref
#'   or a licence code from OpenAlex), `abstract_source`, `abstract_note`,
#'   `metadata_source`, `abstract_policy` and `retrieved`.
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
                                 abstracts = c("open_licence", "exclude_nc", "none", "all"), openalex = TRUE,
                                 openalex_key = Sys.getenv("OPENALEX_API_KEY"), cache = NULL, pause = 0.2) {
  abstracts <- match.arg(abstracts)
  policy <- paste0(abstracts, if (!openalex) "; Crossref only")
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
  openalex_failed <- character()
  requests <- 0L
  wait <- function() { if (requests > 0L) Sys.sleep(pause); requests <<- requests + 1L }
  for (d in lookup) {
    hit <- previous[!is.na(previous$doi) & previous$doi == d & previous$status == "found" &
                    previous$abstract_policy == policy, , drop = FALSE]
    if (!is.null(hit) && nrow(hit)) { records[[d]] <- as.list(hit[1, ]); next }
    wait()
    record <- metaUI_crossref_record(d, mailto, abstracts, today)
    if (openalex && record$status == "found" && abstracts != "none" &&
        (is.na(record$abstract) || (abstracts != "all" && is.na(record$abstract_licence)))) {
      wait()
      work <- metaUI_openalex_record(d, openalex_key)
      if (is.null(work)) openalex_failed <- c(openalex_failed, d)
      record <- metaUI_add_openalex(record, work, abstracts)
    }
    records[[d]] <- metaUI_apply_abstract_policy(record, abstracts)
  }

  fields <- c("title", "authors", "year", "journal", "volume", "issue", "pages", "abstract",
              "abstract_licence", "abstract_source", "abstract_note", "metadata_source", "retrieved")
  out <- lapply(seq_along(studies), function(i) {
    d <- dois[i]
    row <- if (is.na(d)) list(status = "no_doi") else if (d == "<conflict>") list(status = "conflicting_dois")
      else if (!metaUI_valid_doi(d)) list(status = "invalid_doi") else records[[d]]
    c(list(study = studies[i], doi = if (identical(d, "<conflict>")) NA_character_ else d, status = row$status),
      stats::setNames(lapply(fields, function(f) if (is.null(row[[f]])) NA_character_ else as.character(row[[f]])), fields),
      list(abstract_policy = policy))
  })
  out <- do.call(rbind, lapply(out, as.data.frame, stringsAsFactors = FALSE))
  if (!is.null(cache)) utils::write.csv(out, cache, row.names = FALSE, na = "")

  problems <- out[out$status != "found", c("study", "doi", "status"), drop = FALSE]
  kept <- sum(!is.na(out$abstract) & nzchar(out$abstract))
  message(sum(out$status == "found"), " of ", nrow(out), " studies matched in Crossref; ",
    kept, " abstracts kept (policy: ", policy, ").")
  if (nrow(problems)) message("Not matched (no metadata added):\n",
    paste0("  ", problems$study, " [", problems$status, "] ", ifelse(is.na(problems$doi), "", problems$doi), collapse = "\n"))
  if (length(openalex_failed)) message("OpenAlex could not be checked for: ", paste(openalex_failed, collapse = ", "),
    ". Their abstracts rely on Crossref alone.")
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

metaUI_agent <- function(mailto = "") paste0("metaUI/", utils::packageVersion("metaUI"),
  " (https://github.com/LukasWallrich/metaUI", if (nzchar(mailto)) paste0("; mailto:", mailto), ")")

# One Crossref work as a flat list; never throws, so one failure cannot stop the batch.
metaUI_crossref_record <- function(doi, mailto, abstracts, today) {
  query <- if (nzchar(mailto)) paste0("?mailto=", utils::URLencode(mailto, reserved = TRUE)) else ""
  url <- paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = TRUE), query)
  json <- tryCatch(metaUI_http_get(url, c(`User-Agent` = metaUI_agent(mailto))), error = function(e) e)
  if (inherits(json, "error")) {
    status <- if (metaUI_not_found(json)) "not_found" else "error"
    return(list(status = status))
  }
  metaUI_parse_crossref(jsonlite::fromJSON(json, simplifyVector = FALSE)$message, abstracts, today)
}

# Licences and abstract of one OpenAlex work: an empty list if OpenAlex lacks the DOI, NULL if unreachable.
metaUI_openalex_record <- function(doi, key) {
  url <- paste0("https://api.openalex.org/works/doi:", utils::URLencode(doi, reserved = TRUE),
    "?select=locations,abstract_inverted_index")
  headers <- c(`User-Agent` = metaUI_agent(), if (nzchar(key)) c(Authorization = paste("Bearer", key)))
  json <- tryCatch(metaUI_http_get(url, headers), error = function(e) e)
  if (!inherits(json, "error")) jsonlite::fromJSON(json, simplifyVector = FALSE)
  else if (metaUI_not_found(json)) list() else NULL
}

# The HTTP status phrase only; the message also holds the URL, whose DOI may contain "404".
metaUI_not_found <- function(e) grepl("HTTP status was '404", conditionMessage(e), fixed = TRUE)

# Separate so tests can replace the network call.
metaUI_http_get <- function(url, headers) {
  path <- tempfile(fileext = ".json"); on.exit(unlink(path))
  # libcurl reports the HTTP status (e.g. 404) as a warning before failing.
  withCallingHandlers(
    utils::download.file(url, path, quiet = TRUE, method = "libcurl", headers = headers),
    warning = function(w) stop(conditionMessage(w), call. = FALSE))
  paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

# Bibliographic fields plus the candidate abstract and its Crossref licence; the policy is applied later.
metaUI_parse_crossref <- function(m, abstracts, today) {
  first <- function(x) if (length(x)) metaUI_plain_text(x[[1]]) else NA_character_
  authors <- vapply(m$author, function(a) {
    if (!is.null(a$family)) paste0(a$family, if (!is.null(a$given)) paste0(", ", a$given)) else if (!is.null(a$name)) a$name else NA_character_
  }, character(1))
  year <- m$issued$`date-parts`[[1]][[1]]
  abstract <- if (is.null(m$abstract) || abstracts == "none") NA_character_ else metaUI_jats_text(m$abstract)
  list(status = "found", title = first(m$title),
    authors = if (length(authors)) paste(stats::na.omit(authors), collapse = "; ") else NA_character_,
    year = if (is.null(year)) NA_character_ else as.character(year),
    journal = first(m$`container-title`),
    volume = if (is.null(m$volume)) NA_character_ else m$volume,
    issue = if (is.null(m$issue)) NA_character_ else m$issue,
    pages = if (is.null(m$page)) NA_character_ else m$page,
    abstract = abstract, abstract_licence = metaUI_crossref_licence(m$license, today, abstracts),
    abstract_source = if (is.na(abstract)) NA_character_ else "Crossref",
    metadata_source = "Crossref", retrieved = today)
}

# Licence classes: "open" (CC BY, BY-SA, BY-ND, CC0, Public Domain Mark) or "noncommercial"
# (BY-NC, BY-NC-SA, BY-NC-ND); NA for anything else. Takes Crossref URLs or OpenAlex codes.
metaUI_licence_class <- function(x) {
  x <- tolower(x)
  cc <- sub("^https?://(www\\.)?creativecommons\\.org/licenses/([a-z-]+)/.*$", "cc-\\2", x)
  ifelse(grepl("^https?://(www\\.)?creativecommons\\.org/publicdomain/(zero|mark)/", x) | x %in% c("cc0", "public-domain"), "open",
    ifelse(cc %in% c("cc-by", "cc-by-sa", "cc-by-nd"), "open",
      ifelse(cc %in% c("cc-by-nc", "cc-by-nc-sa", "cc-by-nc-nd"), "noncommercial", NA_character_)))
}

# In order of preference; NC licences unless the authors exclude them.
metaUI_allowed_licences <- function(abstracts) if (abstracts == "exclude_nc") "open" else c("open", "noncommercial")

# The most open acceptable licence among candidates (Crossref URLs or OpenAlex codes), or NA.
metaUI_pick_licence <- function(candidates, abstracts) {
  candidates <- as.character(stats::na.omit(candidates))
  class <- metaUI_licence_class(candidates)
  for (allowed in metaUI_allowed_licences(abstracts)) if (any(class %in% allowed)) return(candidates[class %in% allowed][1])
  NA_character_
}

# Crossref licences count for the version of record (or an unspecified version) once started.
metaUI_crossref_licence <- function(licences, today, abstracts) {
  metaUI_pick_licence(vapply(licences, function(l) {
    started <- is.null(l$start$`date-time`) || substr(l$start$`date-time`, 1, 10) <= today
    if (isTRUE(l$`content-version` %in% c("vor", "unspecified")) && started && is.character(l$URL)) l$URL else NA_character_
  }, character(1)), abstracts)
}

# Fills the licence (from published-version locations) and, if Crossref had none, the abstract.
metaUI_add_openalex <- function(record, work, abstracts) {
  if (is.null(work)) return(record)
  if (is.na(record$abstract_licence)) record$abstract_licence <- metaUI_pick_licence(unlist(lapply(work$locations,
    function(l) if (identical(l$version, "publishedVersion") && is.character(l$license)) l$license)), abstracts)
  if (is.na(record$abstract) && length(work$abstract_inverted_index)) {
    record$abstract <- metaUI_inverted_abstract(work$abstract_inverted_index)
    record$abstract_source <- "OpenAlex"
  }
  record
}

# Plain text from OpenAlex's word -> positions index (already plain, so no markup is stripped).
metaUI_inverted_abstract <- function(index) {
  positions <- unlist(index, use.names = FALSE)
  words <- rep(names(index), lengths(index))
  trimws(gsub("[[:space:]]+", " ", paste(words[order(positions)], collapse = " ")))
}

# Keeps the abstract only when the policy allows it; reader-facing note otherwise.
metaUI_apply_abstract_policy <- function(record, abstracts) {
  if (record$status != "found") return(record)
  licensed <- !is.na(record$abstract_licence)
  keep <- !is.na(record$abstract) && (abstracts == "all" || (abstracts != "none" && licensed))
  record$abstract_note <- if (abstracts == "none") NA_character_
    else if (keep) (if (licensed) NA_character_ else "Included by the app authors.")
    else if (abstracts == "all") "No abstract found." else "No openly licensed abstract found."
  if (!keep) record[c("abstract", "abstract_source")] <- NA_character_
  if (!keep || abstracts == "none") record$abstract_licence <- NA_character_
  record
}

# Plain text from Crossref strings: markup removed, entities decoded.
metaUI_plain_text <- function(x) trimws(gsub("[[:space:]]+", " ", metaUI_decode_entities(gsub("<[^>]+>", "", x))))

# Decodes the HTML entities that bibliographic exports leave in text ("&amp;", "&#8211;",
# "&ndash;"); other named entities stay as they are. Invalid code points are left
# undecoded. &amp; goes last, so "&amp;lt;" becomes the literal "&lt;" rather than "<".
metaUI_decode_entities <- function(x) {
  named <- c(lt = "<", gt = ">", quot = "\"", apos = "'", nbsp = " ", ndash = "\u2013", mdash = "\u2014",
    hellip = "\u2026", lsquo = "\u2018", rsquo = "\u2019", ldquo = "\u201c", rdquo = "\u201d")
  for (name in names(named)) x <- gsub(paste0("&", name, ";"), named[[name]], x, fixed = TRUE)
  for (code in unique(unlist(regmatches(x, gregexpr("&#([xX][0-9A-Fa-f]+|[0-9]+);", x))))) {
    n <- sub("^&#[xX]?([0-9A-Fa-f]+);$", "\\1", code)
    value <- suppressWarnings(if (grepl("^&#[xX]", code)) strtoi(n, 16L) else as.integer(n))
    char <- if (is.na(value) || value <= 0L) NA_character_ else intToUtf8(value)
    if (!is.na(char)) x <- gsub(code, char, x, fixed = TRUE)
  }
  gsub("&amp;", "&", x, fixed = TRUE)
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
  "issue", "pages", "abstract", "abstract_licence", "abstract_source", "abstract_note", "metadata_source", "abstract_policy", "retrieved")

# Checks author-supplied or fetched study information against the prepared dataset.
validate_study_info <- function(info, dataset) {
  if (is.character(info) && length(info) == 1L) info <- utils::read.csv(info, colClasses = "character", na.strings = "")
  if (!is.data.frame(info) || !"study" %in% names(info)) stop("study_info must be a data frame (or CSV) with a `study` column.")
  unknown <- setdiff(names(info), study_info_fields)
  if (length(unknown)) stop("Unknown study_info columns: ", paste(unknown, collapse = ", "),
    ". Allowed: ", paste(study_info_fields, collapse = ", "))
  info[] <- lapply(info, function(x) { x <- as.character(x); x[!is.na(x) & !nzchar(trimws(x))] <- NA; x })
  # Older or hand-written files may keep escapes that prepare_data() decoded in study IDs.
  escaped <- !info$study %in% as.character(dataset$metaUI__study_id)
  info$study[escaped] <- metaUI_decode_entities(info$study[escaped])
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
    sources = sort(unique(stats::na.omit(c(col("metadata_source"), col("abstract_source"))))),
    abstract_policy = sort(unique(stats::na.omit(col("abstract_policy")))),
    retrieved = sort(unique(stats::na.omit(col("retrieved")))),
    note = "Retrieved by the authors when building; the app does not query external services. Abstracts are author responsibility unless an open licence is shown.")
}
