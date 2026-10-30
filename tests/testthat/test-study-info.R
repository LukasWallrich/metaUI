# Crossref-shaped responses; no network access in tests.
crossref_json <- function(doi, licence = NULL, version = "vor", start = "2020-01-01T00:00:00Z", abstract = TRUE) {
  message <- list(DOI = doi, title = list("A <i>tidy</i> title &amp; more"),
    author = list(list(given = "Ada", family = "Lovelace"), list(given = "Alan", family = "Turing"), list(name = "Study Group")),
    `container-title` = list("Journal of Tests"), volume = "4", issue = "2", page = "1-9",
    issued = list(`date-parts` = list(list(2021, 5))))
  if (abstract) message$abstract <- "<jats:title>Abstract</jats:title><jats:p>First &lt;para&gt;.</jats:p><jats:p>Second\n  para &#x2013; end.</jats:p>"
  if (!is.null(licence)) message$license <- list(list(URL = licence, `content-version` = version,
    start = list(`date-time` = start)))
  as.character(jsonlite::toJSON(list(status = "ok", message = message), auto_unbox = TRUE))
}

openalex_json <- function(licence = "cc-by", version = "publishedVersion", abstract = TRUE) {
  work <- list(locations = list(list(license = licence, version = version), list(license = NULL, version = "publishedVersion")))
  if (abstract) work$abstract_inverted_index <- list(Words = list(0L, 3L), from = list(1L), OpenAlex = list(2L), `p<.05.` = list(4L))
  as.character(jsonlite::toJSON(work, auto_unbox = TRUE, null = "null"))
}

test_that("Crossref records become plain-text metadata; abstracts follow the licence rule", {
  today <- "2026-10-30"
  decide <- function(..., abstracts = "open_licence") metaUI:::metaUI_apply_abstract_policy(metaUI:::metaUI_parse_crossref(
    jsonlite::fromJSON(crossref_json("10.1/x", ...), simplifyVector = FALSE)$message, abstracts, today), abstracts)
  open <- decide(licence = "http://creativecommons.org/licenses/by/4.0/")
  expect_identical(open$title, "A tidy title & more")
  expect_identical(open$authors, "Lovelace, Ada; Turing, Alan; Study Group")
  expect_identical(c(open$year, open$journal, open$volume, open$issue, open$pages), c("2021", "Journal of Tests", "4", "2", "1-9"))
  expect_identical(open$abstract, "First <para>.\n\nSecond para – end.")
  expect_identical(c(open$abstract_licence, open$abstract_source), c("http://creativecommons.org/licenses/by/4.0/", "Crossref"))
  expect_true(is.na(open$abstract_note))
  # ND, NC, ShareAlike, CC0 and the Public Domain Mark allow verbatim sharing for non-commercial use.
  for (url in c("https://creativecommons.org/licenses/by-nd/4.0/", "https://creativecommons.org/licenses/by-sa/3.0/",
                "https://creativecommons.org/licenses/by-nc-nd/4.0/",
                "https://creativecommons.org/publicdomain/zero/1.0/", "https://creativecommons.org/publicdomain/mark/1.0/"))
    expect_false(is.na(decide(licence = url)$abstract))
  # Withheld: no licence, a publisher/TDM licence, an embargo, or a licence for another version.
  for (args in list(list(), list(licence = "https://www.elsevier.com/tdm/userlicense/1.0/"),
                    list(licence = "https://creativecommons.org/licenses/by/4.0/", start = "2030-01-01T00:00:00Z"),
                    list(licence = "https://creativecommons.org/licenses/by/4.0/", version = "tdm"),
                    list(abstract = FALSE, licence = "https://creativecommons.org/licenses/by/4.0/"))) {
    closed <- do.call(decide, args)
    expect_true(is.na(closed$abstract) && is.na(closed$abstract_licence))
    expect_identical(closed$abstract_note, "No openly licensed abstract found.")
    expect_identical(closed$title, "A tidy title & more")
  }
  # Commercial apps can exclude NC licences.
  nc <- decide(licence = "https://creativecommons.org/licenses/by-nc-nd/4.0/", abstracts = "exclude_nc")
  expect_true(is.na(nc$abstract))
  expect_identical(nc$abstract_note, "No openly licensed abstract found.")
  all <- decide(abstracts = "all")
  expect_false(is.na(all$abstract))
  expect_identical(all$abstract_note, "Included by the app authors.")
  expect_identical(decide(abstract = FALSE, abstracts = "all")$abstract_note, "No abstract found.")
  none <- decide(licence = "https://creativecommons.org/licenses/by/4.0/", abstracts = "none")
  expect_true(is.na(none$abstract) && is.na(none$abstract_note) && is.na(none$abstract_licence))
})

test_that("OpenAlex fills a missing licence from published versions and a missing abstract", {
  record <- list(status = "found", abstract = "From Crossref.", abstract_licence = NA_character_, abstract_source = "Crossref")
  work <- function(...) jsonlite::fromJSON(openalex_json(...), simplifyVector = FALSE)
  add <- function(record, work, abstracts = "open_licence") metaUI:::metaUI_add_openalex(record, work, abstracts)
  filled <- add(record, work())
  expect_identical(filled[c("abstract", "abstract_licence", "abstract_source")], list(abstract = "From Crossref.", abstract_licence = "cc-by", abstract_source = "Crossref"))
  expect_true(is.na(add(record, work(version = "acceptedVersion"))$abstract_licence))
  expect_identical(add(record, work(licence = "cc-by-nc"))$abstract_licence, "cc-by-nc")
  expect_true(is.na(add(record, work(licence = "cc-by-nc"), "exclude_nc")$abstract_licence))
  expect_true(is.na(add(record, work(licence = "publisher-specific-oa"))$abstract_licence))
  rebuilt <- add(modifyList(record, list(abstract = NA_character_, abstract_source = NA_character_)), work())
  expect_identical(rebuilt$abstract, "Words from OpenAlex Words p<.05.")
  expect_identical(rebuilt$abstract_source, "OpenAlex")
  expect_identical(add(record, NULL), record)
})

test_that("fetch_study_metadata reports unmatched DOIs, never guesses, and reuses its cache", {
  calls <- character()
  local_mocked_bindings(metaUI_http_get = function(url, headers) {
    calls <<- c(calls, url)
    if (grepl("openalex", url)) {
      expect_identical(unname(headers[["Authorization"]]), "Bearer test-key")
      expect_false(grepl("reader@example.org", paste(headers, collapse = " "), fixed = TRUE))
      return(openalex_json())
    }
    expect_match(headers[["User-Agent"]], "mailto:reader@example.org", fixed = TRUE)
    if (grepl("missing", url)) stop("cannot open URL: HTTP status was '404 Not Found'")
    if (grepl("offline", url)) stop("Could not resolve host")
    doi <- utils::URLdecode(sub("^.*/works/([^?]*).*$", "\\1", url))
    crossref_json(doi, licence = if (doi != "10.1234/one") "https://creativecommons.org/licenses/by/4.0/")
  })
  x <- data.frame(study = c("A", "A", "B", "C", "D", "E", "F", "G", "G", "H"),
    doi = c("https://doi.org/10.1234/ABC", "10.1234/abc", "doi:10.1234/missing", "not a doi", NA,
            "10.1234/offline", "10.1234/abc", "10.1234/one", "10.1234/two", "10.1234/one"))
  cache <- tempfile(fileext = ".csv"); on.exit(unlink(cache))
  fetch <- function(...) fetch_study_metadata(x, mailto = "reader@example.org", openalex_key = "test-key", cache = cache, pause = 0, ...)
  messages <- capture_messages(info <- fetch())
  expect_match(messages[1], "3 of 8 studies matched in Crossref; 3 abstracts kept", fixed = TRUE)
  expect_match(messages[2], "B [not_found] 10.1234/missing", fixed = TRUE)
  expect_identical(info$study, c("A", "B", "C", "D", "E", "F", "G", "H"))
  expect_identical(info$status, c("found", "not_found", "invalid_doi", "no_doi", "error", "found", "conflicting_dois", "found"))
  expect_identical(info$doi[1:2], c("10.1234/abc", "10.1234/missing"))
  expect_true(all(is.na(info$title[-c(1, 6, 8)])))
  expect_identical(info$abstract_licence[c(1, 8)], c("https://creativecommons.org/licenses/by/4.0/", "cc-by"))
  # One request per distinct valid DOI; OpenAlex only where Crossref lacks an open licence.
  expect_identical(sum(grepl("crossref", calls)), 4L)
  expect_identical(calls[grepl("openalex", calls)], "https://api.openalex.org/works/doi:10.1234%2Fone?select=locations,abstract_inverted_index")
  expect_match(calls[1], "mailto=reader%40example.org", fixed = TRUE)
  expect_false(any(grepl("test-key", c(calls, readLines(cache)), fixed = TRUE)))
  calls <- character()
  again <- suppressMessages(fetch())
  expect_length(calls, 2) # found DOIs reused; failures retried
  expect_identical(again$title, info$title)
  none <- suppressMessages(fetch(abstracts = "none"))
  expect_length(calls, 6) # another abstract policy needs fresh records; no OpenAlex for "none"
  expect_true(all(is.na(none$abstract)))
  crossref_only <- suppressMessages(fetch(openalex = FALSE))
  expect_false(any(grepl("openalex", calls[-(1:6)])))
  expect_identical(crossref_only$abstract_note[8], "No openly licensed abstract found.")
  expect_error(fetch_study_metadata(prepared()), "no url mapping")
})

test_that("references follow APA author style", {
  apa <- metaUI:::metaUI_apa_authors
  expect_identical(apa("Doe, Jane Marie"), "Doe, J. M.")
  expect_identical(apa("Ioannidis, John P. A."), "Ioannidis, J. P. A.")
  expect_identical(apa("Doe, Jean-Paul; Roe, R.; Study Group"), "Doe, J.-P., Roe, R., & Study Group")
  expect_identical(apa(paste0("A", 1:21, ", Bo", collapse = "; ")), paste0(paste0("A", 1:19, ", B.", collapse = ", "), ", . . . A21, B."))
  expect_identical(metaUI:::metaUI_first_family("Doe, Jane; Roe, Rick"), "Doe & Roe")
  details <- metaUI:::metaUI_study_details(data.frame(study = "A", doi = "10.1/x", authors = "Doe, Jane", year = "2020",
    title = "Title", journal = "J", volume = "4", issue = "2", pages = "1-9", abstract = "Words.",
    abstract_licence = "cc-by", abstract_source = "OpenAlex", metadata_source = "Crossref", retrieved = "2026-10-30"))
  expect_match(details, '<span class="metaui-study-cite">Doe, J. (2020). <span class="metaui-study-title">Title.</span> <i>J</i>, <i>4</i>(2), 1–9. <a href="https://doi.org/10.1/x"', fixed = TRUE)
  expect_match(details, '<button type="button" class="metaui-copy-ref" aria-label="Copy reference"', fixed = TRUE)
  expect_match(details, "Licence: CC BY (as recorded by OpenAlex). Source: abstract from OpenAlex, reference from Crossref, retrieved 2026-10-30; formatting removed.", fixed = TRUE)
})

test_that("study information is stored at build time and shown as escaped, keyboard-operable details", {
  path <- tempfile("metaui-app-")
  on.exit(unlink(path, recursive = TRUE))
  x <- fixture(); x$doi <- rep(c("10.1234/a", NA), 8)
  d <- prepared(x, url = "doi")
  info <- data.frame(study = c("A", "C"), doi = c("10.1234/a", NA), title = c("<script>alert(1)</script> Title", "Second"),
    authors = c("Doe, Jane; Roe, Rick; Poe, Edgar", NA), year = c("2020", NA), abstract = c("One.\n\nTwo & three.", NA),
    abstract_licence = c("https://creativecommons.org/licenses/by/4.0/", NA), metadata_source = c("Crossref", NA))
  expect_error(generate_shiny(d, "x", study_info = transform(info, colour = "red"), save_to_folder = path, launch_app = FALSE), "Unknown study_info columns")
  expect_error(generate_shiny(d, "x", study_info = transform(info, study = c("A", "Z")), save_to_folder = path, launch_app = FALSE), "do not match")
  generate_shiny(d, "Tiny synthetic fixture", study_info = info, save_to_folder = path, launch_app = FALSE, date = "2026-10-30")
  expect_true(file.exists(file.path(path, "study_info.rds")))
  report <- jsonlite::read_json(file.path(path, "validation.json"))
  expect_equal(report$study_info$with_details, 2)
  expect_equal(report$study_info$with_abstract, 1)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(path)
  env <- new.env(parent = globalenv())
  sys.source("global.R", env)
  expect_match(as.character(env$ui), "Select a study name", fixed = TRUE)
  details <- env$metaUI_study_details_html
  expect_false(grepl("<script>", details[1], fixed = TRUE))
  expect_match(details[1], "&lt;script&gt;alert(1)&lt;/script&gt; Title.</span>", fixed = TRUE)
  expect_match(details[1], "<p>One.</p><p>Two &amp; three.</p>", fixed = TRUE)
  expect_match(details[1], ">CC BY 4.0</a>", fixed = TRUE)
  expect_match(details[1], 'href="https://doi.org/10.1234/a"', fixed = TRUE)
  expect_match(details[1], "Doe, J., Roe, R., &amp; Poe, E. (2020).", fixed = TRUE)
  expect_identical(env$metaUI_study_reference_text[1], "Doe et al. (2020). <script>alert(1)</script> Title.")
  cells <- env$metaUI_study_cells(c("A", "B", "C"), c("A", "B", "C"), env$metaUI_study_info,
    env$metaUI_study_details_html, env$metaUI_study_reference_text)
  expect_match(cells$label[1], '^<button type="button" class="metaui-study-toggle" aria-expanded="false" title="Doe et al. \\(2020\\). &lt;script&gt;')
  expect_identical(cells$label[2], "B")
  expect_identical(cells$details, c(details[1], "", details[2]))
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-10, 10), go = 1)
    expect_match(output$sample_table, '{"targets":5,"visible":false,"searchable":false}', fixed = TRUE)
    expect_match(output$sample_table, '"name":"metaUI__details","targets":5', fixed = TRUE)
  })
  setwd(wd)
  # Rebuilding without study information removes the stored file.
  generate_shiny(d, "Tiny synthetic fixture", save_to_folder = path, launch_app = FALSE, overwrite = TRUE)
  expect_false(file.exists(file.path(path, "study_info.rds")))
})

test_that("the sample table escapes study labels and adds no details without study information", {
  path <- tempfile("metaui-app-")
  on.exit(unlink(path, recursive = TRUE))
  x <- fixture(); x$study <- rep(c("<b>A</b>", LETTERS[2:8]), each = 2)
  generate_shiny(prepared(x), "Tiny synthetic fixture", save_to_folder = path, launch_app = FALSE)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(path)
  env <- new.env(parent = globalenv())
  sys.source("global.R", env)
  expect_null(env$metaUI_study_info)
  expect_false(grepl("Select a study name", as.character(env$ui), fixed = TRUE))
  cells <- env$metaUI_study_cells(c("<b>A</b>", "B"), c("<b>A</b>", "B"), NULL, character(), character())
  expect_identical(cells, list(label = c("&lt;b&gt;A&lt;/b&gt;", "B"), details = c("", "")))
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-10, 10), go = 1)
    expect_match(output$sample_table, '"name":"metaUI__details"', fixed = TRUE)
  })
})

test_that("build_app accepts a study_info CSV under schema version 2", {
  dir <- tempfile("metaui-config-"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  x <- fixture(); x$doi <- "10.1234/a"
  utils::write.csv(x, file.path(dir, "data.csv"), row.names = FALSE)
  utils::write.csv(data.frame(study = "B", title = "Study B", year = "2019"), file.path(dir, "studies.csv"), row.names = FALSE)
  config <- list(schema_version = 2, data = "data.csv", dataset_name = "Config", output = "app", study_info = "studies.csv",
    mapping = list(study_label = "study", es_field = "yi", variance = "vi", es_id = "id", es_type = "SMD", url = "doi"))
  json <- file.path(dir, "config.json")
  jsonlite::write_json(config, json, auto_unbox = TRUE)
  build_app(json)
  expect_identical(readRDS(file.path(dir, "app", "study_info.rds"))$title, "Study B")
  config$schema_version <- 1
  jsonlite::write_json(config, json, auto_unbox = TRUE)
  expect_error(build_app(json), "schema_version 2")
})

test_that("build_app can run the lookup into a cache CSV beside the configuration", {
  dir <- tempfile("metaui-config-"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  calls <- character()
  local_mocked_bindings(metaUI_http_get = function(url, headers) {
    calls <<- c(calls, url)
    crossref_json("10.1234/a", licence = "https://creativecommons.org/licenses/by-nc/4.0/")
  })
  x <- fixture(); x$doi <- ifelse(x$study == "B", "10.1234/a", NA)
  utils::write.csv(x, file.path(dir, "data.csv"), row.names = FALSE)
  config <- list(schema_version = 2, data = "data.csv", dataset_name = "Config", output = "app",
    study_info = list(cache = "study-metadata.csv"),
    mapping = list(study_label = "study", es_field = "yi", variance = "vi", es_id = "id", es_type = "SMD", url = "doi"))
  json <- file.path(dir, "config.json")
  jsonlite::write_json(config, json, auto_unbox = TRUE)
  suppressMessages(build_app(json))
  stored <- readRDS(file.path(dir, "app", "study_info.rds"))
  expect_identical(stored$study, "B")
  expect_identical(stored$abstract_licence, "https://creativecommons.org/licenses/by-nc/4.0/") # NC kept by default
  expect_true(file.exists(file.path(dir, "study-metadata.csv")))
  expect_length(calls, 1)
  rebuild <- function() { unlink(file.path(dir, "app"), recursive = TRUE); suppressMessages(build_app(json)) }
  rebuild()
  expect_length(calls, 1) # rebuilt from the cache
  config$study_info <- list(cache = "study-metadata.csv", abstracts = "exclude_nc", openalex = FALSE)
  jsonlite::write_json(config, json, auto_unbox = TRUE)
  rebuild()
  expect_length(calls, 2)
  expect_true(is.na(readRDS(file.path(dir, "app", "study_info.rds"))$abstract))
  for (bad in list(list(abstracts = "open_licence"), list(cache = "x.csv", abstracts = "some"), list(cache = "x.csv", colour = "red"))) {
    config$study_info <- bad
    jsonlite::write_json(config, json, auto_unbox = TRUE)
    expect_error(build_app(json), "lookup object")
  }
})
