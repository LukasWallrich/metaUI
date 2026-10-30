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

test_that("Crossref records become plain-text metadata; abstracts follow the licence rule", {
  today <- "2026-10-30"
  parse <- function(...) metaUI:::metaUI_parse_crossref(jsonlite::fromJSON(crossref_json("10.1/x", ...), simplifyVector = FALSE)$message, "open_licence", today)
  open <- parse(licence = "http://creativecommons.org/licenses/by/4.0/")
  expect_identical(open$title, "A tidy title & more")
  expect_identical(open$authors, "Lovelace, Ada; Turing, Alan; Study Group")
  expect_identical(c(open$year, open$journal, open$volume, open$issue, open$pages), c("2021", "Journal of Tests", "4", "2", "1-9"))
  expect_identical(open$abstract, "First <para>.\n\nSecond para – end.")
  expect_identical(open$abstract_licence, "http://creativecommons.org/licenses/by/4.0/")
  # Withheld: no licence, a publisher/TDM licence, NC terms, an embargo, or a licence for another version.
  for (args in list(list(), list(licence = "https://www.elsevier.com/tdm/userlicense/1.0/"),
                    list(licence = "https://creativecommons.org/licenses/by-nc/4.0/"),
                    list(licence = "https://creativecommons.org/licenses/by/4.0/", start = "2030-01-01T00:00:00Z"),
                    list(licence = "https://creativecommons.org/licenses/by/4.0/", version = "tdm"))) {
    closed <- do.call(parse, args)
    expect_true(is.na(closed$abstract))
    expect_match(closed$abstract_note, "no open licence")
    expect_identical(closed$title, "A tidy title & more")
  }
  expect_false(is.na(parse(licence = "https://creativecommons.org/publicdomain/zero/1.0/")$abstract))
  expect_match(parse(licence = NULL, abstract = FALSE)$abstract_note, "No abstract deposited")
  all <- metaUI:::metaUI_parse_crossref(jsonlite::fromJSON(crossref_json("10.1/x"), simplifyVector = FALSE)$message, "all", today)
  expect_false(is.na(all$abstract))
  expect_match(all$abstract_note, "Included by the app authors")
})

test_that("fetch_study_metadata reports unmatched DOIs, never guesses, and reuses its cache", {
  calls <- character()
  local_mocked_bindings(metaUI_http_get = function(url, agent) {
    calls <<- c(calls, url)
    expect_match(agent, "mailto:reader@example.org", fixed = TRUE)
    if (grepl("missing", url)) stop("cannot open URL: HTTP status was '404 Not Found'")
    if (grepl("offline", url)) stop("Could not resolve host")
    crossref_json(utils::URLdecode(sub("^.*/works/([^?]*).*$", "\\1", url)), licence = "https://creativecommons.org/licenses/by/4.0/")
  })
  x <- data.frame(study = c("A", "A", "B", "C", "D", "E", "F", "G", "G"),
    doi = c("https://doi.org/10.1234/ABC", "10.1234/abc", "doi:10.1234/missing", "not a doi", NA,
            "10.1234/offline", "10.1234/abc", "10.1/one", "10.1/two"))
  cache <- tempfile(fileext = ".csv"); on.exit(unlink(cache))
  messages <- capture_messages(info <- fetch_study_metadata(x, mailto = "reader@example.org", cache = cache, pause = 0))
  expect_match(messages[1], "2 of 7 studies matched in Crossref; 2 abstracts kept", fixed = TRUE)
  expect_match(messages[2], "B [not_found] 10.1234/missing", fixed = TRUE)
  expect_identical(info$study, c("A", "B", "C", "D", "E", "F", "G"))
  expect_identical(info$status, c("found", "not_found", "invalid_doi", "no_doi", "error", "found", "conflicting_dois"))
  expect_identical(info$doi[1:2], c("10.1234/abc", "10.1234/missing"))
  expect_true(all(is.na(info$title[-c(1, 6)])))
  expect_length(calls, 3) # one per distinct valid DOI; shared DOIs queried once
  expect_match(calls[1], "mailto=reader%40example.org", fixed = TRUE)
  calls <- character()
  again <- suppressMessages(fetch_study_metadata(x, mailto = "reader@example.org", cache = cache, pause = 0))
  expect_length(calls, 2) # found DOI reused; failures retried
  expect_identical(again$title, info$title)
  suppressMessages(fetch_study_metadata(x, mailto = "reader@example.org", cache = cache, abstracts = "none", pause = 0))
  expect_length(calls, 5) # another abstract policy needs fresh records
  expect_error(fetch_study_metadata(prepared()), "no url mapping")
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
  expect_identical(env$metaUI_study_reference_text[1], "Doe, Jane et al. (2020). <script>alert(1)</script> Title.")
  cells <- env$metaUI_study_cells(c("A", "B", "C"), c("A", "B", "C"), env$metaUI_study_info,
    env$metaUI_study_details_html, env$metaUI_study_reference_text)
  expect_match(cells$label[1], '^<button type="button" class="metaui-study-toggle" aria-expanded="false" title="Doe, Jane et al. \\(2020\\). &lt;script&gt;')
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
