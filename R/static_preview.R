# Work only on the export copy: author edits to ui.R/server.R are evaluated here.
static_precompute <- function(app_dir, dest) {
  old <- setwd(app_dir)
  on.exit(setwd(old), add = TRUE)
  env <- new.env(parent = globalenv())
  sys.source("global.R", env)
  defaults <- list()
  # Read defaults from the actual UI, including author-edited selections.
  inputs <- function(tag) {
    if (inherits(tag, "shiny.tag")) {
      a <- tag$attribs
      if (tag$name == "input") {
        id <- if (!is.null(a$name)) a$name else a$id
        if (!is.null(id)) {
          if (!is.null(a$`data-from`)) defaults[[id]] <<- as.numeric(c(a$`data-from`, a$`data-to`))
          else if (identical(a$type, "checkbox")) {
            if (!is.null(a$name)) {
              if (is.null(defaults[[id]])) defaults[[id]] <<- character()
              if (!is.null(a$checked)) defaults[[id]] <<- c(defaults[[id]], a$value)
            } else defaults[[id]] <<- !is.null(a$checked)
          } else if (identical(a$type, "number")) defaults[[id]] <<- suppressWarnings(as.numeric(a$value))
        }
      }
      if (tag$name == "select" && !is.null(a$id)) {
        options <- list()
        collect <- function(x) {
          if (inherits(x, "shiny.tag") && x$name == "option")
            options[[length(options) + 1L]] <<- list(value = x$attribs$value, selected = !is.null(x$attribs$selected))
          # shinyWidgets::pickerInput() supplies its options as one HTML string.
          else if (is.character(x)) for (o in regmatches(x, gregexpr("<option\\b[^>]*>", x))[[1]])
            options[[length(options) + 1L]] <<- list(
              value = static_unescape(sub('.*\\svalue="([^"]*)".*', "\\1", o)),
              selected = grepl("\\sselected(\\s|=|>)", o))
          else if (is.list(x)) lapply(if (inherits(x, "shiny.tag")) x$children else x, collect)
        }
        collect(tag$children)
        selected <- Filter(function(x) x$selected, options)
        if (!length(selected) && is.null(a$multiple)) selected <- utils::head(options, 1)
        defaults[[a$id]] <<- vapply(selected, function(x) x$value, character(1))
      }
      lapply(tag$children, inputs)
    } else if (is.list(tag)) lapply(tag, inputs)
    invisible(NULL)
  }
  z <- env$metaUI_slider_input("outliers_z_scores", "z-score range",
    env$metaUI_slider_spec(env$metaUI__df$metaUI__es_z, integer = FALSE))
  inputs(env$ui); inputs(z)
  captured <- list()
  session <- output <- NULL # Bound by testServer inside its expression.
  # Test sessions report every output as 600px wide at 1x. Plots render at 2x for
  # sharp phone and high-density screens, and phones get a second rendering at their
  # column width, reusing the cached fits.
  plot_width <- 600
  for (accessor in c("$", "[["))
    registerS3method(accessor, "metaui_preview_clientdata", function(x, name)
      if (grepl("^output_.+_width$", name)) plot_width else if (identical(name, "pixelratio")) 2 else NextMethod())
  shiny::testServer(env$server, {
    session$clientData <- structure(session$clientData,
      class = c("metaui_preview_clientdata", class(session$clientData)))
    do.call(session$setInputs, defaults)
    session$setInputs(go = 1)
    for (id in c("selection_overview", "selection_status", "primary_estimate", "estimate_gaps",
                 "scale_note", "effectestimate", "sample", "model_comparison"))
      captured[[id]] <<- output[[id]]
    plot_width <<- 360
    session$setInputs(go = 2)
    captured$model_comparison_phone <<- output$model_comparison
  })
  if (is.null(captured$primary_estimate) || !nzchar(captured$effectestimate))
    stop("The generated server did not render default results.")
  transform <- function(tag) {
    if (inherits(tag, "shiny.tag")) {
      a <- tag$attribs
      if (tag$name == "script") return(NULL)
      if (!is.null(a$class)) {
        classes <- strsplit(a$class, "[[:space:]]+")[[1]]
        if (any(classes %in% c("shiny-spinner-placeholder", "load-container"))) return(NULL)
        tag$attribs$class <- paste(classes[classes != "shiny-spinner-hideui"], collapse = " ")
      }
      if (!is.null(a$`data-display-if`)) {
        if (a$`data-display-if` %in% c("!(input.go > 0)", "false")) return(NULL)
        tag$attribs$style <- NULL
      }
      if (!is.null(a$id) && a$id == "z_score_filter") tag$children <- list(z)
      if (!is.null(a$id) && a$id %in% names(captured)) {
        value <- captured[[a$id]]
        if (a$id == "model_comparison") {
          tag$children <- list(htmltools::tags$picture(
            htmltools::tags$source(media = "(max-width: 767px)", srcset = captured$model_comparison_phone$src),
            htmltools::tags$img(src = value$src, width = value$width,
              alt = "Model estimates and 95% intervals for the authors' full dataset",
              style = "max-width:100%;height:auto")))
          tag$attribs$style <- "width:100%;height:auto"
        } else {
          if (is.list(value)) value <- value$html
          if (is.null(value)) value <- ""
          if (a$id %in% c("selection_overview", "selection_status")) value <- htmltools::htmlEscape(value)
          else value <- static_stack_table(value)
          tag$children <- list(htmltools::HTML(value))
        }
      }
      if (identical(a$id, "go")) {
        tag$children <- list("Loading\u2026")
        tag$attribs$`aria-busy` <- "true"
        tag$attribs$`aria-disabled` <- "true"
      }
      if (tag$name %in% c("input", "button", "select", "textarea")) tag$attribs$disabled <- NA
      if (tag$name == "a" && !identical(a$class, "metaui-skip")) {
        tag$attribs$href <- NULL; tag$attribs$`aria-disabled` <- "true"; tag$attribs$tabindex <- "-1"
      }
      tag$children <- lapply(tag$children, transform)
    } else if (inherits(tag, "html_dependency")) return(tag)
    else if (is.list(tag)) tag[] <- lapply(tag, transform)
    tag
  }
  final_dest <- dest
  dest <- tempfile("metaui-preview-assets-")
  dir.create(dest, recursive = TRUE)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)
  file.copy(list.files("www", full.names = TRUE), dest, recursive = TRUE)
  # Shiny resource URLs (not html dependencies), including shinythemes CSS.
  for (name in names(shiny::resourcePaths())) {
    dir.create(file.path(dest, name), showWarnings = FALSE)
    file.copy(list.files(shiny::resourcePaths()[[name]], full.names = TRUE),
      file.path(dest, name), recursive = TRUE)
  }
  # Optional web-font imports must not block first paint or require the network.
  for (css in list.files(dest, pattern = "\\.css$", recursive = TRUE, full.names = TRUE)) {
    content <- paste(readLines(css, warn = FALSE), collapse = "\n")
    clean <- gsub("@import[^;]*https?://[^;]*;", "", content, perl = TRUE)
    if (!identical(content, clean)) writeLines(clean, css)
  }
  jsonlite::write_json(list(date = as.character(Sys.Date()), R = R.version.string,
    metaUI = as.character(utils::packageVersion("metaUI")),
    packages = as.data.frame(utils::installed.packages()[, c("Package", "Version")]),
    basis = "Authors' full built dataset; generated server with default inputs and go = 1"),
    file.path(dest, "provenance.json"), pretty = TRUE, auto_unbox = TRUE)
  htmltools::save_html(transform(env$ui), file.path(dest, "index.html"), libdir = "lib")
  # Dependencies include Shiny's scripts: the preview is deliberately script-free.
  path <- file.path(dest, "index.html")
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  html <- gsub("(?s)<script\\b[^>]*>.*?</script>", "", html, perl = TRUE)
  # Author HTML snippets can also contain controls (for example filter help).
  html <- gsub("<(input|button|select|textarea)\\b(?![^>]*\\sdisabled(?:\\s|=|/?>))",
    "<\\1 disabled", html, perl = TRUE)
  html <- sub("<head>", '<head><meta name="viewport" content="width=device-width, initial-scale=1"/>', html, fixed = TRUE)
  writeLines(html, path)
  files <- c(static_preview_files(dest), "provenance.json")
  for (file in files) {
    to <- file.path(final_dest, file)
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(dest, file), to, overwrite = TRUE)
  }
  invisible(captured)
}

static_add_preview <- function(target, preview) {
  file.copy(preview, target, recursive = TRUE)
  path <- file.path(target, "index.html")
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  controller <- readLines(system.file("template_code", "metaui-preview.js", package = "metaUI"))
  injection <- paste0('<script>', paste(controller, collapse = "\n"), '</script>')
  writeLines(sub('<body>', paste0('<body>\n', injection), html, fixed = TRUE), path)
}

# Follow URLs from the document and recursively from stylesheets. Query strings
# and fragments identify resources within a file, not separate files on disk.
static_preview_files <- function(root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  matches <- function(pattern, text) {
    found <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]
    if (!length(found)) return(character())
    sub(pattern, "\\1", found, perl = TRUE)
  }
  resolve <- function(urls, parent) {
    urls <- trimws(urls)
    urls <- urls[nzchar(urls) & !grepl("^(data:|#)", urls, ignore.case = TRUE)]
    if (any(grepl("^([a-z][a-z0-9+.-]*:|//)", urls, ignore.case = TRUE)))
      stop("Preview assets must be local: ", paste(urls, collapse = ", "))
    urls <- vapply(urls, function(x) utils::URLdecode(sub("[?#].*$", "", x)), character(1))
    referenced <- file.path(dirname(parent), urls)
    if (any(!file.exists(referenced)))
      stop("Missing preview asset: ", paste(urls[!file.exists(referenced)], collapse = ", "))
    paths <- normalizePath(referenced, winslash = "/", mustWork = TRUE)
    if (any(!startsWith(paths, paste0(root, "/")))) stop("Preview asset escapes its folder")
    paths
  }
  pending <- file.path(root, "index.html")
  seen <- character()
  while (length(pending)) {
    path <- pending[1]; pending <- pending[-1]
    if (path %in% seen) next
    seen <- c(seen, path)
    if (!grepl("\\.(html|css)$", path)) next
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    # CSS comments may contain example URLs that are not dependencies.
    text <- gsub("(?s)/\\*.*?\\*/", "", text, perl = TRUE)
    urls <- matches("url\\(\\s*[\"']?([^\"')]+)[\"']?\\s*\\)", text)
    if (endsWith(path, ".html"))
      urls <- c(urls, matches("src=[\"']([^\"']+)[\"']", text),
        matches("<link\\b[^>]*href=[\"']([^\"']+)[\"']", text))
    else
      urls <- c(urls, matches("@import\\s+[\"']([^\"']+)[\"']", text))
    pending <- unique(c(pending, resolve(urls, path)))
  }
  substring(seen, nchar(root) + 2L)
}

# Attribute value from htmltools-escaped HTML; whitespace is kept as factor levels may carry it.
static_unescape <- function(x) {
  x <- gsub("&lt;", "<", gsub("&gt;", ">", gsub("&quot;|&#34;", "\"", gsub("&#39;|&apos;", "'", x))))
  for (code in unique(regmatches(x, gregexpr("&#x?[0-9A-Fa-f]+;", x))[[1]])) {
    n <- sub("^&#(x?)([0-9A-Fa-f]+);$", "\\2", code)
    x <- gsub(code, intToUtf8(if (grepl("^&#x", code)) strtoi(n, 16L) else as.integer(n)), x, fixed = TRUE)
  }
  gsub("&amp;", "&", x)
}

# The preview has no scripts, so tables get the phone card labels that metaui.js adds
# in the live app: the same rule (four or more columns, named first column) in R.
static_stack_table <- function(html) {
  head <- regmatches(html, regexpr("(?s)<thead\\b.*?</thead>", html, perl = TRUE))
  body_at <- regexpr("(?s)<tbody\\b.*?</tbody>", html, perl = TRUE)
  if (!length(head) || body_at < 0) return(html)
  cells <- regmatches(head, gregexpr("(?s)<th\\b[^>]*>.*?</th>", head, perl = TRUE))[[1]]
  names <- trimws(gsub("[[:space:]]+", " ", static_unescape(gsub("<[^>]+>", "", cells))))
  if (length(names) < 4L) return(html)
  body <- regmatches(html, body_at)
  rows <- regmatches(body, gregexpr("(?s)<tr\\b.*?</tr>", body, perl = TRUE))[[1]]
  first <- character()
  labelled <- vapply(rows, function(row) {
    starts <- gregexpr("<td\\b", row)[[1]]
    if (length(starts) != length(names) || starts[1] < 0) return(row)
    parts <- regmatches(row, list(starts))[[1]]
    first <<- c(first, trimws(static_unescape(gsub("<[^>]+>", "", sub("(?s)^<td\\b[^>]*>(.*?)</td>.*", "\\1",
      substring(row, starts[1]), perl = TRUE)))))
    contents <- sub("(?s)^<td\\b[^>]*>(.*?)</td>.*", "\\1", substring(row, starts), perl = TRUE)
    empty <- !nzchar(trimws(gsub("<[^>]+>", "", contents))) & !grepl("<(img|svg|button|input)\\b", contents)
    attrs <- paste0('<td data-label="', htmltools::htmlEscape(names, attribute = TRUE), '"',
      ifelse(empty, " data-empty", ""))
    regmatches(row, list(starts)) <- list(paste0(attrs, substring(parts, 4L)))
    row
  }, character(1), USE.NAMES = FALSE)
  for (i in seq_along(rows)) body <- sub(rows[i], labelled[i], body, fixed = TRUE)
  numeric <- grepl("^[[:space:][:digit:].,%<>=+()\\[\\]−-]*$|^NA$", first)
  class <- paste0("metaui-stack", if (length(first) && !any(numeric)) " metaui-stack-named")
  html <- paste0(substring(html, 1L, body_at - 1L), body, substring(html, body_at + attr(body_at, "match.length")))
  sub("<table\\b([^>]*?)class=\"([^\"]*)\"", paste0("<table\\1class=\"\\2 ", class, "\""),
    if (grepl("<table\\b[^>]*class=", html)) html else sub("<table\\b", "<table class=\"\"", html), perl = TRUE)
}
