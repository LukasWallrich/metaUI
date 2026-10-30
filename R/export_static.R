#' Export a generated app as a static website (experimental)
#'
#' Converts an app folder written by [generate_shiny()] or [build_app()] into
#' static files that run R in the reader's browser through 'shinylive'
#' (WebAssembly R). The result can be served by any static web host that
#' supports HTTPS, such as GitHub Pages, or opened from `localhost`; no Shiny
#' server is needed.
#'
#' The app folder is left unchanged. The export appends a small script to the
#' copied `www/metaui.js` so that selection links and downloads work inside the
#' shinylive frame, bundles WebAssembly builds of all required R packages, and
#' writes `STATIC-NOTICE.md` and `bundled-packages.csv` with the versions and
#' licences of the bundled software.
#'
#' By default, the generated server computes the initial Summary at export time.
#' Readers see these results with disabled controls while the interactive app loads.
#' Selection links skip the preview. Export provenance is stored in
#' `metaui-preview/provenance.json`. Readers download about 140 MB; models fit
#' more slowly than on a server. The dataset is shipped to every reader, so only
#' export data that may be published.
#'
#' When apps share `dest_dir`, their package inventories, notices and About
#' tables describe the shared runtime: they can include packages used only by
#' another app. They are inventories of available software, not proof that each
#' app uses every package. `include_sources` collects the union of copyleft
#' packages in the shared runtime, preserving earlier source archives.
#'
#' @param app_dir Folder of a generated metaUI app.
#' @param dest_dir Folder for the static site. Must not exist unless `overwrite = TRUE`.
#' @param subdir Optional subfolder of `dest_dir` for this app, so that several
#'   apps can share one set of WebAssembly assets.
#' @param overwrite Replace an existing app in `dest_dir`/`subdir`? A nonempty
#'   target must contain `shinylive-sw.js`, or both `app.json` and `index.html`,
#'   identifying a previous export. Otherwise use an empty folder. Replacement
#'   deletes the entire target, including hand edits and unrelated files. With
#'   `subdir`, only that app subfolder is deleted; without it, all of `dest_dir`
#'   is deleted and rewritten. Keep hand edits in the editable app folder.
#' @param precompute Show precomputed default Summary results while the interactive app loads?
#'   Failures warn and export the interactive app alone. No browser is required.
#' @param include_sources Bundle exact copyleft package, R and webR source archives
#'   in `dest_dir/sources/`? Recommended for archival deposits such as Zenodo.
#'   Live sites can use the source links in `bundled-packages.csv` instead.
#' @return The path of the exported app, invisibly.
#' @export
#' @examples
#' \dontrun{
#' export_static("my-app", "my-app-static")
#' # Preview locally (service workers need https or localhost):
#' httpuv::runStaticServer("my-app-static")
#' }
export_static <- function(app_dir, dest_dir, subdir = "", overwrite = FALSE, precompute = TRUE, include_sources = FALSE) {
  if (!requireNamespace("shinylive", quietly = TRUE))
    stop("export_static() requires the shinylive package: install.packages(\"shinylive\").")
  if (!all(file.exists(file.path(app_dir, c("global.R", "ui.R", "server.R", "www/metaui.js")))))
    stop("app_dir must be a folder created by generate_shiny() or build_app().")
  if (!is.character(subdir) || length(subdir) != 1L || is.na(subdir))
    stop("subdir must be a scalar string.")
  dest_dir <- metaUI_canonical_path(dest_dir)
  target <- metaUI_canonical_path(if (nzchar(subdir)) file.path(dest_dir, subdir) else dest_dir)
  if (nzchar(subdir) &&
      (grepl("^(/|[A-Za-z]:|\\\\)", subdir) || identical(target, dest_dir) ||
       !metaUI_paths_overlap(dest_dir, target) ||
       !startsWith(target, paste0(sub("/+$", "", dest_dir), "/"))))
    stop("subdir must identify a folder strictly inside dest_dir.")
  # Pass the validated canonical destination to shinylive as well.
  subdir <- if (nzchar(subdir)) substring(target, nchar(sub("/+$", "", dest_dir)) + 2L) else ""
  # Never delete or write into the source app: reject overlapping folders first.
  if (metaUI_paths_overlap(app_dir, target) ||
      startsWith(paste0(metaUI_canonical_path(dest_dir), "/"), paste0(sub("/+$", "", metaUI_canonical_path(app_dir)), "/")))
    stop("dest_dir (and subdir) must be separate from app_dir: not equal to, inside or containing it.")
  validate_static_destination(target, overwrite)
  if (dir.exists(target) && length(list.files(target, all.files = TRUE, no.. = TRUE)))
    unlink(target, recursive = TRUE)

  staging <- tempfile("metaui-static-")
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  file.copy(list.files(app_dir, full.names = TRUE, all.files = FALSE), staging, recursive = TRUE)
  bridge <- readLines(system.file("template_code", "metaui-static.js", package = "metaUI"))
  cat(c("", bridge), file = file.path(staging, "www", "metaui.js"), sep = "\n", append = TRUE)

  preview <- file.path(staging, "metaui-preview")
  if (isTRUE(precompute)) {
    tryCatch(static_precompute(staging, preview), error = function(e) {
      unlink(preview, recursive = TRUE)
      warning("Precomputation failed; exporting without a preview: ", conditionMessage(e), call. = FALSE)
    })
  }
  # Preview assets are website files, not files for webR's virtual filesystem.
  preview_copy <- tempfile("metaui-preview-")
  dir.create(preview_copy)
  on.exit(unlink(preview_copy, recursive = TRUE), add = TRUE)
  has_preview <- dir.exists(preview)
  if (has_preview) {
    file.copy(preview, preview_copy, recursive = TRUE)
    unlink(preview, recursive = TRUE)
  }
  shinylive::export(staging, dest_dir, subdir = subdir, wasm_packages = TRUE)
  if (has_preview) static_add_preview(target, file.path(preview_copy, "metaui-preview"))
  bundled <- static_bundled_packages(file.path(dest_dir, "shinylive", "webr"))
  r_version <- static_r_version(file.path(dest_dir, "shinylive", "webr"))
  app_json <- file.path(target, "app.json")
  files <- jsonlite::read_json(app_json)
  files <- lapply(files, function(f) {
    if (identical(f$name, "ui.R")) f$content <- static_rewrite_about(f$content, bundled, r_version)
    f
  })
  jsonlite::write_json(files, app_json, auto_unbox = TRUE)
  if (has_preview) {
    path <- file.path(target, "metaui-preview", "index.html")
    writeLines(static_rewrite_about(paste(readLines(path, warn = FALSE), collapse = "\n"),
      bundled, r_version), path)
  }
  utils::write.csv(bundled, file.path(target, "bundled-packages.csv"), row.names = FALSE)
  if (isTRUE(include_sources))
    tryCatch(static_include_sources(bundled, file.path(dest_dir, "shinylive", "webr"),
      file.path(dest_dir, "sources")), error = function(e)
        warning("Source collection incomplete: ", conditionMessage(e), call. = FALSE))
  writeLines(static_notice(bundled, include_sources, static_sources_path(subdir)), file.path(target, "STATIC-NOTICE.md"))
  invisible(normalizePath(target))
}

#' Versions and licences of the R packages in a shinylive export
#'
#' Reads app packages from `webr/packages/*/*.tgz` and the packages preinstalled
#' in shinylive's webR library (`library.data.gz`, indexed by `library.js.metadata`).
#' @noRd
static_bundled_packages <- function(webr_dir) {
  row <- function(lines, location) {
    desc <- read.dcf(textConnection(lines), fields = c("Package", "Version", "License"))
    # Versions ending .8000/.9000 are webR-patched builds, not CRAN releases.
    data.frame(package = desc[, "Package"], version = desc[, "Version"], license = desc[, "License"],
      location = location, source = static_package_source(desc[, "Package"], desc[, "Version"]))
  }
  archives <- list.files(file.path(webr_dir, "packages"), pattern = "\\.tgz$", recursive = TRUE, full.names = TRUE)
  rows <- lapply(archives, function(archive) {
    name <- basename(dirname(archive))
    tmp <- tempfile()
    on.exit(unlink(tmp, recursive = TRUE))
    utils::untar(archive, files = file.path(name, "DESCRIPTION"), exdir = tmp, tar = "internal")
    row(readLines(file.path(tmp, name, "DESCRIPTION"), warn = FALSE), "app packages")
  })
  index <- file.path(webr_dir, "library.js.metadata")
  if (file.exists(index)) {
    files <- jsonlite::read_json(index)$files
    files <- Filter(function(f) grepl("^/[^/]+/DESCRIPTION$", f$filename), files)
    data <- file.path(webr_dir, "library.data.gz")
    raw <- memDecompress(readBin(data, "raw", file.size(data)), "gzip")
    rows <- c(rows, lapply(files, function(f)
      row(strsplit(rawToChar(raw[(f$start + 1):f$end]), "\n")[[1]], "webR base library")))
  }
  out <- do.call(rbind, rows)
  out <- out[order(tolower(out$package), out$location), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Text of STATIC-NOTICE.md
#' @noRd
static_notice <- function(bundled, include_sources = FALSE, source_path = "sources/") {
  c("# Static (WebAssembly) metaUI app",
    "",
    "This folder runs a metaUI app entirely in the reader's browser. Serve the export",
    "folder that contains `shinylive-sw.js` over HTTPS or from localhost; for example, run",
    "`python3 -m http.server` there and open http://localhost:8000/ (plus this app's",
    "subfolder, if any). Opening index.html as a file does not work.",
    "",
    "**Data:** the full dataset is downloaded by every visitor. Publish only data that may be shared.",
    "",
    "## Bundled third-party software",
    "",
    "The inventory and About package table describe the shared runtime. When several apps",
    "share this export folder, they can include packages used only by another app.",
    "Bundled sources cover the union of copyleft packages in the shared runtime.",
    "",
    "- shinylive web assets (Posit, PBC; MIT licence): https://github.com/posit-dev/shinylive",
    "- webR, a WebAssembly build of R (webR binaries: GNU GPL version 3; R: GNU GPL 2 or 3):",
    "  https://github.com/r-wasm/webr",
    paste0("- ", nrow(bundled), " R packages built for WebAssembly by https://repo.r-wasm.org,",
      " listed with versions and licences in `bundled-packages.csv`. Source code for CRAN",
      " versions is available from CRAN (current and archived releases) at the listed URL;",
      " webR-patched builds are marked as such."),
    "",
    if (isTRUE(include_sources)) paste0("Corresponding sources are in `", source_path,
      "` (see its README for origins and licences). Downloads that failed are reported during export.") else
      "Package sources are linked in `bundled-packages.csv`; use include_sources = TRUE to bundle archives for deposits.",
    "",
    "The app code itself (ui.R, server.R, and the other R files) is part of this export and",
    "carries the licence stated in its COPYRIGHTS file.")
}

# translations ships with R itself; its version is the runtime version, rather
# than the R version used to build an arbitrary contributed package.
static_r_version <- function(webr_dir) {
  path <- file.path(webr_dir, "vfs", "usr", "lib", "R", "library", "translations", "DESCRIPTION")
  unname(read.dcf(path, fields = "Version")[1, 1])
}

# Works on both HTML and the HTML string embedded in generated ui.R.
static_rewrite_about <- function(text, bundled, r_version) {
  pattern <- "(?s)<h4>R packages used</h4><div class='metaui-packages'>.*?</div>"
  if (!grepl(pattern, text, perl = TRUE)) return(text)
  packages <- bundled[!duplicated(bundled$package), c("package", "version")]
  packages <- rbind(data.frame(package = "R", version = r_version), packages)
  rows <- paste0("<tr><td>", htmltools::htmlEscape(packages$package), "</td><td>",
    htmltools::htmlEscape(packages$version), "</td></tr>")
  groups <- split(rows, ceiling(seq_along(rows) / ceiling(length(rows) / 3)))
  tables <- vapply(groups, function(x) paste0("<table><tr><th>Package</th><th>Version</th></tr>",
    paste(x, collapse = ""), "</table>"), character(1))
  replacement <- paste0("<h4>R packages used</h4><div class='metaui-packages'>",
    "<p>Packages available in the bundled runtime; shared exports can include packages used only by another app.</p>",
    paste(tables, collapse = ""), "</div>")
  # Avoid replacement-string escape processing in author code.
  match <- regexpr(pattern, text, perl = TRUE)
  regmatches(text, match) <- replacement
  text
}

# target has already been canonicalised to a strict descendant of dest_dir.
static_sources_path <- function(subdir) {
  depth <- if (nzchar(subdir)) length(strsplit(subdir, "/", fixed = TRUE)[[1]]) else 0L
  paste0(paste(rep("../", depth), collapse = ""), "sources/")
}

# Run before generating sources or removing any existing site.
validate_static_destination <- function(target, overwrite) {
  if (!dir.exists(target) || !length(list.files(target, all.files = TRUE, no.. = TRUE)))
    return(invisible(NULL))
  if (!overwrite) stop("Destination exists: ", target, ". Use overwrite = TRUE to replace it.")
  previous_export <- file.exists(file.path(target, "shinylive-sw.js")) ||
    all(file.exists(file.path(target, c("app.json", "index.html"))))
  if (!previous_export)
    stop("Use an empty or previous-export folder for static output; refusing to delete: ", target)
  invisible(NULL)
}
