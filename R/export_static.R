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
#' Readers download about 140 MB and wait roughly 40 seconds before the app is
#' ready (measured on a 2017 quad-core laptop); models fit several times
#' more slowly than on a server. The dataset is shipped to every reader, so only
#' export data that may be published.
#'
#' @param app_dir Folder of a generated metaUI app.
#' @param dest_dir Folder for the static site. Must not exist unless `overwrite = TRUE`.
#' @param subdir Optional subfolder of `dest_dir` for this app, so that several
#'   apps can share one set of WebAssembly assets.
#' @param overwrite Replace an existing app in `dest_dir`/`subdir`?
#' @return The path of the exported app, invisibly.
#' @export
#' @examples
#' \dontrun{
#' export_static("my-app", "my-app-static")
#' # Preview locally (service workers need https or localhost):
#' httpuv::runStaticServer("my-app-static")
#' }
export_static <- function(app_dir, dest_dir, subdir = "", overwrite = FALSE) {
  if (!requireNamespace("shinylive", quietly = TRUE))
    stop("export_static() requires the shinylive package: install.packages(\"shinylive\").")
  if (!all(file.exists(file.path(app_dir, c("global.R", "ui.R", "server.R", "www/metaui.js")))))
    stop("app_dir must be a folder created by generate_shiny() or build_app().")
  target <- if (nzchar(subdir)) file.path(dest_dir, subdir) else dest_dir
  if (dir.exists(target) && length(list.files(target))) {
    if (!overwrite) stop("Destination exists: ", target, ". Use overwrite = TRUE to replace it.")
    unlink(target, recursive = TRUE)
  }

  staging <- tempfile("metaui-static-")
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  file.copy(list.files(app_dir, full.names = TRUE, all.files = FALSE), staging, recursive = TRUE)
  bridge <- readLines(system.file("template_code", "metaui-static.js", package = "metaUI"))
  cat(c("", bridge), file = file.path(staging, "www", "metaui.js"), sep = "\n", append = TRUE)

  shinylive::export(staging, dest_dir, subdir = subdir, wasm_packages = TRUE)
  bundled <- static_bundled_packages(file.path(dest_dir, "shinylive", "webr"))
  utils::write.csv(bundled, file.path(target, "bundled-packages.csv"), row.names = FALSE)
  writeLines(static_notice(bundled), file.path(target, "STATIC-NOTICE.md"))
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
    patched <- grepl("\\.(8|9)[0-9]{3}$", desc[, "Version"])
    data.frame(package = desc[, "Package"], version = desc[, "Version"], license = desc[, "License"],
      location = location, source = if (patched) "webR-patched build: source location not yet recorded" else
        paste0("https://cran.r-project.org/package=", desc[, "Package"]))
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
static_notice <- function(bundled) {
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
    "- shinylive web assets (Posit, PBC; MIT licence): https://github.com/posit-dev/shinylive",
    "- webR, a WebAssembly build of R (webR binaries: GNU GPL version 3; R: GNU GPL 2 or 3):",
    "  https://github.com/r-wasm/webr",
    paste0("- ", nrow(bundled), " R packages built for WebAssembly by https://repo.r-wasm.org,",
      " listed with versions and licences in `bundled-packages.csv`. Source code for CRAN",
      " versions is available from CRAN (current and archived releases) at the listed URL;",
      " webR-patched builds are marked as such."),
    "",
    "The About tab lists package versions on the authoring computer. The versions that",
    "run in the browser can differ and are listed in `bundled-packages.csv`.",
    "",
    "The app code itself (ui.R, server.R, and the other R files) is part of this export and",
    "carries the licence stated in its COPYRIGHTS file.")
}
