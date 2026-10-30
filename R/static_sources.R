# Verified upstream forks/tags for the exact webR builds, including permissive
# packages so that the exported inventory records their provenance too.
static_patched_sources <- function() {
  c("shiny_1.13.0.8000" = "https://github.com/r-wasm/shiny/tree/v1.13.0.8000",
    "httpuv_1.6.6.9000" = "https://github.com/r-wasm/httpuv/tree/5558cf7aa7dbc840ce2e2c52ed9e473075f002a3",
    "later_1.4.2.9000" = "https://github.com/r-wasm/later/tree/25181f652249585525686b90382192d2adb2acb2",
    "vroom_1.7.1.9000" = "https://github.com/r-wasm/vroom/tree/d0640b28f49e44bd70523ec035e549e69d3d8067",
    "sass_0.4.10.9000" = "https://github.com/rstudio/sass/tree/a8d149b69f90fbed1e2d45e6e9bcc3c6ee2122ad")
}

static_package_source <- function(package, version) {
  if (!grepl("\\.(8|9)[0-9]{3}$", version))
    return(paste0("https://cran.r-project.org/package=", package))
  source <- static_patched_sources()[paste(package, version, sep = "_")]
  if (is.na(source)) "webR-patched build: source not recorded" else unname(source)
}

static_webr_version <- function(webr_dir) {
  js <- paste(readLines(file.path(webr_dir, "webr.mjs"), warn = FALSE), collapse = "\n")
  match <- regexpr("https://webr\\.r-wasm\\.org/v[^/\"]+/", js)
  if (match[1] == -1L) stop("Cannot determine webR version from bundled webr.mjs")
  sub("/$", "", sub(".*/v", "", regmatches(js, match)))
}

static_source_plan <- function(bundled, r_version, webr_version) {
  copyleft <- grepl("GPL|MPL", bundled$license, ignore.case = TRUE)
  packages <- bundled[copyleft & !duplicated(bundled[c("package", "version")]), , drop = FALSE]
  plan <- lapply(seq_len(nrow(packages)), function(i) {
    p <- packages[i, ]
    filename <- paste0(p$package, "_", p$version, ".tar.gz")
    if (grepl("\\.(8|9)[0-9]{3}$", p$version)) {
      source <- static_package_source(p$package, p$version)
      urls <- if (startsWith(source, "https://github.com/"))
        paste0(sub("/tree/", "/archive/", source, fixed = TRUE), ".tar.gz") else character()
    } else {
      urls <- paste0("https://cloud.r-project.org/src/contrib/",
        c("", paste0("Archive/", p$package, "/")), filename)
    }
    list(package = p$package, file = filename, urls = urls, license = p$license)
  })
  c(plan, list(
    list(package = "R", file = paste0("R-", r_version, ".tar.gz"),
      urls = paste0("https://cloud.r-project.org/src/base/R-", strsplit(r_version, ".", fixed = TRUE)[[1]][1],
        "/R-", r_version, ".tar.gz"), license = "GPL-2 | GPL-3"),
    list(package = "webR", file = paste0("webr-", webr_version, ".tar.gz"),
      urls = paste0("https://github.com/r-wasm/webr/archive/refs/tags/v", webr_version, ".tar.gz"), license = "GPL-3")))
}

static_include_sources <- function(bundled, webr_dir, dest, downloader = utils::download.file) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  unknown <- vapply(seq_len(nrow(bundled)), function(i)
    grepl("source not recorded", static_package_source(bundled$package[i], bundled$version[i]), fixed = TRUE), logical(1))
  for (package in bundled$package[unknown]) warning("Source not recorded for patched package ", package, call. = FALSE)
  plan <- static_source_plan(bundled, static_r_version(webr_dir), static_webr_version(webr_dir))
  manifest_path <- file.path(dest, "README.md")
  manifest <- if (file.exists(manifest_path)) readLines(manifest_path, warn = FALSE) else
    c("# Bundled software sources", "", "File | Origin URL | Licence", "--- | --- | ---")
  entry <- function(item, url) paste(item$file, url,
    gsub("|", "\\|", item$license, fixed = TRUE), sep = " | ")
  for (item in plan) {
    target <- file.path(dest, item$file)
    # Existing archives and their recorded origins survive multi-app exports.
    if (file.exists(target)) {
      if (!any(startsWith(manifest, paste0(item$file, " | "))))
        manifest <- c(manifest, entry(item, paste(item$urls, collapse = " or ")))
      next
    }
    success <- FALSE
    for (url in item$urls) {
      tmp <- tempfile("source-", tmpdir = dest)
      status <- tryCatch(suppressWarnings(downloader(url, tmp, mode = "wb", quiet = TRUE)), error = function(e) 1L)
      success <- identical(as.integer(status), 0L) && file.exists(tmp) && file.size(tmp) > 0
      if (success) success <- file.rename(tmp, target)
      unlink(tmp)
      if (success) {
        manifest <- c(manifest, entry(item, url))
        break
      }
    }
    if (!success && length(item$urls)) warning("Source download failed for ", item$package, call. = FALSE)
  }
  writeLines(manifest, manifest_path)
  invisible(plan)
}
