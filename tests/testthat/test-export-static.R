test_that("bundled-package table reads licences and flags webR-patched builds", {
  pkg_dir <- tempfile(); src_root <- tempfile(); old <- getwd()
  on.exit({setwd(old); unlink(c(pkg_dir, src_root), recursive = TRUE)})
  for (spec in list(c("alpha", "1.2.3", "MIT + file LICENSE"), c("beta", "2.0.0.9000", "GPL-3"))) {
    src <- file.path(src_root, spec[1])
    dir.create(src, recursive = TRUE)
    write.dcf(data.frame(Package = spec[1], Version = spec[2], License = spec[3]), file.path(src, "DESCRIPTION"))
    dir.create(file.path(pkg_dir, "packages", spec[1]), recursive = TRUE)
    old <- setwd(dirname(src))
    utils::tar(file.path(pkg_dir, "packages", spec[1], paste0(spec[1], ".tgz")), spec[1], compression = "gzip", tar = "internal")
    setwd(old)
  }
  bundled <- metaUI:::static_bundled_packages(pkg_dir)
  expect_equal(bundled$package, c("alpha", "beta"))
  expect_equal(bundled$license, c("MIT + file LICENSE", "GPL-3"))
  expect_equal(bundled$source[1], "https://cran.r-project.org/package=alpha")
  expect_match(bundled$source[2], "webR-patched")
  notice <- metaUI:::static_notice(bundled)
  expect_true(any(grepl("2 R packages", notice)))
  expect_true(any(grepl("downloaded by every visitor", notice)))
})

test_that("export_static() rejects folders that are not generated apps", {
  skip_if_not_installed("shinylive")
  expect_error(export_static(tempdir(), tempfile()), "created by generate_shiny")
})

test_that("export_static() refuses destinations that overlap the source app", {
  skip_if_not_installed("shinylive")
  root <- tempfile(); app <- file.path(root, "project", "app")
  on.exit(unlink(root, recursive = TRUE))
  dir.create(file.path(app, "www"), recursive = TRUE)
  for (f in c("global.R", "server.R", "ui.R", "www/metaui.js")) writeLines("", file.path(app, f))
  writeLines("keep", file.path(root, "project", "sibling.txt"))
  for (dest in list(list(app, ""), list(dirname(app), ""), list(file.path(app, "site"), ""),
                    list(dirname(dirname(app)), "project"), list(app, "site")))
    expect_error(export_static(app, dest[[1]], subdir = dest[[2]], overwrite = TRUE), "separate from app_dir")
  expect_true(file.exists(file.path(app, "global.R")))
  expect_true(file.exists(file.path(root, "project", "sibling.txt")))
})

test_that("export_static() writes a static site with the bridge and notices", {
  # Downloads ~100 MB of WebAssembly packages; run explicitly.
  skip_on_cran()
  skip_if(Sys.getenv("METAUI_TEST_SHINYLIVE") != "1", "Set METAUI_TEST_SHINYLIVE=1 to run the shinylive export")
  skip_if_not_installed("shinylive")
  app <- tempfile(); site <- tempfile(); one_step <- tempfile(); editable <- tempfile()
  on.exit(unlink(c(app, site, one_step, editable), recursive = TRUE), add = TRUE)
  generate_shiny(prepared(), "Static export fixture", save_to_folder = app, launch_app = FALSE)
  before <- tools::md5sum(file.path(app, c("www/metaui.js", "ui.R")))
  out <- export_static(app, site, subdir = "fixture")
  expect_true(all(file.exists(file.path(out, c("index.html", "app.json", "STATIC-NOTICE.md", "bundled-packages.csv")))))
  expect_true(file.exists(file.path(site, "shinylive-sw.js")))
  expect_identical(tools::md5sum(file.path(app, c("www/metaui.js", "ui.R"))), before)
  files <- jsonlite::read_json(file.path(out, "app.json"))
  js <- Filter(function(f) f$name == "www/metaui.js", files)[[1]]$content
  expect_match(js, "Static hosting (shinylive)", fixed = TRUE)
  bundled <- utils::read.csv(file.path(out, "bundled-packages.csv"))
  expect_true(all(c("metafor", "shiny", "bslib") %in% bundled$package))
  ui <- Filter(function(f) f$name == "ui.R", files)[[1]]$content
  expect_silent(parse(text = ui))
  preview <- paste(readLines(file.path(out, "metaui-preview/index.html")), collapse = "\n")
  for (package in c("readxl", "shiny")) {
    row <- paste0("<td>", package, "</td><td>", bundled$version[match(package, bundled$package)], "</td>")
    expect_match(ui, row, fixed = TRUE)
    expect_match(preview, row, fixed = TRUE)
  }
  expect_match(ui, paste0("<td>R</td><td>", metaUI:::static_r_version(file.path(site, "shinylive/webr")), "</td>"), fixed = TRUE)
  expect_setequal(unique(bundled$location), c("app packages", "webR base library"))
  expect_error(export_static(app, site, subdir = "fixture"), "Destination exists")
  result <- generate_shiny(prepared(), "One-step static fixture", save_to_folder = editable, static_folder = one_step)
  expect_identical(result, normalizePath(one_step))
  expect_true(all(file.exists(file.path(result, c("app.json", "index.html", "metaui-preview/provenance.json")))))
})

test_that("precomputation uses the app server and writes an inert Summary", {
  app <- tempfile(); preview <- tempfile()
  on.exit(unlink(c(app, preview), recursive = TRUE))
  raw <- fixture()
  raw$year <- rep(c(2000, 2020), 8)
  raw$group <- factor(rep(c("A", "B"), 8))
  data <- prepared(raw, filters = c("year", "group"))
  generate_shiny(data, "Preview fixture", save_to_folder = app, launch_app = FALSE,
    primary_model = get_model_tibble()$name[1], filter_popups = list(year = "Year help"))
  ui_path <- file.path(app, "ui.R")
  writeLines(c("author_ui <- {", readLines(ui_path), "}",
    'shiny::tagList(author_ui, shiny::tags$div(class = "download-container upload-container custom-load-container shiny-spinner-placeholder-custom", "Author preview content"))'), ui_path)
  before <- tools::md5sum(list.files(app, recursive = TRUE, full.names = TRUE))
  rendered <- metaUI:::static_precompute(app, preview)
  html <- paste(readLines(file.path(preview, "index.html")), collapse = "\n")
  expect_match(rendered$selection_overview, "16 of 16")
  expect_match(html, "Author preview content", fixed = TRUE)
  expect_match(html, 'aria-busy="true"', fixed = TRUE)
  expect_match(html, "Loading…", fixed = TRUE)
  expect_false(grepl("Precomputed results", html, fixed = TRUE))
  provenance <- jsonlite::read_json(file.path(preview, "provenance.json"))
  expect_equal(provenance$date, as.character(Sys.Date()))
  expect_equal(provenance$metaUI, as.character(packageVersion("metaUI")))
  expect_match(html, as.character(rendered$primary_estimate$html), fixed = TRUE)
  expect_match(html, 'data:image/png;base64,', fixed = TRUE)
  expect_match(html, 'disabled', fixed = TRUE)
  expect_false(grepl("<(input|button|select|textarea)\\b(?![^>]*\\sdisabled)", html, perl = TRUE))
  expect_false(grepl('<script', html, fixed = TRUE))
  css <- list.files(preview, pattern = "\\.css$", recursive = TRUE, full.names = TRUE)
  expect_false(any(vapply(css, function(path) any(grepl("@import[^;]*https?://", readLines(path, warn = FALSE))), logical(1))))
  referenced <- metaUI:::static_preview_files(preview)
  expect_setequal(list.files(preview, recursive = TRUE), c(referenced, "provenance.json"))
  expect_true(all(file.exists(file.path(preview, referenced))))
  expect_identical(tools::md5sum(list.files(app, recursive = TRUE, full.names = TRUE)), before)
})

test_that("preview opt-out and failed precomputation preserve a working export", {
  skip_if_not_installed("shinylive")
  app <- tempfile(); site <- tempfile()
  on.exit(unlink(c(app, site), recursive = TRUE))
  dir.create(app); dir.create(file.path(app, "www"))
  for (f in c("global.R", "server.R", "ui.R", "www/metaui.js")) writeLines("", file.path(app, f))
  local_mocked_bindings(export = function(appdir, destdir, subdir, ...) {
    dir.create(destdir, showWarnings = FALSE)
    writeLines('<html><body><div id="root"></div></body></html>', file.path(destdir, "index.html"))
    jsonlite::write_json(list(), file.path(destdir, "app.json"))
  }, .package = "shinylive")
  local_mocked_bindings(static_bundled_packages = function(...) data.frame(package = "shiny"),
    static_r_version = function(...) "4.6.0",
    static_notice = function(...) "notice", static_precompute = function(...) stop("fixture failure"), .package = "metaUI")
  expect_no_warning(export_static(app, site, precompute = FALSE))
  expect_false(file.exists(file.path(site, "metaui-preview")))
  expect_warning(export_static(app, site, overwrite = TRUE), "fixture failure")
  expect_match(readLines(file.path(site, "index.html")), 'id="root"', fixed = TRUE)
  expect_false(file.exists(file.path(site, "metaui-preview")))
})

test_that("static About rewriting uses bundled versions and preserves author content", {
  html <- "<p>Author's citation</p><h4>R packages used</h4><div class='metaui-packages'><table><tr><td>readxl</td><td>1.6.0</td></tr></table></div>"
  bundled <- data.frame(package = c("readxl", "shiny"), version = c("1.5.0.1", "1.13.0.8000"))
  rewrite <- function(x) metaUI:::static_rewrite_about(x, bundled, "4.6.0")
  out <- rewrite(html)
  expect_match(out, "Author's citation", fixed = TRUE)
  expect_match(out, "<td>R</td><td>4.6.0</td>", fixed = TRUE)
  expect_match(out, "<td>readxl</td><td>1.5.0.1</td>", fixed = TRUE)
  expect_match(out, "<td>shiny</td><td>1.13.0.8000</td>", fixed = TRUE)
  expect_false(grepl("1.6.0", out, fixed = TRUE))
  code <- paste0("shiny::HTML(", paste(deparse(html), collapse = "\n"), ")")
  expect_identical(as.character(eval(parse(text = rewrite(code)))), out)
  expect_identical(rewrite("No package table"), "No package table")
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  path <- file.path(root, "vfs/usr/lib/R/library/translations")
  dir.create(path, recursive = TRUE)
  writeLines(c("Package: translations", "Version: 9.8.7"), file.path(path, "DESCRIPTION"))
  expect_identical(metaUI:::static_r_version(root), "9.8.7")
})

test_that("preview dependency closure resolves CSS relatives, queries and fonts", {
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  dir.create(file.path(root, "css")); dir.create(file.path(root, "fonts"))
  writeLines('<link href="css/main.css"><img src="data:image/png;base64,AAA">', file.path(root, "index.html"))
  writeLines('@import "extra.css"; .a {src:url(../fonts/font.woff?v=1#x)}', file.path(root, "css/main.css"))
  writeLines('.b { background:url("../image.png") }', file.path(root, "css/extra.css"))
  file.create(file.path(root, c("fonts/font.woff", "image.png", "unused.js")))
  expect_setequal(metaUI:::static_preview_files(root), c("index.html", "css/main.css", "css/extra.css", "fonts/font.woff", "image.png"))
  unlink(file.path(root, "image.png"))
  expect_error(metaUI:::static_preview_files(root), "Missing preview asset: .*image.png")
})


test_that("export rejects subdirectories escaping or replacing the destination", {
  skip_if_not_installed("shinylive")
  root <- tempfile(); app <- file.path(root, "app"); site <- file.path(root, "site")
  on.exit(unlink(root, recursive = TRUE))
  dir.create(file.path(app, "www"), recursive = TRUE)
  for (f in c("global.R", "ui.R", "server.R", "www/metaui.js")) writeLines("", file.path(app, f))
  dir.create(site); writeLines("keep", file.path(site, "keep.txt"))
  outside <- file.path(root, "outside"); dir.create(outside)
  writeLines("keep", file.path(outside, "keep.txt"))
  for (subdir in c("../outside", ".", "a/..", outside, "/absolute", "C:/absolute")) {
    expect_error(export_static(app, site, subdir = subdir, overwrite = TRUE), "strictly inside dest_dir")
    expect_identical(readLines(file.path(site, "keep.txt")), "keep")
    expect_identical(readLines(file.path(outside, "keep.txt")), "keep")
  }
  skip_on_os("windows")
  alias <- file.path(site, "alias")
  if (!file.symlink(outside, alias)) skip("symlinks are unavailable")
  on.exit(unlink(alias), add = TRUE)
  expect_error(export_static(app, site, subdir = "alias", overwrite = TRUE), "strictly inside dest_dir")
  expect_true(file.exists(file.path(outside, "keep.txt")))
})

test_that("source notice links account for nested subdirectories", {
  for (subdir in c("", "a", "a/b")) {
    expected <- switch(subdir, "sources/", a = "../sources/", "a/b" = "../../sources/")
    path <- metaUI:::static_sources_path(subdir)
    expect_identical(path, expected)
    expect_true(any(grepl(path, metaUI:::static_notice(data.frame(package = "shiny"), TRUE, path), fixed = TRUE)))
  }
})

test_that("overwrite replaces only recognisable exports and sources cover the shared union", {
  skip_if_not_installed("shinylive")
  root <- tempfile(); app <- file.path(root, "app"); site <- file.path(root, "site")
  on.exit(unlink(root, recursive = TRUE))
  dir.create(file.path(app, "www"), recursive = TRUE)
  for (f in c("global.R", "ui.R", "server.R", "www/metaui.js")) writeLines("", file.path(app, f))
  union <- data.frame(package = c("appA", "appB"), version = "1.0", license = "GPL-3")
  source_union <- NULL
  local_mocked_bindings(export = function(appdir, destdir, subdir, ...) {
    target <- file.path(destdir, subdir)
    dir.create(target, recursive = TRUE, showWarnings = FALSE)
    writeLines("<html><body></body></html>", file.path(target, "index.html"))
    jsonlite::write_json(list(), file.path(target, "app.json"))
    writeLines("", file.path(destdir, "shinylive-sw.js"))
  }, .package = "shinylive")
  local_mocked_bindings(static_bundled_packages = function(...) union,
    static_r_version = function(...) "4.6.0",
    static_include_sources = function(bundled, ...) { source_union <<- bundled }, .package = "metaUI")
  for (subdir in c("", "a/b")) {
    target <- if (nzchar(subdir)) file.path(site, subdir) else site
    dir.create(target, recursive = TRUE, showWarnings = FALSE)
    writeLines("keep", file.path(target, "hand-edit.txt"))
    expect_error(export_static(app, site, subdir, overwrite = TRUE, precompute = FALSE),
      "empty or previous-export folder")
    expect_identical(readLines(file.path(target, "hand-edit.txt")), "keep")
    # Both permitted recognition forms: root service worker or nested app files.
    if (!nzchar(subdir)) writeLines("", file.path(target, "shinylive-sw.js"))
    else {
      writeLines("", file.path(target, "index.html"))
      jsonlite::write_json(list(), file.path(target, "app.json"))
      writeLines("sibling", file.path(site, "sibling.txt"))
    }
    export_static(app, site, subdir, overwrite = TRUE, precompute = FALSE, include_sources = TRUE)
    expect_false(file.exists(file.path(target, "hand-edit.txt")))
    expect_identical(source_union, union)
    notice <- paste(readLines(file.path(target, "STATIC-NOTICE.md")), collapse = "\n")
    expect_match(notice, "shared runtime", fixed = TRUE)
    expect_match(notice, if (nzchar(subdir)) "../../sources/" else "sources/", fixed = TRUE)
    if (nzchar(subdir)) expect_identical(readLines(file.path(site, "sibling.txt")), "sibling")
  }
})
