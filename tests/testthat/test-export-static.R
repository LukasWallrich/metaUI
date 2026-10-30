test_that("bundled-package table reads licences and flags webR-patched builds", {
  pkg_dir <- tempfile()
  on.exit(unlink(pkg_dir, recursive = TRUE))
  for (spec in list(c("alpha", "1.2.3", "MIT + file LICENSE"), c("beta", "2.0.0.9000", "GPL-3"))) {
    src <- file.path(tempfile(), spec[1])
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

test_that("export_static() writes a static site with the bridge and notices", {
  # Downloads ~100 MB of WebAssembly packages; run explicitly.
  skip_on_cran()
  skip_if(Sys.getenv("METAUI_TEST_SHINYLIVE") != "1", "Set METAUI_TEST_SHINYLIVE=1 to run the shinylive export")
  skip_if_not_installed("shinylive")
  app <- file.path(tempfile(), "app")
  generate_shiny(prepared(), "Static export fixture", save_to_folder = app, launch_app = FALSE)
  before <- tools::md5sum(file.path(app, "www", "metaui.js"))
  site <- tempfile()
  out <- export_static(app, site, subdir = "fixture")
  expect_true(all(file.exists(file.path(out, c("index.html", "app.json", "STATIC-NOTICE.md", "bundled-packages.csv")))))
  expect_true(file.exists(file.path(site, "shinylive-sw.js")))
  expect_identical(tools::md5sum(file.path(app, "www", "metaui.js")), before)
  files <- jsonlite::read_json(file.path(out, "app.json"))
  js <- Filter(function(f) f$name == "www/metaui.js", files)[[1]]$content
  expect_match(js, "Static hosting (shinylive)", fixed = TRUE)
  bundled <- utils::read.csv(file.path(out, "bundled-packages.csv"))
  expect_true(all(c("metafor", "shiny", "bslib") %in% bundled$package))
  expect_setequal(unique(bundled$location), c("app packages", "webR base library"))
  expect_error(export_static(app, site, subdir = "fixture"), "Destination exists")
})
