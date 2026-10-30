test_that("source plans select copyleft packages and construct exact-version URLs", {
  bundled <- data.frame(package = c("gpl", "lgpl", "agpl", "mpl", "mit", "httpuv"),
    version = c(rep("1.2.3", 5), "1.6.6.9000"),
    license = c("GPL (>= 2)", "LGPL-2.1", "AGPL-3", "MPL-2.0", "MIT", "GPL (>= 2)"))
  plan <- metaUI:::static_source_plan(bundled, "9.8.7", "6.5.4")
  expect_equal(vapply(plan, `[[`, "", "package"), c("gpl", "lgpl", "agpl", "mpl", "httpuv", "R", "webR"))
  expect_equal(plan[[1]]$urls, c("https://cloud.r-project.org/src/contrib/gpl_1.2.3.tar.gz",
    "https://cloud.r-project.org/src/contrib/Archive/gpl/gpl_1.2.3.tar.gz"))
  expect_equal(plan[[5]]$urls, "https://github.com/r-wasm/httpuv/archive/5558cf7aa7dbc840ce2e2c52ed9e473075f002a3.tar.gz")
  expect_equal(plan[[6]]$urls, "https://cloud.r-project.org/src/base/R-9/R-9.8.7.tar.gz")
  expect_equal(plan[[7]]$urls, "https://github.com/r-wasm/webr/archive/refs/tags/v6.5.4.tar.gz")
  expect_match(metaUI:::static_package_source("shiny", "1.13.0.8000"), "/tree/v1.13.0.8000", fixed = TRUE)
  expect_match(metaUI:::static_package_source("shiny", "2.0.0.9000"), "source not recorded", fixed = TRUE)
})

test_that("source collection retries archives, preserves downloads and warns on failure", {
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  path <- file.path(root, "vfs/usr/lib/R/library/translations")
  dir.create(path, recursive = TRUE)
  writeLines(c("Package: translations", "Version: 4.6.0"), file.path(path, "DESCRIPTION"))
  writeLines('var base="https://webr.r-wasm.org/v0.6.0/";', file.path(root, "webr.mjs"))
  bundled <- data.frame(package = c("alpha", "beta", "unknown"), version = c("1.0", "1.0", "1.0.9000"), license = "GPL-3")
  calls <- character()
  download <- function(url, destfile, ...) {
    calls <<- c(calls, url)
    if (grepl("beta|contrib/alpha", url)) stop("Unavailable")
    writeLines("archive fixture", destfile)
    0L
  }
  sources <- file.path(root, "sources")
  expect_warning(expect_warning(metaUI:::static_include_sources(bundled, root, sources, download),
    "unknown"), "beta")
  expect_true(all(file.exists(file.path(sources, c("alpha_1.0.tar.gz", "R-4.6.0.tar.gz", "webr-0.6.0.tar.gz", "README.md")))))
  expect_match(paste(readLines(file.path(sources, "README.md")), collapse = "\n"), "Archive/alpha/alpha_1.0.tar.gz", fixed = TRUE)
  expect_true(any(grepl("contrib/alpha", calls)))
  calls <- character()
  suppressWarnings(metaUI:::static_include_sources(bundled, root, sources, download))
  expect_false(any(grepl("alpha|R-4.6.0|webr-0.6.0", calls)))
  expect_equal(sum(startsWith(readLines(file.path(sources, "README.md")), "alpha_1.0.tar.gz | ")), 1)
})


test_that("source plans retain each bundled package version", {
  bundled <- data.frame(package = rep("example", 3), version = c("1.0", "2.0", "1.0"), license = "GPL-3")
  plan <- metaUI:::static_source_plan(bundled, "4.6.0", "0.6.0")
  expect_identical(vapply(plan[1:2], `[[`, "", "file"), c("example_1.0.tar.gz", "example_2.0.tar.gz"))
  expect_length(plan, 4L)
})
