test_that("static generation exports generated sources and suppresses launching", {
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  app <- file.path(root, "app"); site <- file.path(root, "site")
  calls <- list()
  local_mocked_bindings(requireNamespace = function(...) TRUE, .package = "base")
  local_mocked_bindings(
    shinyAppDir = function(...) stop("must not launch"),
    export_static = function(app_dir, dest_dir, overwrite, precompute = TRUE) {
      expect_true(file.exists(file.path(app_dir, "dataset.rds")))
      calls[[length(calls) + 1L]] <<- list(app_dir, dest_dir, overwrite, precompute)
      dest_dir
    }, .package = "metaUI")
  result <- withVisible(generate_shiny(prepared(), "Static", save_to_folder = app,
    static_folder = site, overwrite = TRUE, launch_app = TRUE))
  expect_false(result$visible)
  expect_identical(result$value, site)
  expect_identical(calls[[1]], list(app, site, TRUE, TRUE))
  expect_false(eval(formals(generate_shiny)$launch_app,
    list(save_to_folder = NA, static_folder = site)))
  expect_false(eval(formals(generate_shiny)$launch_app,
    list(save_to_folder = app, static_folder = NULL)))
  expect_true(eval(formals(generate_shiny)$launch_app,
    list(save_to_folder = NA, static_folder = NULL)))
  expect_identical(generate_shiny(prepared(), "Temporary", static_folder = site), site)
  temporary <- calls[[2]][[1]]
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  expect_true(dir.exists(temporary))
  expect_true(startsWith(temporary, tempdir()))
  expect_identical(calls[[2]][2:4], list(site, FALSE, TRUE))
})

test_that("static generation validates before writing sources", {
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  app <- file.path(root, "app")
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(generate_shiny(NULL, "Static", save_to_folder = app,
    static_folder = file.path(root, "site")), "requires the shinylive package")
  expect_false(dir.exists(app))
  local_mocked_bindings(requireNamespace = function(...) TRUE, .package = "base")
  # Equal, inside, or containing the source folder (which overwrite would delete).
  for (site in c(app, file.path(app, "site"), file.path(app, "new", "..", "site"), root)) {
    expect_error(generate_shiny(NULL, "Static", save_to_folder = app,
      static_folder = site), "separate from save_to_folder")
    expect_false(dir.exists(app))
  }
  dir.create(app)
  link <- file.path(root, "alias")
  if (file.symlink(app, link))
    expect_error(generate_shiny(NULL, "Static", save_to_folder = app,
      static_folder = file.path(link, "site")), "separate from save_to_folder")
  for (site in list(NA_character_, "", c("a", "b"), 1, FALSE))
    expect_error(generate_shiny(NULL, "Static", static_folder = site), "nonempty scalar string")
})

test_that("JSON static_folder resolves relative to the config in both schema versions", {
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  utils::write.csv(fixture(), file.path(root, "data.csv"), row.names = FALSE)
  cfg <- list(schema_version = 1, data = "data.csv", output = "app",
    static_folder = "site", dataset_name = "Static config",
    mapping = list(study_label = "study", es_field = "yi", variance = "vi", es_type = "SMD"))
  local_mocked_bindings(requireNamespace = function(...) TRUE, .package = "base")
  local_mocked_bindings(export_static = function(app_dir, dest_dir, overwrite) {
      expect_identical(app_dir, file.path(root, "app"))
      expect_true(file.exists(file.path(app_dir, "server.R")))
      expect_false(overwrite)
      dest_dir
    }, .package = "metaUI")
  for (version in 1:2) {
    cfg$schema_version <- version
    jsonlite::write_json(cfg, file.path(root, "config.json"), auto_unbox = TRUE)
    result <- withVisible(build_app(file.path(root, "config.json")))
    expect_identical(result$value, file.path(root, "site"))
    expect_false(result$visible)
    unlink(file.path(root, "app"), recursive = TRUE)
  }
  for (site in list(NULL, NA_character_, "", 1, c("a", "b"))) {
    cfg["static_folder"] <- list(site)
    expect_error(build_app(cfg), "nonempty scalar string")
  }
})
