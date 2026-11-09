test_that("Summary table recovers from startup with webR Shiny error handling", {
  skip_without_app_packages()
  fixture_env <- new.env()
  sys.source(test_path("fixtures", "webr-hybrid-chain.R"), fixture_env)
  hybrid <- fixture_env$webr_hybrid_chain
  environment(hybrid) <- asNamespace("shiny")
  local_mocked_bindings(hybrid_chain = hybrid, .package = "shiny")

  path <- tempfile("summary-plot-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  d <- prepared()
  generate_shiny(d, "Summary startup", save_to_folder = path, launch_app = FALSE)
  wd <- setwd(path); on.exit(setwd(wd), add = TRUE)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  shiny::testServer(env$server, {
    # Evaluate before Analyze, just as a connected browser does: it must not abort.
    expect_error(output$effectestimate, class = "shiny.silent.error")
    session$setInputs(outliers_z_scores = c(-10, 10), go = 1)
    table <- output$effectestimate
    expect_match(table, "metaui-track-diamond", fixed = TRUE)
    session$setInputs(outliers_z_scores = c(-1.5, 1), go = 2)
    changed <- output$effectestimate
    expect_match(changed, "metaui-track-diamond", fixed = TRUE)
    expect_false(identical(table, changed))
    # An empty selection blanks the table; it must redraw once rows are selected again.
    session$setInputs(outliers_z_scores = c(9, 10), go = 3)
    expect_error(output$effectestimate, class = "shiny.silent.error")
    expect_match(as.character(output$primary_estimate$html), "No effects match this selection", fixed = TRUE)
    session$setInputs(outliers_z_scores = c(-10, 10), go = 4)
    expect_match(output$effectestimate, "metaui-track-diamond", fixed = TRUE)
  })
})


test_that("Egger panel explains selections with fewer than three studies", {
  skip_without_app_packages()
  path <- tempfile(); old <- getwd()
  on.exit({setwd(old); unlink(path, recursive = TRUE)})
  generate_shiny(prepared(fixture()[1:2, ]), "Two studies", save_to_folder = path, launch_app = FALSE)
  setwd(path)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-10, 10), go = 1)
    expect_error(output$eggers, "Egger test requires at least three studies", class = "shiny.silent.error")
  })
})
