test_that("Summary plot recovers from startup with webR Shiny error handling", {
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
    # Evaluate before Analyze, just as a connected browser does. The height
    # must not abort inside renderPlot's isolated getDims() call.
    expect_error(output$model_comparison, class = "shiny.silent.error")
    session$setInputs(outliers_z_scores = c(-10, 10), go = 1)
    image <- output$model_comparison
    expect_match(image$src, "^data:image/png;base64,")
    expect_equal(image$height, 65 + 38 * max(1, sum(estimatesfiltered()$status == "ok")))
    session$setInputs(outliers_z_scores = c(-1.5, 1), go = 2)
    changed <- output$model_comparison
    expect_match(changed$src, "^data:image/png;base64,")
    expect_false(identical(image$src, changed$src))
    expect_equal(changed$height, 65 + 38 * max(1, sum(estimatesfiltered()$status == "ok")))
    # An empty selection blanks the plot; it must redraw once rows are selected again.
    session$setInputs(outliers_z_scores = c(9, 10), go = 3)
    expect_error(output$model_comparison, class = "shiny.silent.error")
    session$setInputs(outliers_z_scores = c(-10, 10), go = 4)
    expect_match(output$model_comparison$src, "^data:image/png;base64,")
  })
})
