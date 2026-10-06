test_that("generated sample tabs share values without duplicate output bindings", {
  path <- tempfile(); on.exit(unlink(path, recursive = TRUE))
  d <- prepared()
  generate_shiny(d, "Sample overview", save_to_folder = path, launch_app = FALSE)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(path)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  html <- as.character(env$ui)
  expect_equal(lengths(regmatches(html, gregexpr('id="sample"', html, fixed = TRUE))), 1L)
  expect_equal(lengths(regmatches(html, gregexpr('id="sample_overview"', html, fixed = TRUE))), 1L)
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-10, 10), go = 1)
    expect_identical(output$sample, output$sample_overview)
    session$setInputs(outliers_z_scores = c(0, 10), go = 2)
    expect_identical(output$sample, output$sample_overview)
  })
})

test_that("oversized forest selections show HTML without creating a plot output", {
  path <- tempfile(); on.exit(unlink(path, recursive = TRUE))
  d <- prepared()
  generate_shiny(d, "Forest limit", save_to_folder = path, launch_app = FALSE,
    options = list(max_forest_plot_rows = 8))
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(path)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-10, 10), go = 1)
    expect_match(output$forest_panel$html, "8 effect sizes or fewer")
    expect_false(grepl('id="foreststudies"', output$forest_panel$html, fixed = TRUE))
    expect_error(output$foreststudies, "8 effect sizes or fewer")
    # The exact boundary should still render a plot.
    state_values$uploaded_data <- d[1:8, ]
    session$setInputs(go = 2)
    expect_match(output$forest_panel$html, 'id="foreststudies"', fixed = TRUE)
    expect_true(is.list(output$foreststudies))
  })
})

test_that("inline filter help preserves HTML, Unicode and plain-text escaping", {
  x <- fixture(); x$year <- 2001:2016; x$group <- factor(rep(c("A", "B"), 8))
  d <- prepare_data(x, "study", "yi", variance = "vi", es_id = "id",
    filters = c(Year = "year", Group = "group"))
  path <- tempfile(); on.exit(unlink(path, recursive = TRUE))
  generate_shiny(d, "Filter-help fixture", save_to_folder = path, launch_app = FALSE,
    filter_popups = list(Year = shiny::HTML("<b>Year's</b> \"quoted\" help <table><tr><td>Été</td></tr></table><a href='https://example.org'>Details</a>"), Group = "Scores < 5 or A&B"))
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(path)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  html <- as.character(env$ui)
  expect_match(html, "Help for Year", fixed = TRUE)
  expect_match(html, "Help for Group", fixed = TRUE)
  expect_match(html, "<td>Été</td>", fixed = TRUE)
  expect_match(html, "https://example.org", fixed = TRUE)
  expect_match(html, "Scores &lt; 5 or A&amp;B", fixed = TRUE)
  expect_match(html, "<b>Year's</b>", fixed = TRUE)
  expect_match(html, 'aria-controls="metaui-help-1"', fixed = TRUE)
  expect_false(any(grepl("shinyBS", readLines("global.R"), fixed = TRUE)))
  expect_true(file.exists("www/metaui.js"))
})

test_that("invalid forest limits fail before creating an app", {
  for (limit in list(0, -1, 1.5, Inf, NA_real_, "10", c(1, 2))) {
    path <- tempfile()
    expect_error(generate_shiny(prepared(), "Bad limit", save_to_folder = path,
      launch_app = FALSE, options = list(max_forest_plot_rows = limit)),
      "positive finite whole number")
    expect_false(dir.exists(path))
  }
})

test_that("installed correlation example builds and matches its independent fit", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  file.copy(system.file("examples", "correlations", "build.R", package = "metaUI"), root)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(root)
  env <- new.env(parent = globalenv()); sys.source("build.R", env)
  table <- fit(env$d)$table
  expect_equal(table$fit_es[1], as.numeric(env$reference$b), tolerance = 1e-8)
  expect_equal(table$es[1], tanh(as.numeric(env$reference$b)), tolerance = 1e-8)
  expect_equal(table$LCL[1], tanh(env$reference$ci.lb), tolerance = 1e-8)
  expect_equal(table$UCL[1], tanh(env$reference$ci.ub), tolerance = 1e-8)
  expect_true(file.exists("correlation-app/validation.json"))
})

test_that("forest rows retain independent intervals and both summary models", {
  d <- prepared()
  estimates <- fit(d)$table
  rows <- metaUI:::metaUI_forest_rows(d, estimates)
  expect_equal(nrow(rows), nrow(d) + 2L)
  expect_equal(rows$label[rows$kind == "Model summary"], estimates$Model)
  expect_equal(rows$LCL[rows$kind == "Model summary"], estimates$LCL)
  expect_equal(rows$fit_UCL[1], d$metaUI__effect_size[1] + qnorm(.975) * d$metaUI__se[1])
  renamed <- get_model_tibble()[1:2, ]; renamed$name[1] <- "RE 2-level model"
  renamed_rows <- metaUI:::metaUI_forest_rows(d, fit(d, renamed)$table, renamed)
  expect_equal(renamed_rows$label[renamed_rows$kind == "Model summary"], c("RE 2-level model", "Robust Variance Estimation"))
  x <- fixture(); x$yi <- x$yi/2
  z <- prepared(x, es_type = "COR", variance_scale = "r")
  r <- metaUI:::metaUI_forest_rows(z, fit(z)$table)
  expect_equal(r$es, tanh(r$fit_es))
  expect_equal(r$LCL, tanh(r$fit_LCL))
  expect_equal(r$UCL, tanh(r$fit_UCL))
  expect_s3_class(metaUI:::metaUI_forest_plot(r, "r"), "ggplot")
})

test_that("interval assessments use strict bounds, valid intervals and display scales", {
  t <- data.frame(Model = letters[1:8], status = c(rep("ok", 6), "failed", "unsupported"),
    LCL = c(-.1, .05, .21, -.4, -.2, -.5, NA, NA), UCL = c(.1, .1, .4, -.21, .2, .5, NA, NA))
  r <- metaUI:::metaUI_interval_assessment(t, .2, known_models = "a")
  expect_equal(r$assessment, c(rep("Interval within bounds",2), "Exceeds positive bound",
    "Exceeds negative bound", "Inconclusive", "Inconclusive", "Not assessed", "Not assessed"))
  expect_equal(r$threshold[1:6], c(.1, .1, .4, .4, .2, .5))
  expect_equal(r$interval[1], "Reported 95% CI")
  expect_match(r$interval[2], "unknown")
  expect_equal(metaUI:::metaUI_interval_assessment(t)$assessment[1], "No bound chosen")
  expect_error(metaUI:::metaUI_interval_assessment(t, 1, "r"), "between 0 and 1")
  expect_error(metaUI:::metaUI_interval_assessment(t, 0), "positive finite")
  expect_equal(nrow(metaUI:::metaUI_interval_assessment(t[0, ], .2)), 0L)
  # Monotone transform gives the same containment decisions on z and r scales.
  z <- t; z$LCL <- atanh(t$LCL); z$UCL <- atanh(t$UCL)
  expect_identical(metaUI:::metaUI_interval_assessment(t, .2, "r")$assessment,
    metaUI:::metaUI_interval_assessment(z, atanh(.2))$assessment)
})

test_that("changing the practical bound does not refit or change applied filters", {
  path <- tempfile(); on.exit(unlink(path, recursive = TRUE))
  generate_shiny(prepared(), "Bound change", save_to_folder = path, launch_app = FALSE)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(path)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-10, 10), go = 1)
    original <- estimatesreactive()
    expect_equal(length(known_interval_models), 7L)
    session$setInputs(sesoi = 1)
    expect_identical(estimatesreactive(), original)
    expect_false(filters_changed())
    expect_equal(data_list()$equivalence$bound, rep(1, 7))
    expect_true(any(interval_assessment()$assessment == "Interval within bounds"))
    session$setInputs(sesoi = -1)
    expect_match(interval_assessment()$reason, "positive finite")
    expect_identical(estimatesreactive(), original)
  })
})
