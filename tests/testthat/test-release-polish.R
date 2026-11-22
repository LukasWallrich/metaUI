test_that("prepare_data normalises NULL optional columns, checks missing url, and cleans DOIs including factor columns", {
  x <- fixture()
  d <- prepare_data(x, "study", "yi", variance = "vi", es_id = "id",
    article_label = NULL, es_label = NULL, url = NULL)
  expect_false("metaUI__url" %in% names(d))
  expect_false("metaUI__article_label" %in% names(d))
  expect_equal(nrow(d), 16L)

  expect_error(
    prepare_data(x, "study", "yi", variance = "vi", es_id = "id", url = "missing_col"),
    "Mapped URL column not found: missing_col"
  )

  x$url <- factor(rep(c("  doi:10.1234/abc  ", "DOI: 10.5678/xyz", " 10.9999/test ", "https://example.org/p"), 4))
  d_url <- prepare_data(x, "study", "yi", variance = "vi", es_id = "id", url = "url")
  expect_type(d_url$metaUI__url, "character")
  expect_identical(
    d_url$metaUI__url[1:4],
    c("https://doi.org/10.1234/abc", "https://doi.org/10.5678/xyz", "https://doi.org/10.9999/test", "https://example.org/p")
  )
})

test_that("build_app supports eff_size_type_label, filter_popups, and JSON null mapping fields", {
  skip_without_app_packages()
  root <- tempfile("metaui-config-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  x <- fixture(); x$year <- 2001:2016
  utils::write.csv(x, file.path(root, "data.csv"), row.names = FALSE)
  cfg <- list(
    schema_version = 1,
    data = "data.csv",
    output = "app",
    dataset_name = "Config polish",
    eff_size_type_label = "Hedges g",
    filter_popups = list(Year = "Publication year < 2020 & > 2000"),
    mapping = list(
      study_label = "study", es_field = "yi", variance = "vi", es_type = "SMD",
      es_id = "id", article_label = NULL, es_label = NULL, url = NULL,
      filters = list(Year = "year")
    )
  )
  path <- file.path(root, "config.json")
  jsonlite::write_json(cfg, path, auto_unbox = TRUE, null = "null")
  build_app(path)
  expect_true(file.exists(file.path(root, "app", "validation.json")))
  global_code <- paste(readLines(file.path(root, "app", "global.R")), collapse = "\n")
  expect_match(global_code, "Hedges g", fixed = TRUE)
  ui_code <- paste(readLines(file.path(root, "app", "ui.R")), collapse = "\n")
  expect_match(ui_code, "Publication year &lt; 2020 &amp; &gt; 2000", fixed = TRUE)

  bad_label <- cfg; bad_label$eff_size_type_label <- ""
  expect_error(build_app(bad_label), "eff_size_type_label must be a nonempty scalar string")
  bad_popup <- cfg; bad_popup$filter_popups <- list(Year = 123)
  expect_error(build_app(bad_popup), "filter_popups must be an object")
})

test_that("generate_shiny validates options up front before creating output", {
  skip_without_app_packages()
  d <- prepared()
  path <- tempfile("metaui-bad-opts-")
  on.exit(unlink(path, recursive = TRUE))

  expect_error(
    generate_shiny(d, "Bad opt", save_to_folder = path, launch_app = FALSE, options = list(typo_option = 1)),
    "Unknown option"
  )
  expect_false(dir.exists(path))

  expect_error(
    generate_shiny(d, "Bad theme", save_to_folder = path, launch_app = FALSE, options = list(shiny_theme = "not-a-theme")),
    "Invalid options\\$shiny_theme"
  )
  expect_false(dir.exists(path))

  expect_error(
    generate_shiny(d, "Bad threshold", save_to_folder = path, launch_app = FALSE, options = list(selection_list_threshold = 0)),
    "selection_list_threshold must be a positive finite whole number"
  )
  expect_false(dir.exists(path))

  expect_error(
    generate_shiny(d, "Bad intro", save_to_folder = path, launch_app = FALSE, options = list(intro = 123)),
    "options\\$intro must be a single string of HTML"
  )
  expect_false(dir.exists(path))
})

test_that("se.from.p handles NA inputs and pcurve restores graphics margins", {
  res_p <- metaUI:::se.from.p(effect.size = c(0.4, 0.2), p = c(0.05, NA_real_), N = c(100, 100), effect.size.type = "difference")
  expect_true(is.finite(res_p$StandardError[1]))
  expect_true(is.na(res_p$StandardError[2]))

  res_ratio <- metaUI:::se.from.p(effect.size = c(1.5, NA_real_), p = c(0.04, 0.05), N = c(100, 100), effect.size.type = "ratio")
  expect_true(is.finite(res_ratio$logStandardError[1]))
  expect_true(is.na(res_ratio$logStandardError[2]))

  png_file <- tempfile(fileext = ".png")
  on.exit(unlink(png_file), add = TRUE)
  grDevices::png(png_file)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(5, 4, 4, 2) + 0.1)
  before <- graphics::par("mar")
  dat <- data.frame(
    TE = c(0.55, 0.62, 0.48, 0.71, 0.59, 0.64),
    seTE = c(0.18, 0.20, 0.16, 0.22, 0.19, 0.21),
    studlab = paste0("S", 1:6)
  )
  invisible(utils::capture.output(suppressWarnings(metaUI:::pcurve(dat))))
  expect_equal(graphics::par("mar"), before)
})

test_that("study_info helpers handle unencoded percent signs, %00, and empty Crossref issued date-parts", {
  expect_warning(
    out <- metaUI:::metaUI_normalise_doi("https://example.org/100%"),
    NA
  )
  expect_identical(out, "https://example.org/100%")
  expect_false(metaUI:::metaUI_valid_doi(out))
  expect_identical(metaUI:::metaUI_normalise_doi("https://doi.org/10.1234%2Fabc%20def"), "10.1234/abc def")
  expect_identical(metaUI:::metaUI_normalise_doi("10.1000/test%2520"), "10.1000/test%20")
  nul_doi <- metaUI:::metaUI_normalise_doi("10.1234/abc%00xyz")
  expect_identical(nul_doi, "10.1234/abc%00xyz")
  expect_false(metaUI:::metaUI_valid_doi(nul_doi))

  msg <- list(
    DOI = "10.1/empty-date",
    title = list("Title"),
    issued = list(`date-parts` = list(list()))
  )
  parsed <- metaUI:::metaUI_parse_crossref(msg, "none", "2026-10-30")
  expect_true(is.na(parsed$year))
  expect_identical(parsed$title, "Title")
})

test_that("metaUI_reproduction_code does not leave temporary .R files behind", {
  skip_without_app_packages()
  d <- prepared()
  models <- get_model_tibble()[1:2, ]
  before <- list.files(tempdir(), pattern = "\\.R$", full.names = TRUE)
  code <- metaUI:::metaUI_reproduction_code(
    format = "R",
    applied = data.frame(id = "outliers_z_scores", selection = c(-10, 10)),
    filters = list(),
    source = d,
    built = d,
    models = models,
    estimates = fit(d, models)$table,
    settings = list(
      dataset_name = "Tempfile check",
      exported = "2026-11-22",
      correlation = 0.6,
      aggregation = "aggregate",
      known_models = models$name,
      bound = NULL,
      selected_rows = nrow(d),
      workbook = "metaui-data.xlsx",
      upload = NULL,
      primary_model = NULL
    ),
    app_dir = system.file("template_code", package = "metaUI")
  )
  after <- list.files(tempdir(), pattern = "\\.R$", full.names = TRUE)
  expect_true(nzchar(code))
  expect_identical(setdiff(after, before), character())
})

test_that("metaUI_moderator_fit works on one-effect-per-study data without global-binding issues", {
  x <- data.frame(
    study = LETTERS[1:8],
    yi = c(0.1, 0.3, 0.2, 0.4, 0.5, 0.6, 0.4, 0.7),
    vi = rep(0.02, 8),
    grp = factor(rep(c("A", "B"), each = 4))
  )
  d <- prepare_data(x, "study", "yi", variance = "vi", filters = c(Group = "grp"))
  spec <- get_model_tibble(one_effect_per_study = TRUE)[1, ]
  fitted <- metaUI:::metaUI_moderator_fit(d, "metaUI__filter_Group", list(name = spec$name, code = spec$code))
  expect_s3_class(fitted$model, "rma.uni")
  expect_identical(fitted$working, "random")
})

test_that("global.R warmup does not overwrite variables defined in models.R", {
  skip_without_app_packages()
  root <- tempfile("metaui-warmup-")
  on.exit(unlink(root, recursive = TRUE))
  models_file <- tempfile(fileext = ".R")
  on.exit(unlink(models_file), add = TRUE)
  writeLines(c(
    "p <- 42",
    metaUI:::generate_models.R(get_model_tibble()[1:2, ])
  ), models_file)
  generate_shiny(prepared(), "Warmup scope", save_to_folder = root, launch_app = FALSE, models = models_file)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(root)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  expect_identical(env$p, 42)
})

test_that("Effect computations and Bayesian sensitivity handle formatting, warnings, Unavailable names, empty selections, and integer slider steps", {
  skip_without_app_packages()
  root <- tempfile("metaui-polish-app-")
  on.exit(unlink(root, recursive = TRUE))
  x <- fixture()
  x$alt <- -abs(x$yi) - 0.15
  x$year <- rep(2001:2008, each = 2)
  d <- prepared(x, filters = c(Year = "year"),
    alternatives = list("Alt <1 & 2>" = list(es_field = "alt", variance = "vi", es_type = "SMD", justification = "Negative check")),
    primary_label = "Primary & base")
  models <- get_model_tibble()[1:2, ]
  models$name[1] <- "Unavailable"
  generate_shiny(d, "Polish app", save_to_folder = root, launch_app = FALSE,
    models = models, bayesian = list(enabled = TRUE, tau_scale = 0.5))

  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(root)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  shiny::testServer(env$server, {
    # Normal selection: model literally named "Unavailable" renders normally; separate Estimate & 95% CI columns.
    session$setInputs(outliers_z_scores = c(-10, 10), metaUI__filter_Year = c(2001, 2008),
      metaUI__filter_Year_include_NA = TRUE, computation = "primary", go = 1)
    tbl_html <- output$computation_comparison
    expect_match(tbl_html, "<th>Computation</th>", fixed = TRUE)
    expect_match(tbl_html, ">Estimate</th>", fixed = TRUE)
    expect_match(tbl_html, ">95% CI</th>", fixed = TRUE)
    expect_match(tbl_html, "><i>k</i></th>", fixed = TRUE)
    expect_match(tbl_html, "Primary &amp; base", fixed = TRUE)
    expect_match(tbl_html, "Alt &lt;1 &amp; 2&gt;", fixed = TRUE)
    expect_match(tbl_html, "Unavailable", fixed = TRUE)
    expect_match(tbl_html, "\u2212", fixed = TRUE)
    # Downloads keep raw numeric columns.
    raw_comp <- data_list()$computations
    expect_true(all(c("computation", "Model", "es", "LCL", "UCL", "status") %in% names(raw_comp)))

    # Small selection (3 studies): RVE df < 4 warning is carried into Effect computations footnotes.
    session$setInputs(metaUI__filter_Year = c(2001, 2003), go = 2)
    small_html <- output$computation_comparison
    expect_match(small_html, "metaui-note-mark", fixed = TRUE)
    expect_match(small_html, "metaui-warnings", fixed = TRUE)
    expect_match(small_html, "unreliable", fixed = TRUE)

    # Empty selection: neither Effect computations nor Bayesian sensitivity errors on 1-column Note fallback.
    session$setInputs(metaUI__filter_Year = c(1990, 1995), go = 3)
    expect_match(output$computation_comparison, "<th>Note</th>", fixed = TRUE)
    expect_match(output$bayesian_sensitivity, "<th>Note</th>", fixed = TRUE)

    # Uploading a workbook with extended integer Year preserves step = 1.
    u <- as.data.frame(d); attr(u, "metaUI_validation") <- NULL
    u$metaUI__filter_Year[1] <- 2025
    up_file <- tempfile(fileext = ".xlsx"); on.exit(unlink(up_file), add = TRUE)
    writexl::write_xlsx(list(dataset = u, filters = data.frame(
      id = c(rep("outliers_z_scores", 2), rep("metaUI__filter_Year", 2)),
      selection = c(-10, 10, 2001, 2025))), up_file)
    slider_calls <- list()
    testthat::local_mocked_bindings(
      updateSliderInput = function(session = shiny::getDefaultReactiveDomain(), inputId, ...) {
        slider_calls[[inputId]] <<- list(...)
      },
      .package = "shiny"
    )
    session$setInputs(uploadData = list(datapath = up_file, name = "wide-year.xlsx"), executeUpload = 1)
    expect_identical(slider_calls$metaUI__filter_Year$step, 1)
  })

  # Custom interval_type uses "Interval" header and carries its type note; missing endpoints show Not estimated cleanly.
  root_custom <- tempfile("metaui-polish-custom-")
  on.exit(unlink(root_custom, recursive = TRUE), add = TRUE)
  m_custom <- get_model_tibble()[1:2, ]
  m_custom$interval_type <- c("90% custom interval", "Confidence interval (coverage model-dependent)")
  m_custom$LCL[2] <- "NA_real_"
  m_custom$UCL[2] <- "NA_real_"
  generate_shiny(d, "Custom interval app", save_to_folder = root_custom, launch_app = FALSE, models = m_custom)
  setwd(root_custom)
  env_custom <- new.env(parent = globalenv()); sys.source("global.R", env_custom)
  shiny::testServer(env_custom$server, {
    session$setInputs(outliers_z_scores = c(-10, 10), metaUI__filter_Year = c(2001, 2008),
      metaUI__filter_Year_include_NA = TRUE, computation = "primary", go = 1)
    custom_html <- output$computation_comparison
    expect_match(custom_html, ">Interval</th>", fixed = TRUE)
    expect_false(grepl(">95% CI</th>", custom_html, fixed = TRUE))
    expect_match(custom_html, "90% custom interval", fixed = TRUE)
    expect_match(custom_html, "Not estimated:", fixed = TRUE)
    expect_false(grepl("[NA", custom_html, fixed = TRUE))
  })
})

test_that("LOO refit button avoids double-escaping and ratio help text uses null = 1", {
  skip_without_app_packages()
  root_or <- tempfile("metaui-polish-or-")
  on.exit(unlink(root_or, recursive = TRUE))
  x_or <- data.frame(
    study = rep(paste0("S", 1:10), each = 2),
    id = rep(1:2, 10),
    log_or = c(0.1, 0.2, -0.1, 0.3, 0.4, 0.2, 0.0, 0.5, 0.2, 0.1, 0.3, 0.2, -0.2, 0.1, 0.4, 0.3, 0.2, 0.1, 0.0, 0.2),
    v_log = rep(0.04, 20)
  )
  d_or <- prepare_data(x_or, "study", "log_or", variance = "v_log", es_id = "id", es_type = "OR", variance_scale = "log")
  m_or <- get_model_tibble()[1:2, ]
  m_or$name[2] <- "RVE & CR2"
  generate_shiny(d_or, "OR LOO polish", save_to_folder = root_or, launch_app = FALSE,
    models = m_or, primary_model = "RVE & CR2")
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(root_or)
  env_or <- new.env(parent = globalenv()); sys.source("global.R", env_or)
  shiny::testServer(env_or$server, {
    # Force refit offer instead of auto-refit so the button actually renders.
    assign("loo_quick_seconds", -1, envir = environment(loo_run_refits))
    assign("loo_few_seconds", -1, envir = environment(loo_run_refits))
    session$setInputs(outliers_z_scores = c(-10, 10), go = 1)
    panel <- as.character(output$loo_panel$html)
    expect_match(panel, 'id="loo_refit"', fixed = TRUE)
    expect_match(panel, "Refit RVE &amp; CR2 without each study", fixed = TRUE)
    expect_false(grepl("&amp;amp;", panel, fixed = TRUE))
    expect_match(panel, "close to 1", fixed = TRUE)
    expect_false(grepl("close to 0", panel, fixed = TRUE))
  })
})
