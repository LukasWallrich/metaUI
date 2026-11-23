test_that("unweighted (BHHR) and GLS aggregation match metafor::aggregate.escalc exactly", {
  coles_path <- system.file("extdata", "coles2019.csv", package = "metaUI")
  coles <- utils::read.csv(coles_path)
  d <- suppressMessages(prepare_data(coles, "study_id", "yi", variance = "vi", es_id = "es_id", direction = "positive"))
  es <- metafor::escalc(yi = yi, vi = vi, data = coles)

  for (rho in c(0, 0.5, 0.6, 0.8)) {
    # Unweighted BHHR vs metafor::aggregate.escalc(..., weighted = FALSE)
    u <- metaUI:::metaUI_aggregate(d, correlation = rho, method = "unweighted")
    ref_u <- as.data.frame(metafor::aggregate.escalc(es, cluster = study_id, struct = "CS", rho = rho, weighted = FALSE))
    ref_u <- ref_u[match(as.character(u$metaUI__study_id), as.character(ref_u$study_id)), ]
    expect_equal(u$metaUI__effect_size, ref_u$yi, tolerance = 1e-12)
    expect_equal(u$metaUI__variance, ref_u$vi, tolerance = 1e-12)
    expect_equal(u$metaUI__se, sqrt(ref_u$vi), tolerance = 1e-12)
    expect_null(attr(u, "metaUI_aggregation_diagnostics"))

    # Every BHHR composite stays within its study's observed effect range.
    ranges <- lapply(split(d$metaUI__effect_size, as.character(d$metaUI__study_id)), range)
    lo <- vapply(ranges[as.character(u$metaUI__study_id)], `[`, numeric(1), 1)
    hi <- vapply(ranges[as.character(u$metaUI__study_id)], `[`, numeric(1), 2)
    expect_true(all(u$metaUI__effect_size >= lo - 1e-12 & u$metaUI__effect_size <= hi + 1e-12))

    # Inverse-covariance GLS vs metafor::aggregate.escalc(..., weighted = TRUE)
    g <- metaUI:::metaUI_aggregate(d, correlation = rho, method = "gls")
    ref_g <- as.data.frame(metafor::aggregate.escalc(es, cluster = study_id, struct = "CS", rho = rho, weighted = TRUE))
    ref_g <- ref_g[match(as.character(g$metaUI__study_id), as.character(ref_g$study_id)), ]
    expect_equal(g$metaUI__effect_size, ref_g$yi, tolerance = 1e-11)
    expect_equal(g$metaUI__variance, ref_g$vi, tolerance = 1e-11)
    expect_equal(g$metaUI__se, sqrt(ref_g$vi), tolerance = 1e-11)
    expect_error(metaUI:::metaUI_aggregate(d, correlation = rho, method = "aggregate"), "Unknown aggregation method")
  }

  # Equal variances: BHHR and GLS coincide exactly.
  eq <- data.frame(study = rep(c("S1", "S2"), each = 3), id = 1:6, yi = c(.1, .4, .7, -.2, .2, .6), vi = rep(.04, 6))
  d_eq <- prepare_data(eq, "study", "yi", variance = "vi", es_id = "id")
  u_eq <- metaUI:::metaUI_aggregate(d_eq, .6, "unweighted")
  g_eq <- metaUI:::metaUI_aggregate(d_eq, .6, "gls")
  expect_equal(u_eq$metaUI__effect_size, g_eq$metaUI__effect_size, tolerance = 1e-12)
  expect_equal(u_eq$metaUI__variance, g_eq$metaUI__variance, tolerance = 1e-12)
  expect_identical(attr(g_eq, "metaUI_aggregation_diagnostics")$n_negative_weights, 0L)

  # First-effect rule retains first row per study untouched.
  f <- metaUI:::metaUI_aggregate(d_eq, .6, "first")
  expect_equal(f$metaUI__effect_size, c(.1, -.2))
  expect_equal(f$metaUI__variance, c(.04, .04))
  expect_null(attr(f, "metaUI_aggregation_diagnostics"))
})

test_that("GLS negative-weight diagnostics flag Coles Study 117 and Aksayli BHHR trim-and-fill converges", {
  skip_without_app_packages()
  coles <- utils::read.csv(system.file("extdata", "coles2019.csv", package = "metaUI"))
  d_coles <- suppressMessages(prepare_data(coles, "study_id", "yi", variance = "vi", es_id = "es_id", direction = "positive"))

  g8 <- metaUI:::metaUI_aggregate(d_coles, correlation = 0.8, method = "gls")
  diag8 <- attr(g8, "metaUI_aggregation_diagnostics")
  expect_identical(diag8$multi_effect_studies, 61L)
  expect_identical(diag8$n_negative_weights, 11L)
  expect_identical(diag8$n_out_of_range, 7L)
  expect_true("117" %in% diag8$negative_weight_studies)
  expect_false("117" %in% diag8$out_of_range_studies)
  expect_lt(diag8$min_weight, 0)
  expect_match(metaUI:::metaUI_gls_note(diag8), "11 of 61 multi-effect studies", fixed = TRUE)
  expect_match(metaUI:::metaUI_gls_note(diag8), "7 combined estimates lie outside their studies\u2019 observed effects", fixed = TRUE)
  # Tiny negative weights stay nonzero rather than rounding to -0.00.
  tiny_note <- metaUI:::metaUI_gls_note(list(multi_effect_studies = 2L, n_negative_weights = 1L, n_out_of_range = 1L, min_weight = -0.00123))
  expect_match(tiny_note, "1 combined estimate lies outside its study\u2019s observed effects", fixed = TRUE)
  expect_match(tiny_note, "\u22120.0012", fixed = TRUE)

  # Study 117: BHHR gives +0.734; GLS at rho = 0.8 flips sign to -1.324.
  u8 <- metaUI:::metaUI_aggregate(d_coles, correlation = 0.8, method = "unweighted")
  expect_equal(u8$metaUI__effect_size[as.character(u8$metaUI__study_id) == "117"], 0.7343598, tolerance = 1e-5)
  expect_equal(g8$metaUI__effect_size[as.character(g8$metaUI__study_id) == "117"], -1.323668, tolerance = 1e-5)

  # Aksayli 2019 under default BHHR: default ML Fisher scoring fails to converge,
  # while metaUI_univariate() and metaUI_trimfill() recover cleanly via the damped retry.
  aksayli <- utils::read.csv(system.file("extdata", "aksayli2019.csv", package = "metaUI"))
  d_aks <- suppressMessages(prepare_data(aksayli, "study_id", "yi", variance = "vi", es_id = "es_id", direction = "positive"))
  agg_aks <- metaUI:::metaUI_aggregate(d_aks, correlation = 0.6, method = "unweighted")
  expect_error(
    metafor::rma.uni(yi = agg_aks$metaUI__effect_size, sei = agg_aks$metaUI__se, method = "ML", test = "knha"),
    "did not converge"
  )
  uni_aks <- metaUI:::metaUI_univariate(agg_aks$metaUI__effect_size, agg_aks$metaUI__se, method = "ML")
  ref_aks <- metafor::rma.uni(yi = agg_aks$metaUI__effect_size, sei = agg_aks$metaUI__se, method = "ML", test = "knha",
    control = list(stepadj = .5, maxiter = 1000))
  expect_equal(as.numeric(uni_aks$b), as.numeric(ref_aks$b), tolerance = 1e-10)
  expect_equal(uni_aks$tau2, ref_aks$tau2, tolerance = 1e-10)
  tf_aks <- metaUI:::metaUI_fit_models(d_aks, get_model_tibble()[3, ], correlation = 0.6, aggregation = "unweighted")$table
  tf_ref <- metaUI:::metaUI_trimfill(agg_aks, control = list(stepadj = .5, maxiter = 1000))
  expect_identical(tf_aks$status, "ok")
  expect_equal(tf_aks$es, 0.261, tolerance = 1e-2)
  expect_equal(tf_aks$es, as.numeric(tf_ref$b), tolerance = 1e-10)

  # Leave-one-out shortcut rejects metaUI_trimfill() even when zero studies are imputed (k0 == 0).
  sym <- prepare_data(data.frame(study = paste0("S", 1:6), yi = c(.1, .5, .2, .4, .3, .3), vi = c(.02, .02, .04, .04, .06, .06)),
    "study", "yi", variance = "vi")
  tf0 <- metaUI:::metaUI_trimfill(sym)
  expect_equal(tf0$k0, 0)
  loo0 <- metaUI:::metaUI_loo_blocks(tf0, sym, c(as.numeric(tf0$b), tf0$ci.lb, tf0$ci.ub))
  expect_false(is.null(loo0$reason))
})

test_that("options$aggregation_method works through generate_shiny, build_app, server UI, validation.json, and code export", {
  skip_on_cran()
  skip_without_app_packages()
  # Construct synthetic data where study S1 has unequal variances triggering negative GLS weight at rho = 0.8.
  raw <- data.frame(
    study = c("S1", "S1", rep(paste0("S", 2:8), each = 2)),
    id = 1:16,
    yi = c(0.0, 2.0, 0.2, 0.3, 0.1, 0.4, 0.2, 0.5, 0.3, 0.2, 0.1, 0.3, 0.4, 0.5, 0.2, 0.3),
    vi = c(0.01, 0.25, rep(c(0.03, 0.04), 7))
  )
  d <- prepare_data(raw, "study", "yi", variance = "vi", es_id = "id", direction = "positive")

  # Invalid aggregation_method fails fast before creating destination directory.
  bad_dir <- tempfile("metaui-bad-agg-")
  on.exit(unlink(bad_dir, recursive = TRUE), add = TRUE)
  expect_error(
    generate_shiny(d, "Bad agg", save_to_folder = bad_dir, launch_app = FALSE,
      options = list(aggregation_method = "median")),
    'options\\$aggregation_method must be one of "unweighted", "gls", or "first"'
  )
  expect_false(dir.exists(bad_dir))

  # Default build ("unweighted"): no negative-weight warning, records BHHR in validation.json and UI.
  dir_bhhr <- tempfile("metaui-agg-bhhr-")
  on.exit(unlink(dir_bhhr, recursive = TRUE), add = TRUE)
  expect_warning(
    generate_shiny(d, "BHHR app", save_to_folder = dir_bhhr, launch_app = FALSE,
      models = get_model_tibble()[c(1, 3, 6), ], bayesian = list(enabled = TRUE),
      options = list(correlation_dependent = 0.8)),
    NA
  )
  rep_bhhr <- jsonlite::read_json(file.path(dir_bhhr, "validation.json"))
  expect_identical(rep_bhhr$aggregation$method, "unweighted")
  expect_match(rep_bhhr$aggregation$label, "Borenstein et al., 2009", fixed = TRUE)
  expect_null(rep_bhhr$aggregation$diagnostics)
  expect_match(rep_bhhr$bayesian$aggregation, "Borenstein et al., 2009", fixed = TRUE)

  # Opt-in GLS build: warns on negative weight in S1, records diagnostics, surfaces calm note in Methods & Forest.
  dir_gls <- tempfile("metaui-agg-gls-")
  on.exit(unlink(dir_gls, recursive = TRUE), add = TRUE)
  expect_warning(
    generate_shiny(d, "GLS app", save_to_folder = dir_gls, launch_app = FALSE,
      models = get_model_tibble()[c(1, 3, 6), ], bayesian = list(enabled = TRUE),
      options = list(correlation_dependent = 0.8, aggregation_method = "gls")),
    "GLS study-level aggregation assigns negative weights"
  )
  rep_gls <- jsonlite::read_json(file.path(dir_gls, "validation.json"))
  expect_identical(rep_gls$aggregation$method, "gls")
  expect_identical(rep_gls$aggregation$diagnostics$n_negative_weights, 1L)
  expect_identical(rep_gls$aggregation$diagnostics$n_out_of_range, 1L)
  # Single-study lists stay JSON arrays (not unboxed scalars).
  expect_true(is.list(rep_gls$aggregation$diagnostics$negative_weight_studies))
  expect_identical(unlist(rep_gls$aggregation$diagnostics$negative_weight_studies), "S1")
  expect_true(is.list(rep_gls$aggregation$diagnostics$out_of_range_studies))
  expect_match(rep_gls$bayesian$aggregation, "GLS", fixed = TRUE)

  # Server UI checks for BHHR and GLS + provenance + standalone code export parity.
  check_server <- function(app_dir, expected_method, expect_gls_warning) {
    wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(app_dir)
    env <- new.env(parent = globalenv())
    sys.source("global.R", env)
    expect_identical(env$aggregation_method[1], expected_method)
    run_dir <- tempfile("metaui-agg-export-"); dir.create(run_dir)
    on.exit(unlink(run_dir, recursive = TRUE), add = TRUE)
    shiny::testServer(env$server, {
      session$setInputs(outliers_z_scores = c(-10, 10), go = 1)
      scale_html <- as.character(output$scale_note$html)
      if (expected_method == "unweighted") {
        expect_match(scale_html, "unweighted average (Borenstein et al., 2009)", fixed = TRUE)
        expect_false(grepl("negative weights", scale_html, fixed = TRUE))
        expect_error(output$forest_aggregation_note, class = "shiny.silent.error")
        expect_match(as.character(env$forest_note), "unweighted study-level averages (Borenstein et al., 2009)", fixed = TRUE)
      } else {
        expect_match(scale_html, "inverse-covariance GLS average", fixed = TRUE)
        expect_match(scale_html, "negative weights to less-precise effects in 1 of 8", fixed = TRUE)
        forest_warn <- as.character(output$forest_aggregation_note$html)
        expect_match(forest_warn, "negative weights to less-precise effects in 1 of 8", fixed = TRUE)
        expect_match(as.character(env$forest_note), "inverse-covariance GLS", fixed = TRUE)
      }
      prov <- data_list()$provenance
      expect_identical(prov$value[prov$field == "aggregation_method"], expected_method)
      expect_identical(prov$value[prov$field == "correlation_dependent"], "0.8")

      # Verify exported R script reproduces exact model rows without metaUI.
      app_est <- estimatesfiltered()
      wb <- file.path(run_dir, "analysis-metaUIdata.xlsx")
      file.copy(output$executeDownload, wb, overwrite = TRUE)
      script <- file.path(run_dir, "selection.R")
      file.copy(output$executeCodeDownload, script, overwrite = TRUE)
      code_txt <- paste(readLines(script), collapse = "\n")
      unwrapped <- gsub("\n#\\s*", " ", code_txt)
      expect_match(code_txt, paste0('aggregation_method <- "', expected_method, '"'), fixed = TRUE)
      if (expected_method == "unweighted") {
        expect_match(unwrapped, "unweighted average (Borenstein et al., 2009)", fixed = TRUE)
      } else {
        expect_match(unwrapped, "inverse-covariance GLS average", fixed = TRUE)
      }
      out_rds <- file.path(run_dir, "out.rds")
      writeLines(c(
        sprintf("source(%s)", deparse(script)),
        sprintf("saveRDS(list(estimates = estimates, agreement = agreement, metaUI = 'metaUI' %%in%% loadedNamespaces()), %s)", deparse(out_rds))
      ), file.path(run_dir, "run.R"))
      old_env <- Sys.getenv(c("R_TESTS", "R_LIBS"), unset = NA)
      Sys.unsetenv("R_TESTS")
      Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
      wd2 <- setwd(run_dir)
      on.exit({
        setwd(wd2)
        for (nm in names(old_env)) if (is.na(old_env[[nm]])) Sys.unsetenv(nm) else do.call(Sys.setenv, as.list(old_env[nm]))
      }, add = TRUE)
      log <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", "run.R", "analysis-metaUIdata.xlsx"),
        stdout = TRUE, stderr = TRUE))
      expect_true(file.exists(out_rds), info = paste(log, collapse = "\n"))
      res <- readRDS(out_rds)
      expect_false(res$metaUI)
      expect_true(all(res$agreement$agrees), info = paste(log, collapse = "\n"))
      m_est <- res$estimates[match(app_est$Model, res$estimates$Model), ]
      expect_equal(m_est$es, app_est$es, tolerance = 1e-8)
    })
  }

  check_server(dir_bhhr, "unweighted", FALSE)
  check_server(dir_gls, "gls", TRUE)

  # build_app() JSON config supports options.aggregation_method and rejects invalid values.
  cfg_root <- tempfile("metaui-cfg-agg-"); dir.create(cfg_root)
  on.exit(unlink(cfg_root, recursive = TRUE), add = TRUE)
  utils::write.csv(raw, file.path(cfg_root, "data.csv"), row.names = FALSE)
  cfg <- list(
    schema_version = 1,
    data = "data.csv",
    output = "app-first",
    dataset_name = "JSON first aggregation",
    mapping = list(study_label = "study", es_field = "yi", variance = "vi", es_type = "SMD", es_id = "id"),
    options = list(correlation_dependent = 0.7, aggregation_method = "first")
  )
  cfg_path <- file.path(cfg_root, "config.json")
  jsonlite::write_json(cfg, cfg_path, auto_unbox = TRUE)
  build_app(cfg_path)
  rep_first <- jsonlite::read_json(file.path(cfg_root, "app-first", "validation.json"))
  expect_identical(rep_first$aggregation$method, "first")
  expect_equal(rep_first$aggregation$correlation, 0.7)

  cfg_bad <- cfg
  cfg_bad$data <- file.path(cfg_root, "data.csv")
  cfg_bad$output <- file.path(cfg_root, "app-bad")
  cfg_bad$options$aggregation_method <- "invalid"
  expect_error(build_app(cfg_bad), "options\\$aggregation_method")
  expect_false(dir.exists(cfg_bad$output))
})

test_that("forest plot orders numeric study IDs numerically while keeping text IDs alphabetical", {
  # Numeric study IDs: 1, 2, 10, 100 (not 1, 10, 100, 2)
  raw_num <- data.frame(
    study = c(100, 2, 10, 1, 10, 2),
    id = c(1, 1, 2, 1, 1, 2),
    yi = c(.3, .1, .5, .2, .4, .1),
    vi = rep(.02, 6)
  )
  d_num <- prepare_data(raw_num, "study", "yi", variance = "vi", es_id = "id")
  est <- data.frame(Model = character(), status = character(), aggregated = logical(), fit_es = numeric(), fit_LCL = numeric(),
    fit_UCL = numeric(), es = numeric(), LCL = numeric(), UCL = numeric())
  rows <- metaUI:::metaUI_forest_rows(d_num, est)
  expect_identical(metaUI:::metaUI_forest_order(rows), c("1", "2", "10", "100"))
  # Effects within study 10 also ordered numerically (1 before 2).
  s10 <- rows[rows$study == "10", ]
  expect_identical(s10$effect_id, c("1", "2"))

  # Ties in estimate sort preserve numeric study order.
  tied <- data.frame(study = c(10, 2, 1), id = 1:3, yi = c(.2, .2, .2), vi = rep(.02, 3))
  d_tied <- prepare_data(tied, "study", "yi", variance = "vi", es_id = "id")
  rows_tied <- metaUI:::metaUI_forest_rows(d_tied, est)
  expect_identical(metaUI:::metaUI_forest_order(rows_tied, sort = "estimate"), c("1", "2", "10"))

  # Pure text study IDs keep alphabetical order; mixed puts finite numbers before text.
  raw_mix <- data.frame(study = c("Zeta", "10", "Alpha", "2", "Inf"), id = 1:5, yi = rep(.2, 5), vi = rep(.02, 5))
  d_mix <- prepare_data(raw_mix, "study", "yi", variance = "vi", es_id = "id")
  rows_mix <- metaUI:::metaUI_forest_rows(d_mix, est)
  expect_identical(metaUI:::metaUI_forest_order(rows_mix), c("2", "10", "Alpha", "Inf", "Zeta"))
})
