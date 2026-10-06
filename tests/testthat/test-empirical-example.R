test_that("CC-BY example retains exact native-scale membership and reference fit", {
  root <- system.file("examples", "dannheim", package = "metaUI")
  x <- read.csv(file.path(root, "mental-health.csv"))
  provenance <- jsonlite::read_json(file.path(root, "provenance.json"), simplifyVector = TRUE)
  expect_setequal(x$row_uid, provenance$member_ids)
  expect_equal(nrow(x), 12)
  expect_equal(length(unique(x$study)), 12)
  expect_equal(x$se_g, (x$ci_upper - x$ci_lower) / (2 * qnorm(.975)), tolerance = 1e-11)
  d <- prepare_data(x, "study", "g", se = "se_g", es_id = "row_uid", es_type = "SMD")
  expect_identical(d$metaUI__effect_size, x$g)
  expect_true(all(is.na(d$metaUI__N)) && all(is.na(d$metaUI__pvalue)))
  env <- new.env(); sys.source(file.path(root, "models.R"), env)
  expect_equal(env$models_to_run[1:7, ], get_model_tibble())
  source_spec <- env$models_to_run[8, ]
  actual <- metaUI:::metaUI_fit_models(d, source_spec)$table
  reference <- metafor::rma(yi = x$g, sei = x$se_g, method = "REML", test = "knha")
  expect_identical(actual$status, "ok")
  expect_equal(actual$fit_es, as.numeric(reference$b), tolerance = 1e-8)
  expect_equal(actual$fit_LCL, reference$ci.lb, tolerance = 1e-8)
  expect_equal(actual$fit_UCL, reference$ci.ub, tolerance = 1e-8)
  expect_lt(abs(actual$es - as.numeric(provenance$gate$replicated_r)), 5e-5)
  # Printed precision, not a claim of equality to unavailable unrounded inputs.
  expect_lt(abs(actual$es - (-.38)), as.numeric(provenance$gate$rounding_tol))
  expect_lt(abs(actual$LCL - (-.69)), .005)
  expect_lt(abs(actual$UCL - (-.08)), .005)
  repeated <- x; repeated$study[2] <- repeated$study[1]
  repeated <- prepare_data(repeated, "study", "g", se = "se_g", es_id = "row_uid")
  failed <- metaUI:::metaUI_fit_models(repeated, source_spec)$table
  expect_identical(failed$status, "failed")
  expect_match(failed$reason, "one effect per independent study")
})

test_that("model-file generation neither leaks state nor accepts stale globals", {
  path <- tempfile(); on.exit(unlink(path, recursive = TRUE))
  file <- tempfile(fileext = ".R"); on.exit(unlink(file), add = TRUE)
  writeLines("# deliberately no models_to_run", file)
  old <- get0("models_to_run", envir = globalenv(), inherits = FALSE)
  assign("models_to_run", get_model_tibble(), envir = globalenv())
  on.exit(if (is.null(old)) rm("models_to_run", envir = globalenv()) else assign("models_to_run", old, envir = globalenv()), add = TRUE)
  expect_error(generate_shiny(prepared(), "invalid", models = file, save_to_folder = path, launch_app = FALSE), "does not create")
  expect_identical(get("models_to_run", envir = globalenv()), get_model_tibble())
})
