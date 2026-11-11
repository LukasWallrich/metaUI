# Odds and risk ratios: fitted as logs, shown as ratios. Fixture: the BCG vaccine trials
# (Colditz et al., 1994; metadat::dat.bcg), copied here so that no extra package is needed.
bcg <- function() data.frame(
  trial = 1:13,
  author = c("Aronson", "Ferguson & Simes", "Rosenthal et al", "Hart & Sutherland", "Frimodt-Moller et al", "Stein & Aronson",
    "Vandiviere et al", "TPT Madras", "Coetzee & Berjak", "Rosenthal et al", "Comstock et al", "Comstock & Webster", "Comstock et al"),
  year = c(1948, 1949, 1960, 1977, 1973, 1953, 1973, 1980, 1968, 1961, 1974, 1969, 1976),
  tpos = c(4, 6, 3, 62, 33, 180, 8, 505, 29, 17, 186, 5, 27),
  tneg = c(119, 300, 228, 13536, 5036, 1361, 2537, 87886, 7470, 1699, 50448, 2493, 16886),
  cpos = c(11, 29, 11, 248, 47, 372, 10, 499, 45, 65, 141, 3, 29),
  cneg = c(128, 274, 209, 12619, 5761, 1079, 619, 87892, 7232, 1600, 27197, 2338, 17825),
  ablat = c(44, 55, 42, 52, 13, 44, 19, 13, 27, 42, 18, 33, 33),
  alloc = factor(c("random", "random", "random", "random", "alternate", "alternate", "random", "random",
    "random", "systematic", "systematic", "systematic", "systematic")))
bcg_counts <- c(ai = "tpos", bi = "tneg", ci = "cpos", di = "cneg")
bcg_prepared <- function(es_type = "OR", ...) suppressMessages(prepare_data(bcg(), "trial", counts = bcg_counts, es_type = es_type, ...))

test_that("counts give metafor::escalc's log ratios, including its correction for zero cells", {
  for (measure in c("OR", "RR")) {
    d <- bcg_prepared(measure)
    e <- metafor::escalc(measure, ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg())
    expect_equal(d$metaUI__effect_size, as.numeric(e$yi), tolerance = 1e-12)
    expect_equal(d$metaUI__variance, as.numeric(e$vi), tolerance = 1e-12)
    expect_equal(d$metaUI__display_scale[1], measure)
    expect_true(all(c("metaUI__ai", "metaUI__bi", "metaUI__ci", "metaUI__di") %in% names(d)))
  }
  # Group totals instead of non-events; a zero cell gets 0.5 added to every cell, as in metafor.
  x <- data.frame(id = 1:3, e1 = c(0, 4, 10), n1 = c(20, 30, 40), e2 = c(3, 0, 12), n2 = c(25, 30, 40))
  d <- prepare_data(x, "id", counts = c(ai = "e1", n1i = "n1", ci = "e2", n2i = "n2"), es_type = "OR")
  e <- metafor::escalc("OR", ai = e1, n1i = n1, ci = e2, n2i = n2, data = x)
  expect_equal(d$metaUI__effect_size, as.numeric(e$yi), tolerance = 1e-12)
  expect_equal(d$metaUI__effect_size[1], log((.5 * 22.5) / (20.5 * 3.5)))
  # Log ratios supplied directly give the same prepared data.
  e <- metafor::escalc("OR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg())
  direct <- prepare_data(as.data.frame(e), "trial", "yi", variance = "vi", es_type = "OR", variance_scale = "log")
  expect_equal(as.numeric(direct$metaUI__effect_size), bcg_prepared()$metaUI__effect_size)
  expect_error(prepare_data(as.data.frame(e), "trial", "yi", variance = "vi", es_type = "OR"), "log ratios")
  expect_error(prepare_data(bcg(), "trial", "tpos", variance = "ablat", counts = bcg_counts, es_type = "OR"), "leave es_field")
  expect_error(prepare_data(bcg(), "trial", counts = c(ai = "tpos", ci = "cpos"), es_type = "OR"), "counts must name")
  # Impossible counts exclude their row with a reason.
  bad <- x; bad$e1[2] <- 40
  expect_warning(d <- prepare_data(bad, "id", counts = c(ai = "e1", n1i = "n1", ci = "e2", n2i = "n2"), es_type = "RR"), "1 rows excluded")
  expect_match(attr(d, "metaUI_validation")$exclusions$reason, "impossible counts")
})

test_that("ratio estimates match metafor on the log scale and are shown exponentiated", {
  skip_without_app_packages()
  d <- bcg_prepared("OR", direction = "negative")
  result <- metaUI:::metaUI_fit_models(d, get_model_tibble())
  table <- result$table
  ml <- metafor::rma.mv(d$metaUI__effect_size, d$metaUI__variance, random = ~ 1 | metaUI__study_id/metaUI__effect_id,
    data = d, test = "t", dfs = "contain", method = "REML", sparse = TRUE)
  row <- table[table$Model == "Random-Effects Multilevel Model", ]
  expect_equal(row$fit_es, as.numeric(ml$b), tolerance = 1e-8)
  expect_equal(row$fit_LCL, ml$ci.lb, tolerance = 1e-8)
  expect_equal(row$fit_UCL, ml$ci.ub, tolerance = 1e-8)
  pred <- predict(ml)
  expect_equal(c(row$es, row$LCL, row$UCL, row$PI_LCL, row$PI_UCL), exp(c(ml$b, ml$ci.lb, ml$ci.ub, pred$pi.lb, pred$pi.ub)), tolerance = 1e-8)
  expect_equal(row$LCL90, exp(predict(ml, level = 90)$ci.lb), tolerance = 1e-8)
  # Every default model is estimated on the log scale, all shown as ratios.
  expect_true(all(table$status == "ok"), info = paste(table$Model, table$reason))
  expect_equal(table$es, exp(table$fit_es))
  # Study-level univariate model (funnel, Egger), as metafor fits it.
  agg <- result$df_agg
  uni <- metaUI:::metaUI_univariate(agg$metaUI__effect_size, agg$metaUI__se)
  ref <- metafor::rma.uni(yi = agg$metaUI__effect_size, sei = agg$metaUI__se, method = "ML", test = "knha")
  expect_equal(as.numeric(uni$b), as.numeric(ref$b), tolerance = 1e-8)
  # Without a direction, the selection models say why; p-curve estimates stay d-only.
  unspecified <- metaUI:::metaUI_fit_models(bcg_prepared("RR"), get_model_tibble())$table
  expect_equal(unspecified$status[unspecified$Model == "P-uniform star"], "unsupported")
  expect_equal(metaUI:::metaUI_reader_reason(metaUI:::metaUI_model_reason(list(name = "P-Curve (first value)", code = metaUI:::metaUI_default_code[["P-Curve (first value)"]]), d)),
    "Available only for standardized mean differences.")
  # P-curve's tests work from the Wald z of the log ratios.
  pc <- metaUI:::metaUI_pcurve_data(d)
  expect_equal(pc$TE / pc$seTE, (-d$metaUI__effect_size / d$metaUI__se)[match(pc$studlab, d$metaUI__study_id)])
})

test_that("ratio bounds are symmetric on the log scale", {
  table <- data.frame(Model = c("a", "b", "c", "d"), status = "ok", LCL90 = c(.85, 1.3, .5, .4), UCL90 = c(1.2, 1.5, .7, 1.1))
  a <- metaUI:::metaUI_interval_assessment(table, 1.25, "OR")
  expect_equal(a$assessment, c("Interval within bounds", "Exceeds upper bound", "Exceeds lower bound", "Inconclusive"))
  expect_equal(a$threshold, c(1.2, 1.5, 2, 2.5))
  expect_equal(metaUI:::metaUI_interval_assessment(table, .8, "OR")$assessment, a$assessment)
  expect_error(metaUI:::metaUI_interval_assessment(table, 1, "RR"), "ratio")
  expect_equal(metaUI:::metaUI_bounds(.8, TRUE), c(.8, 1.25))
})

test_that("ratio axes are drawn on the log scale around 1", {
  scale <- metaUI:::metaUI_track_scale(c(.25, 4), ratio = TRUE)
  expect_equal(scale$at(1), 50)
  expect_equal(scale$at(.5) - scale$at(.25), scale$at(4) - scale$at(2))
  expect_true(all(c(.5, 1, 2) %in% scale$ticks))
  expect_match(metaUI:::metaUI_track_grid(scale), "metaui-track-zero\" style=\"left:50%")
  html <- metaUI:::metaUI_estimates_html(data.frame(Model = "m", es = .5, LCL = .3, UCL = .8, k = 3, status = "ok", reason = "", warnings = "",
    aggregated = FALSE, interval_type = "Confidence interval"), axis_title = "Odds ratio", bound = 1.25, ratio = TRUE)
  expect_match(html, "metaui-track-band")
  expect_true(all(metaUI:::metaUI_ratio_ticks(log(c(.19, 1.56))) %in% c(.2, .3, .5, .7, 1, 1.5)))
  plot <- metaUI:::metaUI_forest_plot(data.frame(label = c("a", "b"), kind = "Selected effect", study = c("a", "b"),
    es = c(.5, 2), LCL = c(.25, 1), UCL = c(1, 4)), "OR")
  # Shaded study bands start at 0, which the log axis takes to its left edge (with a note that log(0) is infinite).
  built <- suppressWarnings(ggplot2::ggplot_build(plot))
  expect_equal(built$layout$panel_params[[1]]$x$get_labels()[built$layout$panel_params[[1]]$x$get_breaks() %in% log(1)], "1")
})

test_that("an uploaded workbook recomputes ratio effects from edited counts", {
  d <- bcg_prepared("RR")
  roles <- metaUI:::metaUI_upload_roles(d)
  expect_equal(roles$role[roles$column %in% c("metaUI__ai", "metaUI__di")], c("Edit", "Edit"))
  expect_equal(roles$role[roles$column == "metaUI__input_effect"], "Derived")
  upload <- as.data.frame(d)
  upload$metaUI__ai[1] <- 8
  upload$metaUI__input_effect <- NA
  out <- metaUI:::metaUI_validate_upload(upload, d)
  expect_equal(out$metaUI__effect_size[1], as.numeric(metafor::escalc("RR", ai = 8, bi = 119, ci = 11, di = 128)$yi))
  upload$metaUI__ai[1] <- -1
  expect_error(metaUI:::metaUI_validate_upload(upload, d), "Counts must not be negative")
})

test_that("a ratio app is generated and parses", {
  skip_without_app_packages()
  path <- tempfile("ratio-app")
  d <- bcg_prepared("OR", filters = c(Allocation = "alloc", Latitude = "ablat"), direction = "negative")
  suppressMessages(generate_shiny(d, "BCG", save_to_folder = path, launch_app = FALSE))
  for (file in c("ui.R", "server.R", "global.R", "labels_and_options.R")) expect_silent(parse(file.path(path, file)))
  expect_match(paste(readLines(file.path(path, "ui.R")), collapse = "\n"), "Smallest ratio of interest", fixed = TRUE)
  expect_match(paste(readLines(file.path(path, "global.R")), collapse = "\n"), "Odds ratio", fixed = TRUE)
  expect_true(file.exists(file.path(path, "opening_results.rds")))
})

test_that("double-zero tables are kept by default or excluded on request, and N is checked against the counts", {
  x <- data.frame(id = 1:4, e1 = c(0, 4, 10, 2), n1 = c(20, 30, 40, 50), e2 = c(0, 1, 12, 3), n2 = c(25, 30, 40, 50))
  counts <- c(ai = "e1", n1i = "n1", ci = "e2", n2i = "n2")
  kept <- prepare_data(x, "id", counts = counts, es_type = "OR")
  expect_equal(nrow(kept), 4L)
  expect_equal(kept$metaUI__effect_size, as.numeric(metafor::escalc("OR", ai = e1, n1i = n1, ci = e2, n2i = n2, data = x)$yi))
  expect_equal(kept$metaUI__N, x$n1 + x$n2)
  expect_match(metaUI:::metaUI_double_zero_text(kept), "were kept, as metafor does by default \\(1 in these data\\)")
  expect_warning(dropped <- prepare_data(x, "id", counts = counts, es_type = "OR", double_zero = "exclude"), "1 rows excluded")
  expect_equal(dropped$metaUI__study_id, 2:4)
  ref <- metafor::escalc("OR", ai = e1, n1i = n1, ci = e2, n2i = n2, data = x, drop00 = TRUE)
  expect_equal(dropped$metaUI__effect_size, as.numeric(ref$yi[-1]))
  expect_match(metaUI:::metaUI_double_zero_text(dropped), "left out, as the authors chose \\(1 table\\)")
  expect_error(prepare_data(x, "id", "e1", variance = "n1", es_type = "OR", variance_scale = "log", double_zero = "exclude"), "only with counts")
  # Uploads keep the authors' rule and recompute N from the counts.
  upload <- as.data.frame(dropped); upload$metaUI__ai[1] <- 0; upload$metaUI__ci[1] <- 0
  expect_error(metaUI:::metaUI_validate_upload(upload, dropped), "excluded tables with no events")
  upload <- as.data.frame(kept); upload$metaUI__n1i[2] <- 60
  expect_equal(metaUI:::metaUI_validate_upload(upload, kept)$metaUI__N[2], 90)
  roles <- metaUI:::metaUI_upload_roles(kept)
  expect_equal(roles$role[roles$column == "metaUI__N"], "Derived")
  # A supplied N that differs from the count total is named, not silently used.
  x$N <- x$n1 + x$n2; x$N[3] <- 100
  expect_warning(given <- prepare_data(x, "id", counts = counts, es_type = "OR", sample_size = "N"), "differs from the total of the 2x2 counts in 1 row \\(rows 3\\)")
  expect_equal(given$metaUI__N[3], 100)
  expect_equal(attr(given, "metaUI_validation")$optional_inputs$N_differs_from_counts, 1L)
  expect_equal(metaUI:::metaUI_upload_roles(given)$role[metaUI:::metaUI_upload_roles(given)$column == "metaUI__N"], "Edit")
})

test_that("generated apps hold every helper that ratio uploads and Methods need", {
  # The app's own copies, without the metaUI namespace behind them.
  env <- new.env(parent = baseenv())
  for (file in c("helpers.R", "analysis.R", "computations.R", "selection_links.R", "code_export.R", "bayesian.R"))
    sys.source(system.file("template_code", file, package = "metaUI"), env)
  x <- data.frame(id = 1:3, a = c(0, 4, 100), b = c(20, 26, 0), c = c(0, 6, 100), d = c(25, 24, 0))
  d <- prepare_data(x, "id", counts = c(ai = "a", bi = "b", ci = "c", di = "d"), es_type = "RR")
  expect_match(env$metaUI_double_zero_text(d), "were kept, as metafor does by default \\(2 in these data\\)")
  expect_match(env$metaUI_double_zero_text(d), "events in everyone gives a risk ratio near 1 that can have a narrow interval")
  upload <- as.data.frame(d); upload$metaUI__ai[2] <- 8
  out <- env$metaUI_validate_upload(upload, d)
  expect_equal(out$metaUI__effect_size[2], as.numeric(metafor::escalc("RR", ai = 8, bi = 26, ci = 6, di = 24)$yi))
  # An all-events RR table is precise; an all-events OR table is not.
  expect_lt(d$metaUI__variance[3], .001)
  expect_gt(prepare_data(x, "id", counts = c(ai = "a", bi = "b", ci = "c", di = "d"), es_type = "OR")$metaUI__variance[3], 1)
})

test_that("diamonds peak at the estimate on a log axis", {
  scale <- metaUI:::metaUI_track_scale(c(.25, 4), ratio = TRUE)
  for (hollow in c(FALSE, TRUE)) {
    html <- metaUI:::metaUI_track_row(scale, 1, .25, 4, diamond = TRUE, hollow = hollow)
    expect_match(html, if (hollow) "points=\"0.8,5 50.0,0.8" else "polygon\\(0 50%,50.0% 0")
  }
})

test_that("a JSON configuration can map 2x2 counts", {
  skip_without_app_packages()
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  utils::write.csv(bcg(), file.path(root, "bcg.csv"), row.names = FALSE)
  config <- list(schema_version = 2, data = "bcg.csv", dataset_name = "BCG", output = "app",
    mapping = list(study_label = "trial", es_type = "RR", counts = list(ai = "tpos", bi = "tneg", ci = "cpos", di = "cneg"), double_zero = "exclude"))
  path <- file.path(root, "config.json"); jsonlite::write_json(config, path, auto_unbox = TRUE)
  suppressMessages(build_app(path))
  built <- readRDS(file.path(root, "app", "dataset.rds"))
  expect_equal(built$metaUI__effect_size, bcg_prepared("RR")$metaUI__effect_size)
  expect_identical(attr(built, "metaUI_validation")$mapping$double_zero, "exclude")
  config$schema_version <- 1; jsonlite::write_json(config, path, auto_unbox = TRUE)
  expect_error(build_app(path, overwrite = TRUE), "schema_version 2")
  config$schema_version <- 2; config$mapping$counts <- NULL; jsonlite::write_json(config, path, auto_unbox = TRUE)
  expect_error(build_app(path, overwrite = TRUE), "es_field or counts")
})
