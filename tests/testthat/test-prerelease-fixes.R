test_that("filter labels with quotes, brackets and symbols give working apps; ambiguous labels are refused", {
  skip_without_app_packages()
  x <- fixture()
  x$grp <- factor(rep(c("a", "b"), 8)); x$kind <- factor(rep(c("x", "y"), each = 8)); x$dose <- c(1:15, NA); x$share <- seq_len(16) / 16
  expect_error(prepared(x, filters = c("A B" = "grp", A_B = "kind")), "differ only in spaces or symbols")
  expect_error(prepared(x, filters = c(G = "grp", G = "kind")), "must be distinct")
  expect_error(prepared(x, filters = c("a`b" = "grp")), "backticks")
  x$none <- NA_real_
  expect_error(prepared(x, filters = "none"), "has no values")
  d <- prepared(x, filters = c("Author's group" = "grp", 'The "kind" (x+)' = "kind", "Dose (mg)" = "dose", "R&D %" = "share"))
  path <- tempfile()
  generate_shiny(d, "Awkward labels", save_to_folder = path, launch_app = FALSE)
  for (file in c("ui.R", "server.R", "global.R")) expect_silent(parse(file.path(path, file)))
  expect_identical(metaUI:::metaUI_filter_id("metaUI__filter_Dose (mg)"), "metaUI__filter_Dose__mg_")
})

test_that("Sample summaries of numeric filters cope with selections without values", {
  expect_identical(metaUI:::summarise_numeric(c(NA_real_, NA_real_), "Year")$Value[2], "No values")
  expect_identical(metaUI:::summarise_numeric(c(10, NA), "Year")$Value[2], "10 (—)")
})

test_that("moderator tests extend the primary model and use cluster-robust inference", {
  skip_if_not_installed("clubSandwich")
  mods <- ~ `metaUI__filter_g`
  che <- 'metafor::rma.mv(yi = metaUI__effect_size, V = metafor::vcalc(vi = metaUI__variance, cluster = metaUI__study_id, obs = metaUI__effect_id, rho = .6, data = df),
    random = ~ 1 | metaUI__study_id/metaUI__effect_id, test = "t", dfs = "contain", data = df, method = "REML")'
  expect_false(is.null(metaUI:::metaUI_moderator_code(che, mods)))
  wrapped <- metaUI:::metaUI_moderator_code(paste0("metafor::robust(", che, ", cluster = df$metaUI__study_id)"), mods)
  expect_identical(metaUI:::metaUI_call_name(wrapped[[1]]), "rma.mv")
  expect_null(metaUI:::metaUI_moderator_code("my_helper(df)", mods))
  x <- fixture(); x$g <- factor(rep(c("a", "b"), each = 8))
  d <- prepared(x, filters = c(g = "g"))
  fitted <- metaUI:::metaUI_moderator_fit(d, "metaUI__filter_g", list(name = "CHE", code = che))
  expect_identical(fitted$working, "primary")
  expect_true(fitted$robust)
  expect_s3_class(fitted$model, "robust.rma")
  levels <- metaUI:::metaUI_moderator_levels(fitted$model, c("a", "b"))
  expect_equal(levels$b[1], unname(fitted$model$b[1]))
  expect_equal(levels$b[2], sum(fitted$model$b))
  expect_identical(metaUI:::metaUI_moderator_fit(d, "metaUI__filter_g")$working, "default")
})

test_that("the fast HTZ omnibus test matches clubSandwich, also with many categories", {
  skip_if_not_installed("clubSandwich")
  set.seed(1)
  studies <- 40
  x <- data.frame(study = rep(seq_len(studies), each = 3), effect = seq_len(studies * 3))
  x$g <- factor(rep(rep(letters[1:10], length.out = studies), each = 3))
  x$yi <- rnorm(studies, sd = .2)[x$study] + rnorm(nrow(x), sd = .1)
  x$vi <- runif(nrow(x), .01, .05)
  fit <- metafor::rma.mv(yi, vi, mods = ~ g, random = ~ 1 | study/effect, data = x, test = "t", dfs = "contain")
  stock <- clubSandwich::Wald_test(fit, constraints = clubSandwich::constrain_zero(2:10),
    vcov = clubSandwich::vcovCR(fit, cluster = x$study, type = "CR2"), test = "HTZ")
  fast <- metaUI:::metaUI_moderator_omnibus(fit, x$study)
  expect_equal(fast$statistic, stock$Fstat, tolerance = 1e-8)
  expect_equal(fast$df_denom, stock$df_denom, tolerance = 1e-8)
  expect_equal(fast$p_value, stock$p_val, tolerance = 1e-8)
  names(x)[1:4] <- c("metaUI__study_id", "metaUI__effect_id", "metaUI__filter_g", "metaUI__effect_size")
  x$metaUI__variance <- x$vi
  fitted <- metaUI:::metaUI_moderator_fit(x, "metaUI__filter_g")
  expect_true(fitted$omnibus)
  expect_equal(fitted$model$QMp, fast$p_value, tolerance = 1e-6)
})

test_that("default models apply small-sample corrections", {
  skip_without_app_packages()
  d <- prepared(fixture()[1:6, ])
  expect_warning(rve <- metaUI:::metaUI_rve_fit(d), "below 4")
  expect_lt(rve$reg_table$dfs, 4)
  expect_equal(as.numeric(metaUI:::metaUI_multilevel_fit(d)$ddf), 2) # three studies: containment df = 3 - 1
  expect_match(metaUI:::metaUI_reader_message("Small-sample degrees of freedom below 4 (df = 1.6)"), "unreliable")
})

test_that("na.rm = TRUE drops rows missing a filter, p or N, and ignores unmapped columns", {
  x <- fixture()
  x$grp <- factor(rep(c("a", "b"), 8)); x$grp[3] <- NA
  x$dose <- c(1:15, NA)
  x$unused <- c(NA, rep(1, 15))
  d <- prepared(x, filters = c(Group = "grp", Dose = "dose"), na.rm = TRUE)
  expect_equal(nrow(d), 14)
  expect_false("(Missing)" %in% levels(d$metaUI__filter_Group))
  expect_true(1 %in% d$metaUI__effect_id[d$metaUI__study_id == "A"]) # row 1 is only missing `unused`
  kept <- prepared(x, filters = c(Group = "grp", Dose = "dose"))
  expect_equal(nrow(kept), 16)
  expect_true("(Missing)" %in% levels(kept$metaUI__filter_Group))
})

test_that("alphabetical filter order follows the labels", {
  x <- fixture(); x$a <- factor(rep(c("x", "y"), 8)); x$b <- factor(rep(c("u", "v"), 8))
  d <- prepared(x, filters = c(Zeta = "a", Alpha = "b"), arrange_filters = "alphabetical")
  expect_identical(grep("^metaUI__filter_", names(d), value = TRUE), c("metaUI__filter_Alpha", "metaUI__filter_Zeta"))
})

test_that("p-curve is drawn for correlations; only its effect estimate needs SMD", {
  x <- fixture(); x$yi <- abs(x$yi) / 2; x$n <- 50
  d <- prepare_data(x, "study", "yi", variance = "vi", es_label = "label", es_id = "id", es_type = "ZCOR",
    variance_scale = "z", direction = "positive", sample_size = "n")
  expect_s3_class(metaUI:::metaUI_pcurve_data(d), "data.frame")
  expect_error(metaUI:::metaUI_pcurve_data(d, require_N = TRUE), "only for standardized mean differences")
})

test_that("links made before a data update apply what still fits and say what changed", {
  x <- fixture(); x$group <- factor(rep(c("a", "b"), 8)); x$year <- 2001:2016
  d <- prepared(x, filters = c("group", "year"))
  filters <- list(list(id = "metaUI__filter_group", col = "metaUI__filter_group", type = "categorical"),
    list(id = "metaUI__filter_year", col = "metaUI__filter_year", type = "numeric"))
  r <- metaUI:::metaUI_parse_selection("?metaui=old&group=a,gone&year=1990,2005", "new", d, filters)
  expect_true(r$stale)
  expect_equal(r$filters$metaUI__filter_group$selection, "a")
  expect_equal(as.numeric(r$filters$metaUI__filter_year$selection), c(2001, 2005))
  expect_match(r$adjusted, "gone no longer in the data", all = FALSE)
  expect_match(r$adjusted, "trimmed", all = FALSE)
  none <- metaUI:::metaUI_parse_selection("?metaui=old&group=gone", "new", d, filters)
  expect_setequal(none$filters$metaUI__filter_group$selection, c("a", "b"))
  # The same link against the data it was made for is still strict.
  expect_error(metaUI:::metaUI_parse_selection("?metaui=new&group=gone", "new", d, filters), "Unknown")
})

test_that("the table marks the conditional PET-PEESE estimate and notes trim-and-fill moving away from zero", {
  skip_without_app_packages()
  models <- get_model_tibble()
  models <- models[models$name %in% c("Trim-and-fill", "Precision Effect Test", "Precision Effect Estimate using Standard Error"), ]
  one_per_study <- function(yi, se, direction) prepare_data(data.frame(study = paste0("S", seq_along(yi)), id = 1, yi = yi, vi = se^2),
    "study", "yi", variance = "vi", es_id = "id", direction = direction)
  se <- seq(.05, .4, length.out = 20)
  # A genuine effect, unrelated to precision: PET's intercept is clearly positive, so PEESE is reported.
  genuine <- metaUI:::metaUI_fit_models(one_per_study(.5 + rep(c(-.05, .05), 10), se, "positive"), models)$table
  pet_fit <- lm(yi ~ se, weights = 1 / se^2, data = data.frame(yi = .5 + rep(c(-.05, .05), 10), se = se))
  expect_equal(genuine$pet_intercept_p[genuine$Model == "Precision Effect Test"],
    pt(summary(pet_fit)$coefficients[1, "t value"], pet_fit$df.residual, lower.tail = FALSE))
  expect_identical(genuine$Model[genuine$pet_peese_selected], "Precision Effect Estimate using Standard Error")
  # Effects that only follow their standard errors: no evidence of an effect, so PET is reported.
  null <- metaUI:::metaUI_fit_models(one_per_study(2 * se + rep(c(-.05, .05), 10), se, "unspecified"), models)$table
  expect_identical(null$Model[null$pet_peese_selected], "Precision Effect Test")
  html <- metaUI:::metaUI_estimates_html(null)
  expect_match(html, "PET-PEESE estimate</span>", fixed = TRUE)
  expect_match(html, "the conditional rule reports PEESE")
  # Smaller studies show weaker effects: filling on the side of the estimate moves it away from zero.
  weaker <- metaUI:::metaUI_fit_models(one_per_study(.6 - se + rep(c(-.03, .03), 10), se, "positive"), models)$table
  tf <- weaker[weaker$Model == "Trim-and-fill", ]
  expect_true(tf$trimfill_away_from_zero)
  expect_match(metaUI:::metaUI_estimates_html(weaker), "moved the estimate further from zero")
  expect_false(isTRUE(null$trimfill_away_from_zero[null$Model == "Trim-and-fill"]))
  expect_no_match(html, "moved the estimate further from zero")
})

test_that("character filters point to categorical_filters; the JSON config accepts an intro", {
  x <- fixture(); x$grp <- rep(c("a", "b"), 8)
  expect_error(prepared(x, filters = c(Group = "grp")), 'categorical_filters = "grp"', fixed = TRUE)
  schema <- jsonlite::fromJSON(system.file("examples", "config.schema.json", package = "metaUI"), simplifyVector = FALSE)
  expect_true("intro" %in% names(schema$properties$options$properties))
  expect_true("source" %in% unlist(schema$properties$design$properties$font$enum))
})

test_that("an interval far wider than the rest is clipped with an arrow, never the primary one", {
  table <- data.frame(Model = c("Primary", "Other", "PET"), es = c(.3, .25, 0), LCL = c(.1, .05, -13.6), UCL = c(.5, .45, 13.3),
    k = 3, status = "ok", reason = "", warnings = "", aggregated = c(FALSE, TRUE, TRUE), interval_type = "Confidence interval")
  expect_identical(metaUI:::metaUI_clipped_rows(table, 1L), 3L)
  html <- metaUI:::metaUI_estimates_html(table, 1L)
  expect_match(html, "metaui-track-arrow-left", fixed = TRUE)
  expect_match(html, "metaui-track-arrow-right", fixed = TRUE)
  expect_match(html, "The interval of PET is far wider")
  expect_match(html, "[−13.60, 13.30]", fixed = TRUE)
  # Two broken estimators do not shield each other.
  two <- rbind(table[c(1, 2, 2, 2, 2), ], table[3, ], transform(table[3, ], Model = "PEESE", LCL = -4.6, UCL = 4.7))
  expect_identical(metaUI:::metaUI_clipped_rows(two, 1L), 6:7)
  # Comparable widths: nothing is clipped, and a wide primary interval is never clipped.
  table$LCL[3] <- -.5; table$UCL[3] <- .5
  expect_identical(metaUI:::metaUI_clipped_rows(table, 1L), integer())
  table$LCL[1] <- -20; table$UCL[1] <- 20
  expect_identical(metaUI:::metaUI_clipped_rows(table, 1L), integer())
})

test_that("with one effect per study the table drops the effect/study-level split", {
  table <- data.frame(Model = c("Random-Effects Multilevel Model", "Trim-and-fill"), es = c(.3, .25), LCL = c(.1, .05), UCL = c(.5, .45),
    k = 10, input_rows = 10, status = "ok", reason = "", warnings = "", aggregated = c(FALSE, TRUE), interval_type = "Confidence interval")
  html <- metaUI:::metaUI_estimates_html(table, 1L, independent = TRUE)
  expect_no_match(html, "metaui-group-row", fixed = TRUE)
  expect_match(html, '<span class="metaui-primary-tag">primary</span>', fixed = TRUE)
  expect_match(html, "the standard random-effects model, as each study has one effect", fixed = TRUE)
  expect_match(metaUI:::metaUI_estimates_html(table, 1L), "effect-level fit", fixed = TRUE)
})

test_that("one effect per study: one random-effects model replaces multilevel and RVE by default", {
  skip_without_app_packages()
  set.seed(2); x <- data.frame(study = paste0("S", 1:12), yi = rnorm(12, .3, .2), vi = runif(12, .01, .05))
  d <- prepare_data(x, "study", "yi", variance = "vi")
  models <- get_model_tibble(one_effect_per_study = TRUE)
  expect_identical(models$name[1], "Random-Effects Model")
  expect_false(any(c("Random-Effects Multilevel Model", "Robust Variance Estimation") %in% models$name))
  fit <- metaUI:::metaUI_fit_models(d, models)
  reference <- metafor::rma.uni(x$yi, x$vi, method = "REML", test = "knha")
  expect_equal(unname(unlist(fit$table[1, c("es", "LCL", "UCL")])), c(as.numeric(reference$b), reference$ci.lb, reference$ci.ub), tolerance = 1e-8)
  het <- metaUI:::metaUI_heterogeneity(fit$components[[1]], d)
  expect_equal(het$study_variance, reference$tau2, tolerance = 1e-8)
  path <- tempfile()
  expect_message(generate_shiny(d, "Independent", save_to_folder = path, launch_app = FALSE), "one random-effects model")
  generated <- new.env(); sys.source(file.path(path, "models.R"), generated)
  expect_identical(generated$models_to_run$name[1], "Random-Effects Model")
  # Passing the tibble keeps the multilevel model and RVE; nested selections that happen to be
  # independent mark RVE as close to the random-effects model.
  path2 <- tempfile()
  expect_no_message(generate_shiny(d, "Independent", models = get_model_tibble(), save_to_folder = path2, launch_app = FALSE), message = "one random-effects model")
  table <- metaUI:::metaUI_fit_models(d, get_model_tibble()[1:2, ])$table
  expect_match(metaUI:::metaUI_estimates_html(table, independent = TRUE), "differs mainly in using a robust standard error", fixed = TRUE)
})

test_that("ordered moderators are estimated per level with treatment coding", {
  skip_without_app_packages()
  x <- fixture(); x$dose <- factor(rep(c("low", "mid", "high"), length.out = 16), levels = c("low", "mid", "high"))
  plain <- prepared(x, filters = c(Dose = "dose"))
  ordered <- plain; ordered$metaUI__filter_Dose <- factor(ordered$metaUI__filter_Dose, ordered = TRUE)
  a <- metaUI:::metaUI_moderator_fit(plain, "metaUI__filter_Dose")
  b <- metaUI:::metaUI_moderator_fit(ordered, "metaUI__filter_Dose")
  la <- metaUI:::metaUI_moderator_levels(a$model, c("low", "mid", "high"))
  lb <- metaUI:::metaUI_moderator_levels(b$model, c("low", "mid", "high"))
  expect_equal(lb$b, la$b, tolerance = 1e-8)
  expect_equal(lb$ci.lb, la$ci.lb, tolerance = 1e-8)
})

test_that("an app built for one effect per study refuses uploads with several effects in a study", {
  skip_without_app_packages()
  root <- tempfile(); on.exit(unlink(root, recursive = TRUE))
  set.seed(3); x <- data.frame(study = paste0("S", 1:8), yi = rnorm(8, .3, .2), vi = runif(8, .01, .05))
  d <- prepare_data(x, "study", "yi", variance = "vi")
  suppressMessages(generate_shiny(d, "Independent upload", save_to_folder = root, launch_app = FALSE))
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(root)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-10, 10), go = 1)
    sheets <- data_list()
    valid <- tempfile(fileext = ".xlsx"); on.exit(unlink(valid), add = TRUE)
    writexl::write_xlsx(sheets, valid)
    session$setInputs(uploadData = list(datapath = valid, name = "same.xlsx"), executeUpload = 1)
    expect_false(is.null(state_values$uploaded_data))
    state_values$uploaded_data <- NULL
    sheets$dataset$metaUI__study_id[2] <- sheets$dataset$metaUI__study_id[1]
    sheets$dataset$metaUI__effect_id[2] <- "2"
    file <- tempfile(fileext = ".xlsx"); on.exit(unlink(file), add = TRUE)
    writexl::write_xlsx(sheets, file)
    session$setInputs(uploadData = list(datapath = file, name = "dependent.xlsx"), executeUpload = 1)
    expect_null(state_values$uploaded_data)
  })
})

test_that("review fixes on #54: counts, nested data, empty counts, non-ASCII IDs, na.rm placeholders", {
  # Fractional counts describe no contingency table.
  cells <- metaUI:::metaUI_count_effects(data.frame(a = c(4, 4.5), b = 10, c = 3, d = 10), c(ai = "a", bi = "b", ci = "c", di = "d"), "OR")
  expect_identical(cells$invalid, c(FALSE, TRUE))
  # The plain random-effects model cannot take nested data.
  expect_error(generate_shiny(prepared(), "nested", models = get_model_tibble(one_effect_per_study = TRUE), launch_app = FALSE), "treats effects as independent")
  # Accented or non-Latin labels keep distinct input IDs; ASCII IDs are unchanged.
  ids <- metaUI:::metaUI_filter_id(c("metaUI__filter_Größe", "metaUI__filter_Grüße", "metaUI__filter_国家", "metaUI__filter_Dose (mg)"))
  expect_false(anyDuplicated(ids) > 0)
  expect_identical(ids[4], "metaUI__filter_Dose__mg_")
  expect_true(all(grepl("^[A-Za-z0-9_-]+$", ids)))
  # A p-value column that happens to share the internal placeholder's name still counts for na.rm.
  x <- fixture(); x$metaUI_input_p <- c(NA, rep(.04, 15))
  d <- suppressWarnings(prepared(x, pvalue = "metaUI_input_p", na.rm = TRUE))
  expect_equal(nrow(d), 15)
})

test_that("second review: moderator levels with one study, custom weights and start values", {
  skip_if_not_installed("clubSandwich")
  # A level without an interval shows its estimate as a point and says why.
  html <- metaUI:::metaUI_moderator_html(c("a", "b"), c(.2, .4), c(.1, NA), c(.3, NaN), c(1200L, 1L), c("a", "b"), c(.2, .4), c("1", "2"), "SMD")
  expect_match(html, "too few studies", fixed = TRUE)
  expect_match(html, "metaui-track-bare", fixed = TRUE)
  expect_match(html, "1,200", fixed = TRUE)
  expect_false(grepl("NaN", html, fixed = TRUE))
  expect_equal(lengths(regmatches(html, gregexpr("metaui-track-diamond", html))), 1L)
  # Custom weights keep the overall test, from clubSandwich's own HTZ test.
  x <- fixture(); x$g <- factor(rep(c("a", "b"), each = 8))
  d <- prepared(x, filters = c(g = "g"))
  weighted <- 'metafor::rma.mv(metaUI__effect_size, metaUI__variance, W = diag(1 / df$metaUI__variance), random = ~ 1 | metaUI__study_id/metaUI__effect_id, data = df)'
  fitted <- metaUI:::metaUI_moderator_fit(d, "metaUI__filter_g", list(name = "Weighted", code = weighted))
  expect_identical(fitted$working, "primary")
  expect_true(fitted$omnibus)
  env <- new.env(); env$df <- d
  plain <- eval(metaUI:::metaUI_moderator_code(weighted, ~ `metaUI__filter_g`), env)
  ref <- clubSandwich::Wald_test(plain, clubSandwich::constrain_zero(2), vcov = "CR2", cluster = d$metaUI__study_id, test = "HTZ")
  expect_equal(fitted$model$QMp, ref$p_val, tolerance = 1e-8)
  # Starting values from the model without the moderator reach the same fit.
  base <- metaUI:::metaUI_multilevel_fit(d)
  a <- metaUI:::metaUI_moderator_fit(d, "metaUI__filter_g")
  b <- metaUI:::metaUI_moderator_fit(d, "metaUI__filter_g", start = base$sigma2)
  expect_equal(as.numeric(b$model$b), as.numeric(a$model$b), tolerance = 1e-6)
  expect_equal(b$model$QMp, a$model$QMp, tolerance = 1e-6)
})

test_that("second review: exclusion note, stale links and help text", {
  # One reason per effect, the most basic, so the counts add up.
  x <- data.frame(id = 1:4, a = c(0, NA, 3, 4), b = 20, c = c(0, 2, 3, 5), d = 20)
  d <- suppressWarnings(prepare_data(x, "id", counts = c(ai = "a", bi = "b", ci = "c", di = "d"), es_type = "OR"))
  note <- metaUI:::metaUI_exclusions_html(attr(d, "metaUI_validation"))
  expect_match(note, "2 were left out")
  expect_match(note, "1 with no events, or only events, in both groups", fixed = TRUE)
  expect_match(note, "1 with missing, negative, fractional or impossible counts", fixed = TRUE)
  expect_false(grepl("no valid", note))
  # A link for earlier data names a filter that no longer exists, but not a tracking tag.
  x <- fixture(); x$group <- factor(rep(c("a", "b"), 8))
  d <- prepared(x, filters = c("group"))
  filters <- list(list(id = "metaUI__filter_group", col = "metaUI__filter_group", type = "categorical"))
  r <- metaUI:::metaUI_parse_selection("?metaui=old&group=a&region=europe&utm_source=x", "new", d, filters)
  expect_match(r$adjusted, "(region) no longer exists", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("utm_source", r$adjusted, fixed = TRUE)))
  expect_length(metaUI:::metaUI_parse_selection("?metaui=new&group=a&utm_source=x", "new", d, filters)$adjusted, 0L)
})

test_that("second review: numeric study IDs with sources show the Sample table; help names the right corrections", {
  skip_without_app_packages()
  root <- tempfile(); on.exit(unlink(root, recursive = TRUE))
  x <- data.frame(s = 1:6, y = c(.1, .3, .2, .5, .2, .6), v = .04, src = rep(c("Smith 2020", "Jones 2021"), 3))
  d <- prepare_data(x, "s", "y", variance = "v", article_label = "src")
  suppressMessages(generate_shiny(d, "Numeric IDs", save_to_folder = root, launch_app = FALSE))
  server <- paste(readLines(file.path(root, "server.R")), collapse = "\n")
  expect_false(grepl("residual degrees of freedom", server, fixed = TRUE))
  expect_match(server, "Hartung-Knapp adjustment", fixed = TRUE)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(root)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-10, 10), go = 1, sample_view = "effects")
    for (view in c("effects", "studies", "sources")) {
      session$setInputs(sample_view = view)
      expect_match(output$sample_table, "<th>Source", fixed = TRUE)
    }
  })
})
