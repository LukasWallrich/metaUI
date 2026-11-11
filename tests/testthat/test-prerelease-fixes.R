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
