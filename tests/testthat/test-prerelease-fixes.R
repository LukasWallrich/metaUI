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
