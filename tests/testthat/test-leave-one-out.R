skip_without_app_packages()

loo_fixture <- function() {
  set.seed(4)
  x <- data.frame(study = rep(paste0("S", 1:9), times = c(1, 2, 3, 2, 1, 4, 2, 1, 2)))
  x$id <- stats::ave(seq_len(nrow(x)), x$study, FUN = seq_along)
  x$yi <- round(stats::rnorm(nrow(x), .3, .25), 3)
  x$vi <- round(stats::runif(nrow(x), .01, .06), 4)
  x$yi[x$study == "S6"] <- x$yi[x$study == "S6"] + .8
  # Sources: A reports S1 and S6, B reports S2 and S3; every other study is its own source.
  x$source <- ifelse(x$study %in% c("S1", "S6"), "A", ifelse(x$study %in% c("S2", "S3"), "B", paste("Source", x$study)))
  x
}

test_that("the fast route is the exact GLS update with variance components held fixed", {
  d <- prepare_data(loo_fixture(), "study", "yi", variance = "vi", es_id = "id")
  fit <- metaUI:::metaUI_multilevel_fit(d)
  loo <- metaUI:::metaUI_loo_blocks(fit, d, c(fit$b, fit$ci.lb, fit$ci.ub))
  expect_null(loo$reason)
  fast <- metaUI:::metaUI_loo_estimates(loo)
  for (s in unique(as.character(d$metaUI__study_id))) {
    keep <- as.character(d$metaUI__study_id) != s
    study <- as.character(d$metaUI__study_id)[keep]
    M <- diag(d$metaUI__variance[keep]) + fit$sigma2[1] * outer(study, study, "==") + fit$sigma2[2] * diag(sum(keep))
    w <- solve(M)
    b <- sum(w %*% d$metaUI__effect_size[keep]) / sum(w)
    se <- sqrt(1 / sum(w))
    crit <- stats::qt(.975, length(unique(study)) - 1)
    row <- fast[fast$study == s, ]
    expect_equal(row$es, b, tolerance = 1e-10)
    expect_equal(row$se, se, tolerance = 1e-10)
    expect_equal(c(row$LCL, row$UCL), b + c(-1, 1) * crit * se, tolerance = 1e-10)
    expect_equal(row$effects, sum(!keep))
  }
  # With nothing left out, the blocks give back the fitted model.
  all <- metaUI:::metaUI_loo_estimates(loo, leave = FALSE)
  expect_equal(c(all$es, all$LCL, all$UCL), c(as.numeric(fit$b), fit$ci.lb, fit$ci.ub), tolerance = 1e-10)
})

test_that("the fast route covers correlated sampling errors and study-level models, and declines what it cannot reproduce", {
  d <- prepare_data(loo_fixture(), "study", "yi", variance = "vi", es_id = "id")
  che <- metafor::rma.mv(metaUI__effect_size, V = metafor::vcalc(vi = metaUI__variance, cluster = metaUI__study_id, obs = metaUI__effect_id, rho = .6, data = d),
    random = ~ 1 | metaUI__study_id/metaUI__effect_id, data = d, test = "t", dfs = "contain", sparse = TRUE)
  loo <- metaUI:::metaUI_loo_blocks(che, d, c(che$b, che$ci.lb, che$ci.ub))
  expect_null(loo$reason)
  # Against metafor itself: the same model without one study, with its variance components fixed.
  s <- "S6"; rest <- d[d$metaUI__study_id != s, ]
  fixed <- metafor::rma.mv(metaUI__effect_size, V = metafor::vcalc(vi = metaUI__variance, cluster = metaUI__study_id, obs = metaUI__effect_id, rho = .6, data = rest),
    random = ~ 1 | metaUI__study_id/metaUI__effect_id, data = rest, test = "t", dfs = "contain", sigma2 = che$sigma2)
  row <- metaUI:::metaUI_loo_estimates(loo)
  row <- row[row$study == s, ]
  expect_equal(c(row$es, row$LCL, row$UCL), c(as.numeric(fixed$b), fixed$ci.lb, fixed$ci.ub), tolerance = 1e-8)

  agg <- metaUI:::metaUI_aggregate(d)
  for (test in c("z", "knha")) {
    uni <- metafor::rma.uni(agg$metaUI__effect_size, agg$metaUI__variance, test = test)
    loo <- metaUI:::metaUI_loo_blocks(uni, agg, c(uni$b, uni$ci.lb, uni$ci.ub))
    expect_null(loo$reason)
    fixed <- metafor::rma.uni(agg$metaUI__effect_size[-1], agg$metaUI__variance[-1], tau2 = uni$tau2, test = test)
    row <- metaUI:::metaUI_loo_estimates(loo)[1, ]
    expect_equal(c(row$es, row$LCL, row$UCL), c(as.numeric(fixed$b), fixed$ci.lb, fixed$ci.ub), tolerance = 1e-8)
  }

  mods <- metafor::rma.mv(metaUI__effect_size, metaUI__variance, mods = ~ metaUI__se, random = ~ 1 | metaUI__study_id/metaUI__effect_id, data = d)
  expect_match(metaUI:::metaUI_loo_blocks(mods, d, c(mods$b[1], mods$ci.lb[1], mods$ci.ub[1]))$reason, "moderators")
  robust <- metafor::robust(che, cluster = d$metaUI__study_id)
  expect_match(metaUI:::metaUI_loo_blocks(robust, d, c(robust$b, robust$ci.lb, robust$ci.ub))$reason, "model-based")
  # An extraction that reports something other than the fitted interval is not reproduced.
  expect_match(metaUI:::metaUI_loo_blocks(che, d, c(che$b, che$ci.lb - .1, che$ci.ub))$reason, "reproduced")
  # Effects linked across studies (a shared random effect) cannot be left out one study at a time.
  d$lab <- rep(c("a", "b"), length.out = nrow(d))
  crossed <- metafor::rma.mv(metaUI__effect_size, metaUI__variance, random = list(~ 1 | metaUI__study_id, ~ 1 | lab), data = d)
  expect_match(metaUI:::metaUI_loo_blocks(crossed, d, c(crossed$b, crossed$ci.lb, crossed$ci.ub))$reason, "different studies")

  # metaUI_fit_models keeps the blocks for metafor models only.
  fits <- metaUI:::metaUI_fit_models(d, get_model_tibble()[1:3, ])
  expect_null(fits$loo[[1]]$reason)
  expect_null(fits$loo[[2]])
  expect_false(is.null(fits$loo[[3]]$reason))
})

test_that("leaving out a source drops all its studies, in the fast route and in refits", {
  d <- prepare_data(loo_fixture(), "study", "yi", variance = "vi", es_id = "id", article_label = "source")
  groups <- unique(data.frame(study = as.character(d$metaUI__study_id), source = d$metaUI__article_label))
  groups <- stats::setNames(groups$source, groups$study)
  fit <- metaUI:::metaUI_multilevel_fit(d)
  loo <- metaUI:::metaUI_loo_blocks(fit, d, c(fit$b, fit$ci.lb, fit$ci.ub))
  fast <- metaUI:::metaUI_loo_estimates(loo, groups = groups)
  expect_equal(nrow(fast), 7L)
  row <- fast[fast$study == "A", ]
  expect_equal(c(row$studies, row$effects), c(2L, 5L))
  rest <- d[!d$metaUI__study_id %in% c("S1", "S6"), ]
  fixed <- metafor::rma.mv(rest$metaUI__effect_size, V = rest$metaUI__variance, random = ~ 1 | metaUI__study_id/metaUI__effect_id, data = rest,
    test = "t", dfs = "contain", sigma2 = fit$sigma2)
  expect_equal(c(row$es, row$se, row$LCL, row$UCL), c(as.numeric(fixed$b), fixed$se, fixed$ci.lb, fixed$ci.ub), tolerance = 1e-8)
  # Studies without a source are sources of their own.
  expect_equal(metaUI:::metaUI_loo_unit_of(c("S1", "S9"), c(S1 = "A", S9 = NA)), c("A", "S9"))

  slow <- metaUI:::metaUI_loo_refit(d, get_model_tibble()[1, ], studies = "A", groups = groups)
  refit <- metaUI:::metaUI_multilevel_fit(rest)
  pred <- stats::predict(refit)
  expect_equal(unlist(slow[1, c("es", "LCL", "UCL", "se", "PI_LCL", "PI_UCL", "tau")], use.names = FALSE),
    c(as.numeric(refit$b), refit$ci.lb, refit$ci.ub, refit$se, pred$pi.lb, pred$pi.ub, sqrt(sum(refit$sigma2))), tolerance = 1e-8)
  expect_equal(c(slow$studies, slow$effects), c(2L, 5L))

  # The change in standard errors of the full estimate, on the fitting scale.
  table <- metaUI:::metaUI_leave_one_out(slow, c(es = as.numeric(fit$b), LCL = fit$ci.lb, UCL = fit$ci.ub, se = fit$se), d)
  expect_equal(table$change_se, (as.numeric(refit$b) - as.numeric(fit$b)) / fit$se)
  expect_equal(table$PI_LCL, as.numeric(pred$pi.lb))
  html <- metaUI:::metaUI_loo_html(table, c(es = as.numeric(fit$b), LCL = fit$ci.lb, UCL = fit$ci.ub), unit = "source")
  expect_match(html, "Source left out", fixed = TRUE)
  expect_match(html, "2 studies, 5 effects", fixed = TRUE)
  expect_match(html, " SE)</span>", fixed = TRUE)
})

test_that("the refit route equals refitting the model without each study", {
  d <- prepare_data(loo_fixture(), "study", "yi", variance = "vi", es_id = "id")
  slow <- metaUI:::metaUI_loo_refit(d, get_model_tibble()[1, ], studies = c("S6", "S2"))
  for (s in c("S6", "S2")) {
    fit <- metaUI:::metaUI_multilevel_fit(d[d$metaUI__study_id != s, ])
    expect_equal(unlist(slow[slow$study == s, c("es", "LCL", "UCL")], use.names = FALSE), c(as.numeric(fit$b), fit$ci.lb, fit$ci.ub), tolerance = 1e-8)
  }
  expect_equal(slow$effects, c(4L, 2L))
  # The budget stops the refits after the first that exceeds it.
  expect_equal(nrow(metaUI:::metaUI_loo_refit(d, get_model_tibble()[1, ], budget = 0)), 1L)
  # Any model can be refitted, RVE included.
  rve <- metaUI:::metaUI_loo_refit(d, get_model_tibble()[2, ], correlation = .6, studies = "S1")
  expect_equal(rve$status, "ok")
})

test_that("flags mark a change in whether the interval excludes zero and moves across the bound", {
  df <- data.frame(metaUI__es_type = "SMD")
  estimates <- data.frame(study = c("a", "b", "c"), effects = 1:3, es = c(.30, .12, .22), LCL = c(.10, -.02, .05), UCL = c(.50, .26, .40))
  out <- metaUI:::metaUI_leave_one_out(estimates, c(es = .2, LCL = .03, UCL = .37), df, bound = .15)
  expect_equal(out$study, c("a", "b", "c"))
  expect_equal(out$change, c(.1, -.08, .02))
  expect_equal(out$significance_changes, c(FALSE, TRUE, FALSE))
  expect_equal(out$crosses_bound, c(FALSE, TRUE, FALSE))
  expect_true(all(is.na(metaUI:::metaUI_leave_one_out(estimates, c(es = .2, LCL = .03, UCL = .37), df)$crosses_bound)))
  # Correlations are fitted as Fisher z and shown as r; the bound is on the r scale.
  zcor <- metaUI:::metaUI_leave_one_out(estimates, c(es = .2, LCL = .03, UCL = .37), data.frame(metaUI__es_type = "ZCOR"), bound = .2)
  expect_equal(zcor$es[zcor$study == "a"], tanh(.3))
  expect_equal(zcor$crosses_bound[zcor$study == "a"], TRUE)
  html <- metaUI:::metaUI_loo_html(out, c(es = .2, LCL = .03, UCL = .37), bound = .15)
  expect_match(html, "95% CI now includes 0", fixed = TRUE)
  expect_match(html, "estimate now within ±0.15", fixed = TRUE)
  # An omission that could not be estimated keeps its status and has no flags, rather than "no change".
  estimates$es[2] <- estimates$LCL[2] <- estimates$UCL[2] <- NA
  estimates$status <- c("ok", "error", "ok"); estimates$reason <- c(NA, "did not converge", NA)
  failed <- metaUI:::metaUI_leave_one_out(estimates, c(es = .2, LCL = .03, UCL = .37), df, bound = .15)
  expect_equal(failed$study[3], "b")
  expect_equal(failed$status, c("ok", "ok", "error"))
  expect_true(is.na(failed$significance_changes[3]) && is.na(failed$crosses_bound[3]))
  expect_match(metaUI:::metaUI_loo_html(failed, c(es = .2, LCL = .03, UCL = .37), bound = .15), "not estimated without this study", fixed = TRUE)
})

test_that("a generated app shows the leave-one-out table for the primary model, or names the fallback", {
  path <- tempfile(); on.exit(unlink(path, recursive = TRUE))
  d <- prepare_data(loo_fixture(), "study", "yi", variance = "vi", es_id = "id", article_label = "source")
  models <- get_model_tibble()
  generate_shiny(d, "Leave one out", save_to_folder = path, launch_app = FALSE, models = models,
    primary_model = "Robust Variance Estimation")
  expect_true(all(vapply(c("server.R", "ui.R", "global.R"), function(f) is.expression(parse(file.path(path, f))), logical(1))))
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(path)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  html <- as.character(env$ui)
  expect_match(html, 'id="loo_panel"', fixed = TRUE)
  expect_match(html, "metaui-loo-unit", fixed = TRUE)
  shiny::testServer(env$server, {
    # Quick refits would replace the fast route; timing-dependent, so they are switched off here.
    assign("loo_quick_seconds", -1, envir = environment(loo_run_refits))
    assign("loo_few_seconds", -1, envir = environment(loo_run_refits))
    session$setInputs(outliers_z_scores = c(-10, 10), go = 1)
    src <- loo_source()
    expect_equal(src$model, "Random-Effects Multilevel Model")
    expect_false(src$primary)
    expect_equal(src$refit$model, "Robust Variance Estimation")
    panel <- output$loo_panel$html
    expect_match(panel, "From the Random-Effects Multilevel Model. The primary model, Robust Variance Estimation, is not used here because only metafor models", fixed = TRUE)
    expect_match(panel, "metaui-loo", fixed = TRUE)
    expect_match(panel, 'id="loo_refit"', fixed = TRUE)
    expect_match(panel, "With so few studies", fixed = TRUE)
    expect_false(loo_result()$refit)
    loo_refits(loo_run_refits(src))
    result <- loo_result()
    expect_true(result$refit)
    expect_equal(result$model, "Robust Variance Estimation")
    expect_equal(result$table$study[1], "S6")
    session$flushReact()
    panel <- output$loo_panel$html
    expect_match(panel, "Robust Variance Estimation, the primary model chosen by the authors. Each row refits the model without that study", fixed = TRUE)
    session$setInputs(sesoi = .3)
    expect_false(anyNA(loo_result()$table$crosses_bound))
    csv <- utils::read.csv(output$loo_csv)
    expect_equal(nrow(csv), 9L)
    expect_true(all(c("study", "es", "LCL", "UCL", "change", "significance_changes", "crosses_bound", "model", "method") %in% names(csv)))
    expect_equal(csv$method[1], "full refit")
    # A failed refit is reported and listed, and the panel still renders with a bound.
    stored <- loo_refits(); stored$estimates[stored$estimates$study == "S2", c("es", "LCL", "UCL")] <- NA
    stored$estimates$status[stored$estimates$study == "S2"] <- "error"; loo_refits(stored)
    session$flushReact()
    panel <- output$loo_panel$html
    expect_match(panel, "Without one study the model could not be estimated (listed below); what follows covers the other 8.", fixed = TRUE)
    expect_match(panel, "S2<span", fixed = TRUE)
    expect_match(panel, "does not show whether several related studies together drive the result; leave out one source", fixed = TRUE)
    # Refits re-estimate the spread of true effects; RVE gives tau rather than a prediction interval.
    expect_match(panel, "The SD of true effects", fixed = TRUE)
    expect_match(panel, " SE)</span>", fixed = TRUE)

    # Sources: the fast route from the multilevel model, then refits of the primary model.
    session$setInputs(loo_unit = "source")
    result <- loo_result()
    expect_false(result$refit)
    expect_equal(nrow(result$table), 7L)
    panel <- output$loo_panel$html
    expect_match(panel, "Leaving out one source at a time", fixed = TRUE)
    expect_match(panel, "all 7 sources (9 studies)", fixed = TRUE)
    expect_match(panel, "Source left out", fixed = TRUE)
    expect_match(panel, "does not show how the spread of true effects changes", fixed = TRUE)
    loo_refits(loo_run_refits(loo_source()))
    expect_true(loo_result()$refit)
    expect_equal(loo_result()$table$studies[loo_result()$table$study == "A"], 2L)
    csv <- utils::read.csv(output$loo_csv)
    expect_equal(csv$left_out[1], "source")
  })
})

test_that("with one effect per study, the standard random-effects model is left out study by study", {
  x <- loo_fixture(); x <- x[!duplicated(x$study), ]
  d <- prepare_data(x, "study", "yi", variance = "vi", es_id = "id")
  models <- get_model_tibble(one_effect_per_study = TRUE)
  i <- which(models$code == metaUI:::metaUI_code_random & !models$aggregated)
  fits <- metaUI:::metaUI_fit_models(d, models)
  expect_null(fits$loo[[i]]$reason)
  full <- metaUI:::metaUI_univariate(d$metaUI__effect_size, d$metaUI__se, method = "REML")
  fixed <- metafor::rma.uni(d$metaUI__effect_size[-1], sei = d$metaUI__se[-1], tau2 = full$tau2, test = "knha")
  row <- metaUI:::metaUI_loo_estimates(fits$loo[[i]])[1, ]
  expect_equal(c(row$es, row$LCL, row$UCL), c(as.numeric(fixed$b), fixed$ci.lb, fixed$ci.ub), tolerance = 1e-8)
})

test_that("source omissions report what cannot be estimated, keep unlabelled studies apart, and label intervals by type", {
  # An unlabelled study whose id is also a source label stays a unit of its own.
  expect_equal(metaUI:::metaUI_loo_unit_of(c("S1", "A", "S3"), c(S1 = "A", A = NA, S3 = "B")), c("A", "A (study without a source)", "B"))
  # Leaving out a source that holds every study but one: reported as not estimated, not an error.
  x <- loo_fixture(); x$source <- ifelse(x$study == "S9", "Other", "Big")
  d <- prepare_data(x, "study", "yi", variance = "vi", es_id = "id", article_label = "source")
  groups <- unique(data.frame(study = as.character(d$metaUI__study_id), source = d$metaUI__article_label))
  slow <- metaUI:::metaUI_loo_refit(d, get_model_tibble()[1, ], studies = "Big", groups = stats::setNames(groups$source, groups$study))
  expect_equal(slow$status, "failed")
  expect_match(slow$reason, "fewer than two studies")
  expect_true(is.na(slow$es))
  # Credible and author-defined intervals are not called confidence intervals.
  expect_equal(metaUI:::metaUI_loo_interval_label("95% central credible interval (prior-dependent)"), "95% interval")
  expect_equal(metaUI:::metaUI_loo_interval_label("Author-defined interval (type and coverage unknown)"), "95% interval")
  expect_equal(metaUI:::metaUI_loo_interval_label("Confidence interval (coverage model-dependent)"), "95% CI")
  table <- metaUI:::metaUI_leave_one_out(data.frame(study = "a", effects = 1L, es = .1, LCL = -.1, UCL = .3), c(es = .2, LCL = .05, UCL = .35), data.frame(metaUI__es_type = "SMD"))
  html <- metaUI:::metaUI_loo_html(table, c(es = .2, LCL = .05, UCL = .35), interval = "95% interval")
  expect_match(html, "95% interval now includes 0", fixed = TRUE)
  expect_false(grepl("95% CI", html, fixed = TRUE))
})

test_that("the bound is drawn as a band, and odds ratios are judged against 1 and 1/b to b", {
  estimates <- data.frame(study = c("a", "b"), effects = 1:2, es = log(c(1.30, 1.10)), LCL = log(c(1.05, .95)), UCL = log(c(1.60, 1.30)))
  full <- c(es = log(1.2), LCL = log(1.02), UCL = log(1.4))
  df <- data.frame(metaUI__es_type = "OR")
  table <- metaUI:::metaUI_leave_one_out(estimates, full, df, bound = 1.25)
  # Interval b now includes 1; a moves beyond 1.25 (from 1.2), b stays within 0.8-1.25.
  expect_equal(table$significance_changes[table$study == "b"], TRUE)
  expect_equal(table$crosses_bound[table$study == "a"], TRUE)
  expect_equal(table$crosses_bound[table$study == "b"], FALSE)
  html <- metaUI:::metaUI_loo_html(table, metaUI:::metaUI_show(full, df), bound = 1.25, ratio = TRUE)
  expect_match(html, "95% CI now includes 1", fixed = TRUE)
  expect_match(html, "estimate now outside 0.8–1.25", fixed = TRUE)
  # The band spans the bounds on the axis, with positions and a width.
  expect_match(html, 'metaui-track-band" style="left:[0-9.]+%;width:[0-9.]+%"')
  smd <- metaUI:::metaUI_loo_html(metaUI:::metaUI_leave_one_out(estimates, full, data.frame(metaUI__es_type = "SMD"), bound = .25), full, bound = .25)
  expect_match(smd, 'metaui-track-band" style="left:[0-9.]+%;width:[0-9.]+%"')
})
