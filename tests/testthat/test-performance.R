test_that("p-curve optimisation preserves complete baseline results and process options", {
  reference <- readRDS(test_path("fixtures", "pcurve-before.rds"))
  png(tempfile()); on.exit(dev.off(), add = TRUE)
  original <- getOption("scipen")
  for (case in reference$cases) {
    if (!is.null(case$output$error)) expect_error(metaUI:::pcurve(case$input, effect.estimation = FALSE), case$output$error)
    else expect_identical(metaUI:::pcurve(case$input, effect.estimation = FALSE), case$output)
    expect_identical(getOption("scipen"), original)
  }
  x <- prepared(direction = "negative")
  x$metaUI__effect_size <- -abs(x$metaUI__effect_size) - .5
  mirrored <- x; mirrored$metaUI__effect_size <- -x$metaUI__effect_size
  mirrored$metaUI__direction <- "positive"
  expect_identical(metaUI:::pcurve(metaUI:::metaUI_pcurve_data(x)),
                   metaUI:::pcurve(metaUI:::metaUI_pcurve_data(mirrored)))
  bad <- data.frame(studlab = 1:3, TE = .01, seTE = 1)
  expect_error(metaUI:::pcurve(bad), "Two or less")
  expect_identical(getOption("scipen"), original)
  old_warn <- getOption("warn"); options(warn = -1)
  on.exit(options(warn = old_warn), add = TRUE)
  effect <- metaUI:::pcurve(reference$cases$twelve$input, effect.estimation = TRUE,
    N = rep(100, 12), dmax = .6)
  expect_true(is.finite(effect$dEstimate))
  expect_identical(getOption("warn"), -1L)
})

test_that("exact session cache invalidates data, attributes, models and configuration", {
  env <- new.env(parent = asNamespace("metaUI"))
  env$count <- new.env(); env$count$n <- 0L
  cache_factory <- metaUI:::metaUI_fit_cache; environment(cache_factory) <- env
  fitter <- metaUI:::metaUI_fit_models; environment(fitter) <- env; env$metaUI_fit_models <- fitter
  cache <- cache_factory(3)
  model <- get_model_tibble()[1, ]
  model$code <- 'count$n <- count$n + 1L; list(b = mean(df$metaUI__effect_size), ci.lb = -.1, ci.ub = 1, k = nrow(df))'
  d <- prepared()
  first <- cache(d, model); repeated <- cache(d, model)
  expect_false(first$cache_hit); expect_true(repeated$cache_hit)
  expect_identical(first$table$fit_seconds, repeated$table$fit_seconds)
  expect_equal(env$count$n, 1L)
  for (change in c("effect", "variance", "attribute", "direction")) {
    altered <- d
    if (change == "effect") altered$metaUI__effect_size[1] <- .123
    if (change == "variance") { altered$metaUI__variance[1] <- .05; altered$metaUI__se[1] <- sqrt(.05) }
    if (change == "attribute") attr(altered, "new_provenance") <- "changed"
    if (change == "direction") altered$metaUI__direction <- "negative"
    expect_false(cache(altered, model)$cache_hit)
  }
  expect_false(cache(d, model)$cache_hit) # original evicted from three-entry history
  expect_false(cache(d, model, .5)$cache_hit)
  expect_false(cache(d, model, .5, "first")$cache_hit)
  edited <- model; edited$code <- paste0(edited$code, " # author edit")
  expect_false(cache(d, edited)$cache_hit)
  attr(cache, "clear")(); expect_false(cache(d, edited)$cache_hit)
  disabled <- cache_factory(0)
  expect_false(disabled(d, model)$cache_hit); expect_false(disabled(d, model)$cache_hit)
  expect_error(cache_factory(4), "0 to 3")
})

test_that("diagnostics reuse only exact compatible fits and report unidentified splits", {
  d <- prepared(); models <- get_model_tibble()[1:2, ]
  result <- fit(d, models)
  reused <- metaUI:::metaUI_reuse_fit(result, models, metaUI:::metaUI_code_multilevel, d, function() stop("unexpected refit"))
  expect_identical(reused, result$fits[[1]])
  fresh <- metaUI:::metaUI_multilevel_fit(d)
  expect_equal(metaUI:::metaUI_heterogeneity(reused, d), metaUI:::metaUI_heterogeneity(fresh, d), tolerance = 1e-10)
  edited <- models; edited$code[1] <- paste0(edited$code[1], " ")
  expect_identical(metaUI:::metaUI_reuse_fit(result, edited, metaUI:::metaUI_code_multilevel, d, function() "fallback"), "fallback")
  changed <- d; changed$metaUI__effect_size[1] <- .4
  expect_identical(metaUI:::metaUI_reuse_fit(result, models, metaUI:::metaUI_code_multilevel, changed, function() "fallback"), "fallback")
  aggregated <- models; aggregated$aggregated[1] <- TRUE
  expect_identical(metaUI:::metaUI_reuse_fit(result, aggregated, metaUI:::metaUI_code_multilevel, d, function() "fallback"), "fallback")
  reflected <- result; reflected$table$reflected[1] <- TRUE
  expect_identical(metaUI:::metaUI_reuse_fit(reflected, models, metaUI:::metaUI_code_multilevel, d, function() "fallback"), "fallback")
  single <- prepare_data(fixture()[seq(1, 16, 2), ], "study", "yi", variance = "vi", es_id = "id")
  het <- metaUI:::metaUI_heterogeneity(metaUI:::metaUI_multilevel_fit(single), single)
  expect_true(is.na(het$study_variance) && is.na(het$effect_variance))
  expect_match(het$components, "not identified")
})

test_that("standalone helper copies stay identical", {
  for (file in c("analysis.R", "helpers.R", "dmetar_contributions.R")) {
    source <- test_path("..", "..", "R", file)
    skip_if_not(file.exists(source), "Source tree only")
    target <- test_path("..", "..", "inst", "template_code", file)
    expect_identical(readBin(source, "raw", file.info(source)$size),
                     readBin(target, "raw", file.info(target)$size))
  }
})

test_that("upload scale and descriptive-z contracts are explicit", {
  d <- prepared()
  upload <- as.data.frame(d); attr(upload, "metaUI_validation") <- NULL
  upload$metaUI__es_z <- 100
  checked <- metaUI:::metaUI_validate_upload(upload, d)
  expect_equal(checked$metaUI__es_z, d$metaUI__es_z, tolerance = 1e-10)
  expect_equal(attr(checked, "metaUI_upload_derivations")$supplied_z_disagreements, nrow(d))
  bad <- upload; bad$metaUI__effect_size[1] <- .9
  expect_error(metaUI:::metaUI_validate_upload(bad, d), "source/fitting-scale")
  raw <- fixture(); raw$yi <- raw$yi / 2
  cor <- prepare_data(raw, "study", "yi", variance = "vi", es_id = "id", es_type = "COR", variance_scale = "r")
  bad <- as.data.frame(cor); bad$metaUI__effect_size <- bad$metaUI__input_effect
  expect_error(metaUI:::metaUI_validate_upload(bad, cor), "source/fitting-scale")
  expect_equal(metaUI:::metaUI_validate_upload(as.data.frame(cor), cor)$metaUI__effect_size, cor$metaUI__effect_size)
})
