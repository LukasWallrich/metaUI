test_that("to_tribble round-trips code with backslashes and quotes", {
  x <- tibble::tibble(name = "m", code = "grepl(\"^\\\\s*$\", x) && 'a' != \"b\"")
  back <- eval(parse(text = metaUI:::to_tribble(x)))
  expect_identical(back$code, x$code)
})

test_that("a widened range keeps every new value inside once stored as text", {
  range <- c(-1.8, 2.2)
  z <- c(0.3, 5.0493668538738605, -2.0000000000000004)
  widened <- as.numeric(as.character(metaUI:::metaUI_widen_range(range, z, FALSE)))
  expect_true(all(z >= widened[1] & z <= widened[2]))
  expect_identical(metaUI:::metaUI_widen_range(range, c(0, 1), FALSE), range)
  expect_identical(metaUI:::metaUI_widen_range(c(2001, 2016), c(2005, 2030)), c(2001, 2030))
})

test_that("prepare_data accepts the study label column as article label", {
  d <- data.frame(study = c("A", "B", "C"), g = c(.1, .2, .3), v = c(.01, .02, .03))
  out <- prepare_data(d, "study", "g", "v", article_label = "study")
  expect_identical(out$metaUI__article_label, d$study)
})

test_that("trim-and-fill reports observed studies, RVE effects, with units", {
  skip_without_app_packages()
  set.seed(4)
  d <- data.frame(study = rep(paste0("S", 1:12), each = 2), v = runif(24, .005, .08))
  d$g <- .3 + 2 * sqrt(d$v) + rnorm(24, 0, sqrt(d$v))
  prepared <- prepare_data(d, "study", "g", "v", direction = "positive")
  models <- get_model_tibble()
  table <- metaUI:::metaUI_fit_models(prepared, models[models$name %in% c("Robust Variance Estimation", "Trim-and-fill"), ])$table
  tf <- table[table$Model == "Trim-and-fill", ]
  expect_identical(tf$status, "ok")
  expect_equal(tf$k, 12)
  expect_identical(tf$k_unit, "studies")
  rve <- table[table$Model == "Robust Variance Estimation", ]
  expect_equal(rve$k, 24)
  expect_identical(rve$k_unit, "effects")
})
