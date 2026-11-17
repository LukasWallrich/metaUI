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

test_that("I-squared of the CHE model uses the full sampling covariance, as metafor's recipe", {
  set.seed(3)
  x <- data.frame(study = rep(1:12, each = 3), id = rep(1:3, 12), vi = runif(36, .01, .06))
  x$yi <- rnorm(12, .3, .2)[x$study] + rnorm(36, 0, .1) + rnorm(36, 0, sqrt(x$vi))
  d <- prepare_data(x, "study", "yi", variance = "vi", es_id = "id")
  fit <- metaUI:::metaUI_multilevel_fit(d)
  W <- solve(as.matrix(fit$V)); X <- model.matrix(fit)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  reference <- 100 * sum(fit$sigma2) / (sum(fit$sigma2) + (fit$k - fit$p) / sum(diag(P)))
  expect_equal(metaUI:::metaUI_heterogeneity(fit, d)$I2, reference, tolerance = 1e-10)
  cached <- metaUI:::metaUI_fit_models(d, get_model_tibble()[1, ])$components[[1]]
  expect_equal(metaUI:::metaUI_heterogeneity(cached, d)$I2, reference, tolerance = 1e-8)
})
