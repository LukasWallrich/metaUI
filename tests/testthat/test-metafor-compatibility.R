test_that("editable metafor helpers preserve meta's estimators", {
  skip_if_not_installed("meta", "8.5.0")
  y <- c(-.7, -.3, -.2, .1, .4, .6, 1.2)
  se <- c(.1, .15, .2, .25, .3, .35, .4)
  d <- data.frame(metaUI__effect_size = y, metaUI__se = se)
  m <- meta::metagen(y, se, common = FALSE, method.tau = "ML", method.random.ci = "HK")
  a <- meta::trimfill(m); b <- metaUI:::metaUI_trimfill(d)
  expect_equal(as.numeric(b$b), a$TE.random, tolerance = 1e-8)
  expect_equal(c(b$ci.lb, b$ci.ub), c(a$lower.random, a$upper.random), tolerance = 1e-8)
  expect_equal(b$k, a$k)
  expect_equal(b$k0, sum(a$trimfill))
  expect_equal(b$left, a$left)
  a <- meta::metabias(m, k.min = 3, method.bias = "Egger")
  b <- metaUI:::metaUI_egger(y, se)
  expect_equal(unname(b$estimate), unname(a$estimate), tolerance = 1e-8)
  expect_equal(b$tau, a$tau, tolerance = 1e-8)
  expect_equal(b$statistic, unname(a$statistic), tolerance = 1e-8)
  expect_equal(b$p.value, a$p.value, tolerance = 1e-8)
  expect_warning(expect_null(metaUI:::metaUI_trimfill(d[1:2, ])),
    "Minimal number of three studies for trim-and-fill method")
  expect_equal(metaUI:::metaUI_egger(y[1:2], se[1:2]), list(k = 2L, k.min = 3))
  expect_error(metaUI:::metaUI_egger(y, rep(.2, length(y))), "same precision")
})

test_that("meta is required only by explicit author code", {
  expect_false(metaUI:::models_use_meta(get_model_tibble()$code))
  expect_false(metaUI:::models_use_meta(c("bayesmeta::bayesmeta(y)", "robumeta::robu(y)")))
  for (code in c("meta::metagen(x)", "library(meta)", 'library("meta")', "require(meta)"))
    expect_true(metaUI:::models_use_meta(code))
  expect_false("meta" %in% metaUI:::declared_imports())
  expect_false(grepl("packageVersion('meta')", metaUI:::generate_global.R("g"), fixed = TRUE))
  expect_true(grepl("packageVersion('meta')", metaUI:::generate_global.R("g", uses_meta = TRUE), fixed = TRUE))
})

test_that("trim-and-fill keeps average ties and iteration-limit returns", {
  skip_if_not_installed("meta", "8.5.0")
  for (y in list(rep(.2, 20), c(-1, -1, 0, 0, 1, 1))) {
    se <- sqrt(seq(.01, .1, length.out = length(y)))
    m <- meta::metagen(y, se, common = FALSE, method.tau = "ML", method.random.ci = "HK")
    for (limit in c(1L, 50L)) {
      a <- meta::trimfill(m, n.iter.max = limit)
      b <- metaUI:::metaUI_trimfill(data.frame(metaUI__effect_size = y, metaUI__se = se), n.iter.max = limit)
      expect_equal(b$k0, sum(a$trimfill))
      expect_equal(b$left, a$left)
      expect_equal(as.numeric(b$b), a$TE.random, tolerance = 1e-8)
      expect_equal(c(b$ci.lb, b$ci.ub), c(a$lower.random, a$upper.random), tolerance = 1e-8)
    }
  }
})

test_that("author meta code adds the optional dependency and About entry", {
  skip_if_not_installed("meta", "7.0.0")
  path <- tempfile(); on.exit(unlink(path, recursive = TRUE))
  model <- get_model_tibble()[1, ]
  model$code <- 'meta::metagen(df$metaUI__effect_size, df$metaUI__se)'
  model$es <- "mod$TE.random"; model$LCL <- "mod$lower.random"; model$UCL <- "mod$upper.random"
  generate_shiny(prepared(), "Author meta", models = model, meta = FALSE, save_to_folder = path, launch_app = FALSE)
  expect_match(paste(readLines(file.path(path, "ui.R")), collapse = "\n"), "<td>meta</td>", fixed = TRUE)
  expect_true("meta" %in% read.csv(file.path(path, "dependencies.csv"))$package)
  expect_true(any(grepl("packageVersion('meta')", readLines(file.path(path, "global.R")), fixed = TRUE)))
})


test_that("zero trim-and-fill iterations return the unfilled model", {
  skip_if_not_installed("meta", "8.5.0")
  y <- c(-.7, -.3, -.2, .1, .4, .6, 1.2)
  se <- c(.1, .15, .2, .25, .3, .35, .4)
  m <- meta::metagen(y, se, common = FALSE, method.tau = "ML", method.random.ci = "HK")
  # meta 8.5.0 rejects zero iterations; the helper supports the unfilled fit.
  expect_error(meta::trimfill(m, n.iter.max = 0L), "n.iter.max")
  fit <- metaUI:::metaUI_trimfill(data.frame(metaUI__effect_size = y, metaUI__se = se), n.iter.max = 0L)
  expect_equal(fit$k0, 0)
  expect_equal(fit$n.iter, 0)
  expect_equal(fit$k, length(y))
  expect_equal(as.numeric(fit$b), m$TE.random, tolerance = 1e-8)
  expect_equal(c(fit$ci.lb, fit$ci.ub), c(m$lower.random, m$upper.random), tolerance = 1e-8)
})


test_that("meta dependency detection ignores prose and recognises executable calls", {
  for (code in c('# meta::metagen(x)', '"meta::metagen(x)"', 'x <- "library(meta)"', '# library(meta)'))
    expect_false(metaUI:::models_use_meta(code))
  for (code in c('meta::metagen(x)', 'library(meta)', 'require("meta")',
                 'requireNamespace("meta", quietly = TRUE)', 'base::library(package = "meta")',
                 'f <- function() { library(
meta
) }', 'meta::metagen('))
    expect_true(metaUI:::models_use_meta(code))
})
