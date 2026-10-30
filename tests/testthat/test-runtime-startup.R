test_that("generated dependency checks do not load namespaces and retain useful errors", {
  code <- metaUI:::generate_global.R("Effect")
  checks <- Filter(function(x) grepl("Missing dependency:", paste(deparse(x), collapse = " "), fixed = TRUE), as.list(parse(text = code)))
  expect_gt(length(checks), 20)
  env <- new.env()
  env$system.file <- function(...) "installed"
  before <- loadedNamespaces()
  for (check in checks) eval(check, env)
  expect_setequal(loadedNamespaces(), before)
  env$system.file <- function(...) ""
  expect_error(eval(checks[[1]], env), "Missing dependency:.*dependencies.csv")
  expect_match(code, "utils::packageVersion('meta')", fixed = TRUE)
  expect_false(any(c("shinyWidgets", "stringr") %in% metaUI:::runtime_imports()))
})

test_that("runtime string operations preserve generated-app label and p-value handling", {
  x <- c("Smith 2020: Study", "Müller [A]", NA_character_)
  patterns <- c("Smith 2020", "Müller", "other")
  expect_identical(metaUI:::metaUI_remove(x, patterns), stringr::str_remove(x, patterns))
  expect_identical(metaUI:::metaUI_remove_all(c("`group`", NA), "`"), c("group", NA_character_))
  expect_identical(metaUI:::metaUI_locate("=", c("t(10)=2", "none", NA)), c(6L, NA_integer_, NA_integer_))
  expect_identical(metaUI:::fmt_p(c(.001, .05, .5, .999, NA)), c("= .001", "= .050", "= .500", "> .99", NA_character_))
})
