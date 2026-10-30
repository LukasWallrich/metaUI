test_that("every generated-app package is declared in Suggests", {
  suggests <- trimws(gsub("\\([^)]*\\)", "", strsplit(utils::packageDescription("metaUI")$Suggests, ",")[[1]]))
  expect_true(all(metaUI:::metaUI_app_suggests %in% suggests))
  expect_false(any(metaUI:::metaUI_app_suggests %in% metaUI:::declared_imports()))
  expect_true(all(c(metaUI:::declared_imports(), metaUI:::metaUI_app_suggests) %in% metaUI:::app_dependencies()))
})

test_that("missing generated-app packages are named with an install command", {
  local_mocked_bindings(missing_app_packages = function(bayesian = FALSE) c("weightr", "zcurve"))
  expect_error(metaUI:::check_app_packages(), 'install.packages\\(c\\("weightr", "zcurve"\\)\\)')
})

test_that("the removed Barroso example stops with a pointer to the bundled examples", {
  path <- system.file("extdata", "barroso2021.csv", package = "metaUI")
  expect_error(prepare_data(read.csv(path), "study", "yi", variance = "vi"), "no longer bundled.*coles2019")
  expect_error(prepare_data(path, "study", "yi", variance = "vi"), "no longer bundled")
})
