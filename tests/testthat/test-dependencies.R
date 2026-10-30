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

test_that("the removed-example check matches only the stub", {
  unrelated <- data.frame(metaUI_removed_dataset = 1:2, message = "x", yi = 1:2)
  expect_identical(metaUI:::check_removed_example(unrelated), unrelated)
})

test_that("the About page lists the packages the generated app uses", {
  about <- metaUI:::create_about("Fixture")
  for (p in c("metafor", "ggplot2", "meta", "DT")) expect_match(about, paste0("<td>", p, "</td>"), fixed = TRUE)
  expect_false(grepl("<td>bayesmeta</td>", about, fixed = TRUE))
  expect_match(metaUI:::create_about("Fixture", bayesian = TRUE), "<td>bayesmeta</td>", fixed = TRUE)
})
