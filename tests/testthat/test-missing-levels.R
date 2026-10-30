test_that("missing factor values become a final (Missing) level", {
  f <- factor(c("b", "a", NA), levels = c("b", "a", "unused"))
  expect_identical(metaUI:::na_to_level(f, "(Missing)"),
                   factor(c("b", "a", "(Missing)"), levels = c("b", "a", "unused", "(Missing)")))
  # An explicit NA level and an existing (Missing) level are merged, not duplicated
  expect_identical(levels(metaUI:::na_to_level(addNA(factor(c("x", NA))), "(Missing)")), c("x", "(Missing)"))
  existing <- metaUI:::na_to_level(factor(c("(Missing)", NA, "a")), "(Missing)")
  expect_identical(as.character(existing), c("(Missing)", "(Missing)", "a"))
  expect_identical(levels(existing), c("(Missing)", "a"))
  ordered <- metaUI:::na_to_level(factor(c("lo", NA), levels = c("lo", "hi"), ordered = TRUE), "(Missing)")
  expect_true(is.ordered(ordered))
  expect_identical(levels(ordered), c("lo", "hi", "(Missing)"))
  expect_identical(as.character(metaUI:::na_to_level(factor(c(NA, NA), levels = "a"), "(Missing)")), rep("(Missing)", 2))
})

test_that("prepare_data() drops unused levels unless the missing level is kept", {
  x <- fixture()
  x$region <- factor(rep(c("north", "south"), 8), levels = c("north", "south", "east"))
  x$region[1] <- NA
  x$sector <- factor(rep(c("public", "private"), 8))
  d <- prepared(x, filters = c("region", "sector"))
  expect_identical(levels(d$metaUI__filter_region), c("north", "south", "(Missing)"))
  expect_identical(sum(d$metaUI__filter_region == "(Missing)"), 1L)
  expect_identical(levels(d$metaUI__filter_sector), c("private", "public"))
  kept <- prepared(x, filters = c("region", "sector"), keep_missing_level = TRUE)
  expect_identical(levels(kept$metaUI__filter_region), c("north", "south", "east", "(Missing)"))
  expect_identical(levels(kept$metaUI__filter_sector), c("private", "public", "(Missing)"))
})
