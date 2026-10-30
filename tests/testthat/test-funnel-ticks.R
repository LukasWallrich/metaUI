test_that("funnel SE ticks are round, start at zero and stay inside the range", {
  expect_equal(metaUI:::metaUI_funnel_ticks(c(.09, .18, .27, .36)), seq(0, .3, .1))
  for (upper in c(.0036, .36, 3.6, 36)) {
    ticks <- metaUI:::metaUI_funnel_ticks(c(NA, upper))
    expect_equal(ticks[1], 0)
    expect_true(all(diff(ticks) > 0))
    expect_true(all(ticks <= upper))
  }
  expect_equal(metaUI:::metaUI_funnel_ticks(c(0, NA)), 0)
})
