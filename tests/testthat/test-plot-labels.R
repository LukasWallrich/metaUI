test_that("a funnel label takes the first clear spot, else the least covered", {
  spots <- data.frame(x = c(0, 1, 2), y = 0, hjust = 0)
  clear <- metaUI:::metaUI_label_spot(spots, .5, .2, data.frame(x = c(.1, 1.2), y = 0))
  expect_equal(clear$x, 2)
  crowded <- metaUI:::metaUI_label_spot(spots, .5, .2, data.frame(x = c(.1, .2, 1.1, 2.1, 2.2), y = 0))
  expect_equal(crowded$x, 1)
})

test_that("funnel labels stay clear of the points across selections and widths", {
  skip_if_not_installed("metafor")
  for (seed in 1:12) for (width in c(340, 1000)) {
    set.seed(seed)
    k <- sample(15:60, 1)
    fit <- metafor::rma(yi = rnorm(k, .2, .3), sei = runif(k, .05, .4))
    plot <- metaUI:::metaUI_funnel(fit, "Hedges g", width = width)
    is_label <- vapply(plot$layers, function(layer) inherits(layer$geom, "GeomText"), logical(1))
    labels <- do.call(rbind, lapply(which(is_label), function(i) ggplot2::layer_data(plot, i)[c("x", "y", "label", "hjust")]))
    expect_equal(nrow(labels), 4)
    limit <- labels[labels$label == "95% limits", ]
    span <- diff(ggplot2::layer_scales(plot)$x$get_limits())
    label_width <- 10 * 7 / (width - 70) * span
    left <- limit$x - limit$hjust * label_width
    inside <- fit$yi >= left & fit$yi <= left + label_width & abs(sqrt(fit$vi) - abs(limit$y)) <= .0375 * max(sqrt(fit$vi)) * 1.05
    expect_lte(sum(inside), if (width < 500) 1 else 0)
  }
})

test_that("the z-curve axis ends on a labelled tick above every bar", {
  skip_if_not_installed("zcurve")
  set.seed(1)
  fit <- suppressWarnings(zcurve::zcurve(z = abs(rnorm(80, 1.5, 1.2)), method = "EM"))
  plot <- metaUI:::metaUI_zcurve_plot(fit, metaUI:::metaUI_plot_colours(family = ""))
  scale <- plot$scales$get_scales("y")
  bars <- plot$layers[vapply(plot$layers, function(layer) inherits(layer$geom, "GeomRect"), logical(1))]
  expect_gte(max(scale$breaks), max(unlist(lapply(bars, function(layer) layer$data$ymax))))
  expect_equal(max(scale$limits), max(scale$breaks))
})

test_that("a diamond always spans the interval, including on moderator rows", {
  scale <- metaUI:::metaUI_track_scale(c(-1, 1))
  row <- metaUI:::metaUI_track_row(scale, .2, -.1, .5, dots = c(0, .1), diamond = TRUE)
  expect_match(row, "metaui-track-diamond", fixed = TRUE)
  expect_match(row, "metaui-track-dot", fixed = TRUE)
  expect_no_match(row, "metaui-track-ci", fixed = TRUE)
  expect_no_match(metaUI:::metaUI_track_row(scale, .2, .2, .2, diamond = TRUE), "metaui-track-diamond", fixed = TRUE)
})
