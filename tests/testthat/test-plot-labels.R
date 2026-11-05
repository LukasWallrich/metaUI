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
    span <- diff(ggplot2::layer_scales(plot)$x$get_limits())
    # Every label, not just one. Phones are crowded: the least covered spot may hold a point.
    for (i in seq_len(nrow(labels))) {
      # A parsed label holds plotmath source; it renders as six characters ("p < .05").
      chars <- if (grepl("italic", labels$label[i], fixed = TRUE)) 6 else nchar(labels$label[i])
      label_width <- chars * 7 / (width - 70) * span
      left <- labels$x[i] - labels$hjust[i] * label_width
      inside <- fit$yi >= left & fit$yi <= left + label_width & abs(sqrt(fit$vi) - abs(labels$y[i])) <= .0375 * max(sqrt(fit$vi)) * 1.05
      expect_lte(sum(inside), if (width < 500) 1 else 0)
    }
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

test_that("forest intervals include combined rows the effects do not reach", {
  rows <- data.frame(kind = "Selected effect", study = "A", effect_id = 1:2, label = c("1", "2"), es = c(0, 10), LCL = c(-.2, 8), UCL = c(.2, 12))
  studies <- data.frame(study = "A", es = -.56, LCL = -.73, UCL = -.4)
  html <- metaUI:::metaUI_forest_html(rows, "g", studies = studies)
  left <- as.numeric(unlist(regmatches(html, gregexpr("(?<=left:)-?[0-9.]+(?=%)", html, perl = TRUE))))
  expect_gt(length(left), 0)
  expect_true(all(left >= 0 & left <= 100))
})

test_that("RVE counts studies even though it is fitted to effects", {
  row <- data.frame(k = 97, aggregated = FALSE, input_rows = 286)
  expect_equal(metaUI:::metaUI_count_unit(row, 97), "studies")
  row$k <- 286
  expect_equal(metaUI:::metaUI_count_unit(row, 97), "effects")
  row$aggregated <- TRUE; row$k <- 103
  expect_equal(metaUI:::metaUI_count_unit(row, 97), "studies")
})

test_that("a saved fit does not answer when caching is off", {
  df <- data.frame(a = 1)
  seed <- list(key = list(data = df, models = "m", correlation = .6, aggregation = "aggregate"), value = list(table = data.frame(es = 1)))
  on <- metaUI:::metaUI_fit_cache(3L, seed = seed)
  expect_true(on(df, "m")$cache_hit)
  off <- metaUI:::metaUI_fit_cache(0L, seed = seed)
  expect_error(off(df, "m"))
})

test_that("a moderator with one value in the selection is explained, not an error", {
  skip_without_app_packages()
  set.seed(1)
  d <- data.frame(study = rep(paste0("S", 1:8), each = 2), id = 1:16, yi = rnorm(16, .3, .3), vi = runif(16, .02, .08),
    same = 2005, one = "only", cat = rep(c("a", "b"), 8))
  x <- suppressMessages(prepare_data(d, "study", "yi", variance = "vi", es_id = "id",
    filters = c(Same = "same", One = "one", Cat = "cat"), categorical_filters = c("one", "cat")))
  path <- tempfile(); on.exit(unlink(path, recursive = TRUE))
  # Caching off: nothing saved for the opening, so readers always get a fresh fit.
  generate_shiny(x, "Edge", save_to_folder = path, launch_app = FALSE, options = list(fit_cache_entries = 0L))
  expect_false(file.exists(file.path(path, "opening_results.rds")))
  # The note covers the phone histogram, hidden until metaui.js draws one.
  expect_match(paste(readLines(file.path(path, "server.R")), collapse = "\n"), "metaui-hist-note", fixed = TRUE)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(path)
  env <- new.env(parent = globalenv()); suppressMessages(sys.source("global.R", env))
  defaults <- metaUI:::static_default_inputs(env); attr(defaults, "z_slider") <- NULL
  shiny::testServer(env$server, {
    do.call(session$setInputs, defaults)
    session$setInputs(go = 1)
    for (moderator in c("metaUI__filter_Same", "metaUI__filter_One")) {
      session$setInputs(moderator = moderator)
      expect_match(output$moderation_text, "do not vary", fixed = TRUE)
      expect_equal(as.character(output$moderation_table), "")
    }
    session$setInputs(moderator = "metaUI__filter_Cat")
    expect_match(output$moderation_table, "metaui-moderators", fixed = TRUE)
  })
})

test_that("crowded moderator levels become histograms, sooner on phones", {
  js <- paste(readLines(system.file("template_code", "metaui.js", package = "metaUI")), collapse = "\n")
  expect_match(js, "dotCount(track) >= (phone.matches ? 25 : 100)", fixed = TRUE)
})

test_that("correlations are shown as r, and a funnel keeps straight limits with an r axis", {
  df <- data.frame(metaUI__es_type = "ZCOR")
  expect_equal(metaUI:::metaUI_show(atanh(.5), df), .5)
  expect_equal(metaUI:::metaUI_show(.5, data.frame(metaUI__es_type = "SMD")), .5)
  set.seed(1)
  fit <- metafor::rma(yi = atanh(runif(30, -.6, .7)), sei = runif(30, .05, .3))
  plot <- metaUI:::metaUI_funnel(fit, "Correlation r", back = TRUE)
  scale <- plot$scales$get_scales("x")
  expect_equal(tanh(scale$breaks), as.numeric(gsub("\u2212", "-", scale$labels)))
  expect_match(plot$labels$y, "Fisher z", fixed = TRUE)
})

test_that("narrow intervals get three decimals", {
  expect_equal(metaUI:::metaUI_digits(.016, .033), 3L)
  expect_equal(metaUI:::metaUI_digits(c(.1, NA), c(.3, NA)), 2L)
})

test_that("links save only what narrows the data, and read older links too", {
  x <- data.frame(study = rep(LETTERS[1:6], each = 2), yi = rnorm(12), vi = .02, year = rep(2001:2006, 2), group = factor(rep(c("a", "b", "c"), 4)))
  d <- suppressMessages(prepare_data(x, "study", "yi", variance = "vi", filters = c(Year = "year", Group = "group")))
  filters <- list(list(id = "y", type = "numeric", col = "metaUI__filter_Year"), list(id = "g", type = "categorical", col = "metaUI__filter_Group"))
  full <- metaUI:::metaUI_full_selection(d, filters)
  records <- function(g) do.call(rbind, lapply(names(full), function(id) data.frame(id = id, selection = as.character(if (id == "g") g else full[[id]]))))
  everything <- metaUI:::metaUI_selection_query(records(c("a", "b", "c")), "key", NULL, d, filters)
  expect_no_match(utils::URLdecode(everything), "metaUI__|\"g\"|outliers")
  expect_setequal(metaUI:::metaUI_parse_selection(everything, "key", d, filters)$filters$g$selection, c("a", "b", "c"))
  narrowed <- metaUI:::metaUI_selection_query(records("b"), "key", NULL, d, filters)
  parsed <- metaUI:::metaUI_parse_selection(narrowed, "key", d, filters)
  expect_equal(parsed$filters$g$selection, "b")
  expect_equal(as.numeric(parsed$filters$y$selection), as.numeric(full$y))
  old <- paste0("?metaui=1&selection=", utils::URLencode(jsonlite::toJSON(list(dataset = "key", filters = records("b"), sesoi = NULL), auto_unbox = TRUE, null = "null"), reserved = TRUE))
  expect_equal(metaUI:::metaUI_parse_selection(old, "key", d, filters)$filters$g$selection, "b")
})

test_that("an author can name a custom model's interval", {
  table <- data.frame(Model = c("Custom", "Default"), es = c(.1, .2), LCL = c(0, .1), UCL = c(.2, .3), k = 5, status = "ok", reason = "", warnings = "", aggregated = FALSE,
    interval_type = c("Confidence interval", "Confidence interval (coverage model-dependent)"))
  html <- metaUI:::metaUI_estimates_html(table)
  expect_match(html, "95% CI", fixed = TRUE)
  expect_no_match(html, "metaui-row-note", fixed = TRUE)
})

test_that("confidence intervals carry no row note beside an author-defined one", {
  table <- data.frame(Model = c("Custom", "Default"), es = c(.1, .2), LCL = c(0, .1), UCL = c(.2, .3), k = 5, status = "ok", reason = "", warnings = "", aggregated = FALSE,
    interval_type = c("Author-defined interval (type and coverage unknown)", "Confidence interval (coverage model-dependent)"))
  html <- metaUI:::metaUI_estimates_html(table)
  expect_match(html, "Author-defined interval", fixed = TRUE)
  expect_no_match(html, "Confidence interval (coverage", fixed = TRUE)
})
