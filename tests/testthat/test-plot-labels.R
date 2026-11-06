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

# The histogram itself is exercised in a real browser: tools/browser-smoke.cjs.

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

test_that("the forest plot draws one page and sorts on the server", {
  n <- 120
  rows <- data.frame(kind = "Selected effect", study = sprintf("S%03d", 1:n), effect_id = "1", label = sprintf("S%03d: 1", 1:n), es = (1:n) / 200, LCL = (1:n) / 200 - .2, UCL = (1:n) / 200 + .2)
  page <- function(...) metaUI:::metaUI_forest_html(rows, "g", ...)
  count <- function(html, pattern) lengths(regmatches(html, gregexpr(pattern, html, fixed = TRUE)))
  first <- page()
  expect_equal(count(first, "metaui-forest-single"), 50)
  expect_match(first, "Studies 1\u201350 of 120", fixed = TRUE)
  expect_match(first, 'metaui-forest-prev" data-page="0" disabled', fixed = TRUE)
  expect_true(grepl(">S001<", first, fixed = TRUE) && !grepl(">S051<", first, fixed = TRUE))
  last <- page(page = 3)
  expect_equal(count(last, "metaui-forest-single"), 20)
  expect_match(last, "Studies 101\u2013120 of 120", fixed = TRUE)
  # Past the end clamps to the last page; the axis does not depend on the page.
  expect_match(page(page = 99), "Studies 101\u2013120", fixed = TRUE)
  ticks <- function(html) regmatches(html, gregexpr('metaui-axis-tick" style="[^"]+"', html))[[1]]
  expect_identical(ticks(first), ticks(last))
  sorted <- page(sort = "estimate")
  expect_true(grepl(">S120<", sorted, fixed = TRUE) && !grepl(">S001<", sorted, fixed = TRUE))
  expect_match(sorted, 'aria-pressed="true"', fixed = TRUE)
  expect_no_match(metaUI:::metaUI_forest_html(rows[1:10, ], "g"), "metaui-forest-pager", fixed = TRUE)
})

test_that("a declared primary model leads the forest summaries", {
  d <- suppressMessages(prepare_data(data.frame(study = rep(LETTERS[1:8], each = 2), yi = rnorm(16, .2, .2), vi = .02), "study", "yi", variance = "vi"))
  estimates <- data.frame(Model = c("Random-Effects Multilevel Model", "Robust Variance Estimation", "Mine"), aggregated = FALSE, status = "ok",
    es = c(.1, .2, .3), LCL = 0, UCL = .4, fit_es = c(.1, .2, .3), fit_LCL = 0, fit_UCL = .4)
  summaries <- function(...) { r <- metaUI:::metaUI_forest_rows(d, estimates, ...); r$label[r$kind == "Model summary"] }
  expect_equal(summaries(), c("Random-Effects Multilevel Model", "Robust Variance Estimation"))
  expect_equal(summaries(primary = "Mine"), c("Mine", "Random-Effects Multilevel Model", "Robust Variance Estimation"))
  expect_equal(summaries(primary = "Robust Variance Estimation"), c("Robust Variance Estimation", "Random-Effects Multilevel Model"))
})

test_that("reference downloads cover the selection, without repeats, in the authors' wording", {
  info <- data.frame(study = c("B 2", "A 1", "B 1", "C 1", "D 1"),
    reference = c("Brown (2001).", "Adams (2000).", "Brown (2001).", NA, NA), bibtex = c("@b", "@a", "@b", NA, NA),
    authors = c(NA, NA, NA, "Cole, Ann; Day, Bo", NA), title = c(NA, NA, NA, "Teams", "Untitled note"), year = c(NA, NA, NA, "2005", NA), doi = c(NA, NA, NA, "10.1000/x", NA), url = NA)
  all <- metaUI:::metaUI_selected_references(info, info$study)
  expect_equal(all$Reference[1:2], c("Adams (2000).", "Brown (2001)."))
  expect_equal(sum(all$Reference == "Brown (2001)."), 1)
  cole <- all[all$Study == "C 1", ]
  expect_match(cole$bibtex, "author = {Cole, Ann and Day, Bo}", fixed = TRUE)
  expect_equal(cole$Link, "https://doi.org/10.1000/x")
  expect_true(is.na(all$bibtex[all$Study == "D 1"]))
  expect_equal(metaUI:::metaUI_selected_references(info, "A 1")$Study, "A 1")
  expect_equal(nrow(metaUI:::metaUI_selected_references(info, "nobody")), 0)
})

test_that("a study known only by its reference keeps its study information", {
  d <- suppressMessages(prepare_data(data.frame(study = c("A", "B", "C"), yi = c(.1, .2, .3), vi = .02), "study", "yi", variance = "vi"))
  info <- metaUI:::validate_study_info(data.frame(study = c("A", "B", "C"), reference = c("Adams (2000).", NA, NA), bibtex = c(NA, "@b", NA)), d)
  expect_equal(info$study, c("A", "B"))
})

test_that("a range that really narrows large values stays in the link", {
  x <- data.frame(study = c("A", "B", "C"), yi = c(.1, .2, .3), vi = .02, big = c(200000000, 200000001, 200000002))
  d <- suppressMessages(prepare_data(x, "study", "yi", variance = "vi", filters = c(Big = "big")))
  filters <- list(list(id = "b", type = "numeric", col = "metaUI__filter_Big"))
  full <- metaUI:::metaUI_full_selection(d, filters)
  records <- rbind(data.frame(id = "outliers_z_scores", selection = as.character(full$outliers_z_scores)),
    data.frame(id = "b", selection = c("200000000", "200000001")), data.frame(id = "b_include_NA", selection = "TRUE"))
  parsed <- metaUI:::metaUI_parse_selection(metaUI:::metaUI_selection_query(records, "key", NULL, d, filters), "key", d, filters)
  expect_equal(as.numeric(parsed$filters$b$selection), c(200000000, 200000001))
})

test_that("BibTeX-only studies stay in the download and reference-only studies show their text", {
  info <- data.frame(study = c("A", "B"), reference = c(NA, "Baker & Co (2001)."), bibtex = c("@a{x}", NA), url = c(NA, "https://example.org/b"))
  out <- metaUI:::metaUI_selected_references(info, info$study)
  expect_setequal(out$Study, c("A", "B"))
  details <- metaUI:::metaUI_study_details(info)
  expect_match(details[2], "Baker &amp; Co (2001).", fixed = TRUE)
  expect_match(details[2], "https://example.org/b", fixed = TRUE)
})

test_that("a primary summary is not called a confidence interval unless it is one", {
  rows <- data.frame(kind = c("Selected effect", "Model summary"), study = c("A", NA), effect_id = c("1", NA), label = c("A: 1", "Mine"), es = .2, LCL = .1, UCL = .3)
  attr(rows, "interval_types") <- c(Mine = "95% central credible interval (prior-dependent)")
  html <- metaUI:::metaUI_forest_html(rows, "g")
  expect_match(html, "95% central credible interval", fixed = TRUE)
  expect_no_match(html, "95% CI", fixed = TRUE)
  attr(rows, "interval_types") <- c(Mine = "Confidence interval")
  expect_match(metaUI:::metaUI_forest_html(rows, "g"), "95% CI", fixed = TRUE)
  table <- data.frame(Model = "Mine", es = .1, LCL = 0, UCL = .2, k = 5, status = "ok", reason = "", warnings = "", aggregated = FALSE, interval_type = "95% central credible interval (prior-dependent)")
  expect_no_match(metaUI:::metaUI_estimates_html(rbind(table, transform(table, Model = "Other")), 1L), "its 95% CI", fixed = TRUE)
})

test_that("an invalid forest page falls back to the first page", {
  rows <- data.frame(kind = "Selected effect", study = sprintf("S%03d", 1:60), effect_id = "1", label = sprintf("S%03d: 1", 1:60), es = .1, LCL = 0, UCL = .2)
  for (bad in list(NA, "bad", integer(), c(1, 2))) expect_match(metaUI:::metaUI_forest_html(rows, "g", page = bad), "Studies 1\u201350", fixed = TRUE)
})

test_that("a value rounding to zero drops its sign", {
  expect_equal(metaUI:::metaUI_minus(c("-0.000", "-0", "-0.001", "0.000")), c("0.000", "0", "\u22120.001", "0.000"))
})

test_that("moderator effects can be toggled, and the axis fits what is shown", {
  args <- list(levels = c("a", "b"), estimate = c(.02, .04), lcl = c(.01, .03), ucl = c(.03, .05), k = c(2L, 1L),
    effects = c("a", "a", "b"), values = c(-.9, .8, .03), labels = c("x", "y", "z"), axis_title = "r", ref = .03)
  off <- do.call(metaUI:::metaUI_moderator_html, c(args, show_effects = FALSE))
  on <- do.call(metaUI:::metaUI_moderator_html, c(args, show_effects = TRUE))
  expect_no_match(off, "metaui-track-dot", fixed = TRUE)
  expect_match(off, 'data-value="hide" aria-pressed="true">hidden', fixed = TRUE)
  expect_no_match(off, "Grey dots", fixed = TRUE)
  expect_equal(lengths(regmatches(on, gregexpr("metaui-track-dot", on, fixed = TRUE))), 3)
  expect_match(on, 'data-value="show" aria-pressed="true">shown', fixed = TRUE)
  expect_match(on, "metaui-hist-note", fixed = TRUE)
  # Off: only the estimates set the axis, so a 0.02-wide interval takes a visible share of it.
  width <- function(html) { x <- regmatches(html, regexpr('metaui-track-diamond" style="left:[0-9.]+%;width:[0-9.]+', html)); as.numeric(sub(".*width:", "", x)) }
  expect_gt(width(off), 5 * width(on))
})

test_that("the Summary table leads with the primary and groups other estimators by level", {
  table <- data.frame(Model = c("Study A", "Effect B", "Primary", "Effect C"), es = c(.1, .2, .3, .4), LCL = c(0, .1, .2, .3), UCL = c(.2, .3, .4, .5),
    k = 5, status = "ok", reason = "", warnings = "", aggregated = c(TRUE, FALSE, FALSE, FALSE), interval_type = "Confidence interval")
  html <- metaUI:::metaUI_estimates_html(table, 3L)
  at <- function(x) regexpr(x, html, fixed = TRUE)[[1]]
  expect_true(at(">Primary") < at(">Other effect-level estimators") && at(">Other effect-level estimators") < at(">Effect B") && at(">Effect B") < at(">Effect C") &&
    at(">Effect C") < at(">Study-level estimators") && at(">Study-level estimators") < at(">Study A"))
  expect_match(html, "primary, effect-level fit", fixed = TRUE)
  expect_no_match(html, "Level of fit", fixed = TRUE)
  # Rows without a primary keep their groups, without calling anything "other".
  plain <- metaUI:::metaUI_estimates_html(table)
  expect_match(plain, ">Effect-level estimators", fixed = TRUE)
  expect_no_match(plain, "Other", fixed = TRUE)
  # With a study-level primary, the effect-level rows are not "other" ones.
  study <- metaUI:::metaUI_estimates_html(table, 1L)
  expect_match(study, ">Effect-level estimators", fixed = TRUE)
  expect_no_match(study, "Other effect-level", fixed = TRUE)
})

test_that("saved opening results are marked to survive the empty values sent on connecting", {
  expect_match(as.character(metaUI:::metaUI_opening_output(shiny::uiOutput("x"), "<p>saved</p>")), "data-metaui-opening", fixed = TRUE)
  expect_no_match(as.character(metaUI:::metaUI_opening_output(shiny::uiOutput("x"), NULL)), "data-metaui-opening", fixed = TRUE)
})

test_that("the Sample table can show one row per study without averaging effects", {
  effects <- data.frame(Study = c("B", "A", "A", "B"), metaUI__key = c("B", "A", "A", "B"), N = c(40, 10, 12, 40),
    `Effect size` = c(.1, .2, .3, .4), Year = c(2001, 2002, 2002, NA), Domain = factor(c("x", "y", "z", "x")),
    check.names = FALSE)
  studies <- metaUI:::metaUI_study_rows(effects)
  expect_equal(studies$Study, c("B", "A"))
  expect_equal(studies$Effects, c(2L, 2L))
  expect_equal(names(studies)[1:2], c("Study", "Effects"))
  expect_false("Effect size" %in% names(studies))
  expect_equal(studies$N, c("40", "10–12"))
  expect_equal(studies$Year, c(2001, 2002))
  expect_equal(studies$Domain, c("x", "several"))
})

test_that("study rows keep exact range ends, and categories are counted once", {
  effects <- data.frame(Study = c("A", "A"), metaUI__key = "A", N = c(10001, 10002), check.names = FALSE)
  expect_equal(metaUI:::metaUI_study_rows(effects)$N, "10001–10002")
  x <- c(rep(LETTERS[1:11], each = 2), rep("Other", 5))
  counts <- metaUI:::summarise_categorical(x, "Group")
  expect_equal(sum(counts$Count), length(x))
  expect_equal(counts$Count[counts$Group == "Other"], 5 + 2 * 2) # its own 5, and the two levels beyond the top nine
})

test_that("view switches are one pressed option among labelled buttons", {
  html <- metaUI:::metaUI_switch("One row per", "metaui-sample-view", c(effects = "effect", studies = "study"), "studies")
  expect_match(html, 'role="group" aria-label="One row per"', fixed = TRUE)
  expect_match(html, 'data-value="studies" aria-pressed="true">study', fixed = TRUE)
  expect_match(html, 'data-value="effects" aria-pressed="false">effect', fixed = TRUE)
})
