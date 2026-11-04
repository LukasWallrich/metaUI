# This file is copied verbatim into generated apps. Keep it independent of metaUI.

metaUI_code_multilevel <- "metaUI_multilevel_fit(df)"
metaUI_code_rve <- "metaUI_rve_fit(df)"

# Default estimator code. Renamed models are recognised by this exact code
# (ignoring whitespace), so their input checks and sign handling still apply.
metaUI_default_code <- c(
  "Trim-and-fill" = 'metaUI_trimfill(df)',
  "P-uniform star" = 'puniform::puni_star(
                    yi = df$metaUI__effect_size, vi = df$metaUI__variance,
                    alpha = .05,
                    side = "right", method = "ML", boot = FALSE
                    )',
  "Hedges-Vevea Selection Model" = 'weightr::weightfunct(df$metaUI__effect_size,
                df$metaUI__variance, steps = c(0.025, 1), fe = FALSE)',
  "P-Curve (first value)" = 'metaUI_pcurve_fit(df, "first")',
  "P-Curve (last value)" = 'metaUI_pcurve_fit(df, "last")',
  "Precision Effect Test" = 'lm(metaUI__effect_size ~ sqrt(metaUI__variance), data = df, weights = 1 / metaUI__variance)',
  "Precision Effect Estimate using Standard Error" = 'lm(metaUI__effect_size ~ metaUI__variance, data = df, weights = 1 / metaUI__variance)'
)


# ML/REML with unmodified Hartung-Knapp intervals (no ad hoc correction).
metaUI_univariate <- function(y, se, method = "ML", control = list()) {
  fit <- metafor::rma.uni(yi = y, sei = se, method = method,
    test = if (sum(!is.na(y) & !is.na(se)) == 1L) "z" else "knha", control = control)
  if (fit$k == 1L) fit$tau2 <- NA_real_
  fit
}

# Egger's multiplicative-dispersion regression, matching meta's k.min = 3.
metaUI_egger <- function(y, se) {
  keep <- !is.na(y) & !is.na(se)
  if (any(!keep)) warning(sum(!keep), " observation(s) dropped due to missing values")
  y <- y[keep]; se <- se[keep]
  if (length(y) < 3L) return(list(k = length(y), k.min = 3))
  if (length(unique(se)) == 1L)
    stop("Test for small-study effects not feasible as all studies have the same precision.", call. = FALSE)
  base <- suppressWarnings(metafor::rma.uni(y, sei = se, method = "DL", test = "t"))
  fit <- suppressWarnings(metafor::regtest(base, model = "lm", ret.fit = TRUE))
  stats <- suppressWarnings(summary(fit$fit))
  list(estimate = c(bias = unname(stats::coef(fit$fit)[2]), se.bias = stats$coefficients[2, 2]),
       tau = stats$sigma, statistic = fit$zval, p.value = fit$pval)
}

# Summary-centred normal pseudo-CI; grey p < .05 contours centred on zero.
# The two centres deliberately differ, as in meta::funnel.meta.
metaUI_funnel_ticks <- function(se) {
  upper <- max(se[is.finite(se)], 0)
  ticks <- pretty(c(0, upper), n = 5)
  ticks[ticks >= 0 & ticks <= upper]
}

metaUI_funnel <- function(fit, xlab, colours = metaUI_plot_colours()) {
  centre <- as.numeric(fit$b)
  se <- sqrt(fit$vi); yi <- as.numeric(fit$yi)
  z <- stats::qnorm(.975)
  ymax <- max(se) * 1.05; half <- z * ymax
  limits <- range(yi, centre - half, centre + half, -half, half)
  limits <- limits + c(-1, 1) * .04 * diff(limits)
  region <- data.frame(x = c(limits[1], limits[1], -half, 0, limits[2], limits[2], half, 0),
    y = c(0, ymax, ymax, 0, 0, ymax, ymax, 0), side = rep(1:2, each = 4))
  ggplot2::ggplot() +
    ggplot2::geom_polygon(data = region, ggplot2::aes(.data$x, .data$y, group = .data$side), fill = colours$shade) +
    ggplot2::annotate("segment", x = centre, xend = centre, y = 0, yend = ymax, linetype = 2, linewidth = .4, colour = colours$accent) +
    ggplot2::annotate("path", x = centre + c(-half, 0, half), y = c(ymax, 0, ymax), linetype = 2, linewidth = .4, colour = colours$accent) +
    ggplot2::geom_point(data = data.frame(yi = yi, se = se), ggplot2::aes(.data$yi, .data$se), colour = colours$slate, alpha = .6, size = 2) +
    ggplot2::annotate("text", x = limits[1] + .012 * diff(limits), y = ymax * .05, label = 'italic(p) < ".05"', parse = TRUE, hjust = 0, size = 4.5, colour = colours$slate, family = colours$family) +
    ggplot2::annotate("text", x = centre + z * ymax * .93, y = ymax * .93, label = "95% limits", hjust = -.15, size = 4.5, colour = colours$accent, family = colours$family) +
    ggplot2::annotate("text", x = 0, y = ymax * .96, label = 'italic(p) >= ".05"', parse = TRUE, size = 4.5, colour = colours$slate, family = colours$family) +
    ggplot2::annotate("text", x = centre + z * ymax * .03 + .012 * diff(limits), y = ymax * .03, label = paste0("pooled ", metaUI_minus(formatC(centre, format = "f", digits = 2))), hjust = 0, size = 4.5, colour = colours$accent, family = colours$family) +
    ggplot2::scale_y_reverse(breaks = metaUI_funnel_ticks(se), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(limits = limits, expand = c(0, 0), breaks = pretty(limits, 8), labels = function(x) metaUI_minus(format(x, trim = TRUE))) +
    ggplot2::labs(x = metaUI_label_expr(xlab), y = "Standard error") +
    metaUI_plot_theme(colours) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

# Effect-size labels ending in a statistic's single letter ("Hedges g", "Correlation r") set it in italics.
metaUI_label_html <- function(label) {
  sub("(^|[[:space:]])([A-Za-z])$", "\\1<i>\\2</i>", htmltools::htmlEscape(label))
}
metaUI_label_expr <- function(label) {
  if (!grepl("(^|[[:space:]])[A-Za-z]$", label)) return(label)
  rest <- sub("[[:space:]]*[A-Za-z]$", "", label)
  letter <- sub(".*([A-Za-z])$", "\\1", label)
  if (nzchar(rest)) bquote(.(rest) ~ italic(.(letter))) else bquote(italic(.(letter)))
}

# True minus signs for display; downloads keep plain numbers.
metaUI_minus <- function(x) gsub("-", "\u2212", x, fixed = TRUE)

# Colours and font shared by every plot. The font applies on screen only: PDF devices
# reject system font names, so downloads use their device default.
metaUI_plot_colours <- function(accent = "#1d5c48", ink = "#1a2530", grid = "#e3e7ea", background = "#FFFFFF",
                                slate = "#55626e", shade = "#eceff1", bars = "#b4bcc4", attention = "#8a4b00", family = metaUI_plot_family()) {
  list(accent = accent, ink = ink, grid = grid, background = background, slate = slate,
       shade = shade, bars = bars, attention = attention, family = family)
}

# Plots use the interface font where the graphics device can load it (ragg, on a server);
# elsewhere, including webR, Arial or the device default stands in.
metaUI_plot_family <- function(font_dir = file.path("www", "fonts")) {
  regular <- file.path(font_dir, "plot-regular.ttf")
  if (!file.exists(regular) || !requireNamespace("systemfonts", quietly = TRUE) || !requireNamespace("ragg", quietly = TRUE)) return("Arial")
  tryCatch({
    systemfonts::register_font("metaUI Sans", plain = regular, bold = file.path(font_dir, "plot-bold.ttf"),
      italic = file.path(font_dir, "plot-italic.ttf"), bolditalic = file.path(font_dir, "plot-bold.ttf"))
    "metaUI Sans"
  }, error = function(e) "Arial")
}

metaUI_base_par <- function(colours) {
  list(family = colours$family, col = colours$ink, col.axis = colours$slate, col.lab = colours$ink, fg = colours$slate,
       bg = colours$background, las = 1, mgp = c(2.7, .6, 0), tcl = -.25, cex = 1.08, cex.axis = 1, cex.lab = 1, font.lab = 1, bty = "n")
}

# The zcurve package draws fixed colours; recolour its own ggplot layers. Statistics are
# untouched: its annotations move out of the plot, into the "statistics" attribute.
metaUI_zcurve_plot <- function(fit, colours) {
  plot <- zcurve::plot.zcurve(fit, annotation = TRUE, plot_type = "ggplot", main = "")
  is_text <- vapply(plot$layers, function(layer) inherits(layer$geom, "GeomText"), logical(1))
  statistics <- unlist(lapply(plot$layers[is_text], function(layer) as.character(layer$data$label)))
  threshold <- stats::qnorm(fit$control$sig_level / 2, lower.tail = FALSE)
  plot$layers <- lapply(plot$layers[!is_text], function(layer) {
    params <- layer$aes_params
    if (inherits(layer$geom, "GeomRect")) {
      # The package widens the last bin before the threshold. Same density, drawn as regular bins.
      bins <- layer$data
      width <- stats::median(bins$xmax - bins$xmin)
      wide <- bins$xmax - bins$xmin > 1.5 * width
      if (any(wide)) {
        extra <- bins[wide, , drop = FALSE]
        extra$xmin <- bins$xmin[wide] + width
        bins$xmax[wide] <- bins$xmin[wide] + width
        layer$data <- rbind(bins, extra)
      }
      # Both groups opaque, so the significant bars differ in hue rather than weight.
      params$fill <- if (identical(params$colour, "blue")) grDevices::colorRampPalette(c(colours$background, colours$accent))(100)[62] else colours$bars
      params$alpha <- 1
      params$colour <- colours$background
      params$linewidth <- .3
    } else if (inherits(layer$geom, "GeomVline")) {
      # The fitting threshold coincides with the significance line by default: draw one line.
      if (identical(params$colour, "blue") && isTRUE(all.equal(fit$control$a, threshold))) params$alpha <- 0
      params$colour <- colours$ink
      params$linewidth <- .4
      params$linetype <- 2
    } else {
      params$colour <- colours$ink
      params$linewidth <- .8
    }
    layer$aes_params <- params
    layer
  })
  plot <- plot + ggplot2::annotate("text", x = threshold + .08, y = Inf, vjust = 1.5, hjust = 0, size = 4.5, colour = colours$ink, family = colours$family, parse = TRUE,
      label = sprintf('italic(z) == "%s" * "," ~ italic(p) == "%s"', round(threshold, 2), sub("^0", "", format(fit$control$sig_level)))) +
    ggplot2::annotate("text", x = threshold - .08, y = Inf, vjust = 3.5, hjust = 1, size = 4.5, colour = colours$slate, family = colours$family, label = "not significant") +
    ggplot2::annotate("text", x = threshold + .08, y = Inf, vjust = 3.5, hjust = 0, size = 4.5, colour = colours$accent, family = colours$family, label = "significant") +
    metaUI_plot_theme(colours)
  # The curve's name sits beyond its end, in the right margin, clear of the data whatever
  # the selection; tests beyond the plot's end are counted beneath it.
  right <- max(ggplot2::layer_scales(plot)$x$get_limits())
  top <- max(ggplot2::layer_scales(plot)$y$get_limits())
  line <- plot$layers[[which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1)))[1]]]$data
  end <- line[which.max(line$x), ]
  beyond <- sum(fit$data > right)
  plot <- plot +
    ggplot2::annotate("text", x = Inf, y = end$y, hjust = -.05, size = 4.5, colour = colours$ink, family = colours$family, parse = TRUE, label = 'italic(z) * "-curve fit"') +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 62, 4, 4))
  if (beyond > 0L) plot <- plot + ggplot2::annotate("text", x = Inf, y = end$y + top * .14, hjust = -.05, size = 4.2, lineheight = .9, colour = colours$slate, family = colours$family,
    label = paste0(beyond, if (beyond == 1L) " test" else " tests", "\nbeyond ", format(right)))
  plot$coordinates$clip <- "off"
  x_scale <- plot$scales$get_scales("x")
  if (!is.null(x_scale)) x_scale$name <- expression(italic(z) * "-score")
  y_scale <- plot$scales$get_scales("y")
  if (!is.null(y_scale)) y_scale$expand <- ggplot2::expansion(mult = c(0, .02))
  # The tallest bar stays under a labelled gridline.
  plot <- plot + ggplot2::expand_limits(y = ceiling(top))
  attr(plot, "statistics") <- gsub(" ,", ", ", statistics, fixed = TRUE)
  plot
}

# The p-curve's values (pcurve()$PlotData) in the same plot system as every other chart.
metaUI_pcurve_plot <- function(data, colours) {
  d <- data.frame(p = data[[1]], observed = data[[2]], power = data[[3]], flat = data[[4]])
  top <- min(110, max(d$observed, d$power, d$flat) * 1.16 + 4)
  step <- top * .05
  # A value sits above its point, clear of the reference lines; under both of them it goes below.
  below <- d$observed - 2 * step > 0 & d$observed < pmin(d$flat, d$power)
  height <- ifelse(below, d$observed - step * .8, vapply(seq_len(nrow(d)), function(i) {
    y <- d$observed[i] + step * .8
    for (try in 1:4) if (min(abs(y - c(d$flat[i], d$power[i]))) < step * .7) y <- y + step * .6
    y
  }, numeric(1)))
  ends <- c(observed = d$observed[5], power = d$power[5], flat = d$flat[5])
  placed <- ends
  for (name in names(sort(ends))[-1]) placed[name] <- max(ends[name], max(placed[ends <= ends[name] & names(ends) != name]) + step * 1.1)
  text <- function(...) ggplot2::annotate("text", size = 4.5, family = colours$family, ...)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$p)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$flat), colour = colours$slate, linetype = 3, linewidth = .5) +
    ggplot2::geom_line(ggplot2::aes(y = .data$power), colour = colours$slate, linetype = 5, linewidth = .5) +
    ggplot2::geom_line(ggplot2::aes(y = .data$observed), colour = colours$accent, linewidth = .8) +
    ggplot2::geom_point(ggplot2::aes(y = .data$observed), colour = colours$accent, size = 1.8) +
    # Values sit directly above their points, or below where the reference lines leave no room.
    text(x = d$p, y = height, label = paste0(round(d$observed), "%"), colour = colours$accent) +
    text(x = .0511, y = placed[["observed"]], label = "Observed", hjust = 0, colour = colours$accent, fontface = "bold") +
    text(x = .0511, y = placed[["power"]], label = "33% power", hjust = 0, colour = colours$slate) +
    text(x = .0511, y = placed[["flat"]], label = "No effect", hjust = 0, colour = colours$slate) +
    ggplot2::scale_x_continuous(breaks = d$p, labels = sub("^0", "", format(d$p, nsmall = 2)), name = NULL) +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), labels = function(x) paste0(x, "%"), name = "Share of significant results", expand = c(0, 0)) +
    # Two runs for the axis title, because plotmath leaves a gap after an italic letter.
    ggplot2::annotate("text", x = .03, y = -Inf, vjust = 3.7, hjust = 1, label = "p", fontface = "italic", size = 4.75, colour = colours$ink, family = colours$family) +
    ggplot2::annotate("text", x = .03, y = -Inf, vjust = 3.7, hjust = 0, label = "-value", size = 4.75, colour = colours$ink, family = colours$family) +
    ggplot2::coord_cartesian(xlim = c(.0095, .0505), ylim = c(0, top), expand = FALSE, clip = "off") +
    metaUI_plot_theme(colours) +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 92, 28, 4))
}

metaUI_plot_theme <- function(colours, family = colours$family) {
  ggplot2::theme_minimal(base_size = 13, base_family = family) +
    ggplot2::theme(text = ggplot2::element_text(colour = colours$ink),
      axis.text = ggplot2::element_text(colour = colours$slate, size = 13),
      axis.title = ggplot2::element_text(colour = colours$ink, size = 13.5),
      plot.background = ggplot2::element_rect(fill = colours$background, colour = NA),
      panel.background = ggplot2::element_rect(fill = colours$background, colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = colours$grid, linewidth = .4),
      axis.line.x = ggplot2::element_line(colour = colours$slate, linewidth = .3),
      axis.ticks.x = ggplot2::element_line(colour = colours$slate, linewidth = .3))
}

# Shared axis for intervals drawn in table rows. Positions are percentages of the cell,
# so the drawing follows the column width and stays sharp at any pixel density.
metaUI_track_scale <- function(values) {
  values <- c(0, values[is.finite(values)])
  ticks <- pretty(values, n = 5)
  limits <- range(values)
  pad <- max(diff(limits), .1) * .04
  limits <- limits + c(-pad, pad)
  ticks <- ticks[ticks >= limits[1] & ticks <= limits[2]]
  list(limits = limits, ticks = ticks, at = function(x) 100 * (x - limits[1]) / diff(limits))
}

metaUI_track_style <- function(left, width = NULL) {
  paste0("left:", round(left, 2), "%", if (!is.null(width)) paste0(";width:", round(width, 2), "%"))
}

metaUI_track_grid <- function(scale, band = NULL) {
  paste0(
    if (!is.null(band)) sprintf('<i class="metaui-track-band" style="%s"></i>',
      metaUI_track_style(scale$at(max(-band, scale$limits[1])), scale$at(min(band, scale$limits[2])) - scale$at(max(-band, scale$limits[1])))),
    paste(sprintf('<i class="%s" style="%s"></i>', ifelse(scale$ticks == 0, "metaui-track-zero", "metaui-track-grid"),
      metaUI_track_style(scale$at(scale$ticks))), collapse = ""),
    if (!0 %in% scale$ticks) sprintf('<i class="metaui-track-zero" style="%s"></i>', metaUI_track_style(scale$at(0))))
}

metaUI_track_axis <- function(scale, title) {
  paste0('<div class="metaui-track metaui-axis" aria-hidden="true"><span class="metaui-axis-title">', metaUI_label_html(title), '</span>',
    paste(sprintf('<span class="metaui-axis-tick" style="%s">%s</span>', metaUI_track_style(scale$at(scale$ticks)),
      metaUI_minus(format(scale$ticks, trim = TRUE))), collapse = ""), '</div>')
}

metaUI_track_row <- function(scale, es, lcl, ucl, band = NULL, dots = NULL, dot_labels = NULL, size = NULL, ref = NULL, diamond = FALSE, combined = FALSE, dot_sizes = NULL, dot_shift = FALSE) {
  # A diamond spans the interval, with its tallest point at the estimate.
  if (diamond && is.finite(es) && is.finite(lcl) && is.finite(ucl) && ucl > lcl)
    return(paste0('<div class="metaui-track" aria-hidden="true">', metaUI_track_grid(scale, band),
      if (!is.null(ref) && is.finite(ref)) sprintf('<i class="metaui-track-ref" style="%s"></i>', metaUI_track_style(scale$at(ref))),
      sprintf('<i class="metaui-track-diamond" style="%1$s;clip-path:polygon(0 50%%,%2$.1f%% 0,100%% 50%%,%2$.1f%% 100%%)"></i>',
        metaUI_track_style(scale$at(lcl), scale$at(ucl) - scale$at(lcl)), 100 * (es - lcl) / (ucl - lcl)), "</div>"))
  paste0('<div class="metaui-track" aria-hidden="true">', metaUI_track_grid(scale, band),
    if (!is.null(ref) && is.finite(ref)) sprintf('<i class="metaui-track-ref" style="%s"></i>', metaUI_track_style(scale$at(ref))),
    if (length(dots)) paste(sprintf('<i class="metaui-track-dot" style="%s%s"%s></i>', metaUI_track_style(scale$at(dots)),
      paste0(if (length(dot_sizes)) sprintf(";width:%1$.1fpx;height:%1$.1fpx;margin:%2$.2fpx 0 0 %2$.2fpx", dot_sizes, -dot_sizes / 2) else "",
        # Repeatable offsets under the interval, for sight only: position across the row carries the value.
        if (dot_shift) sprintf(";transform:translateY(%.1fpx)", 5 + 8 * ((seq_along(dots) * .618034) %% 1)) else ""),
      if (length(dot_labels)) sprintf(' title="%s"', htmltools::htmlEscape(dot_labels, attribute = TRUE)) else ""), collapse = ""),
    if (is.finite(lcl) && is.finite(ucl)) sprintf('<i class="metaui-track-ci" style="%s"></i>', metaUI_track_style(scale$at(lcl), scale$at(ucl) - scale$at(lcl))),
    if (is.finite(es)) sprintf('<i class="metaui-track-point%s" style="%s%s"></i>', if (combined) " metaui-track-combined" else "", metaUI_track_style(scale$at(es)),
      if (!is.null(size)) sprintf(";width:%1$.1fpx;height:%1$.1fpx;margin:%2$.2fpx 0 0 %2$.2fpx", size, -size / 2) else ""), '</div>')
}

# First screen: the selected effects themselves, before any model is fitted.
metaUI_effects_strip <- function(values, axis_title) {
  scale <- metaUI_track_scale(values)
  # A dot histogram: one dot per effect, stacked within narrow bins.
  width <- diff(scale$limits) / 120
  bins <- round(values / width) * width
  rank <- stats::ave(seq_along(bins), bins, FUN = seq_along)
  step <- min(5.5, 110 / max(rank))
  height <- ceiling(max(rank) * step + 6)
  dots <- paste(sprintf('<i class="metaui-track-dot" style="%s;bottom:%.1fpx"></i>', metaUI_track_style(scale$at(bins)), (rank - 1) * step + 1), collapse = "")
  paste0('<div class="metaui-strip" role="img" aria-label="Dot histogram of every effect size in the dataset">',
    sprintf('<div class="metaui-track metaui-stack" style="height:%dpx">%s%s</div>', height, metaUI_track_grid(scale), dots), metaUI_track_axis(scale, axis_title), "</div>")
}

# Reader table of model estimates: one row per model, every model named, with reasons
# where a model was not estimated. Downloads keep the full table.
metaUI_estimates_html <- function(table, first = integer(), axis_title = "Effect size", bound = NULL, studies = NA) {
  fmt <- function(x) if (is.na(x)) "" else metaUI_minus(formatC(x, format = "f", digits = 2))
  esc <- function(x) htmltools::htmlEscape(as.character(x))
  order <- c(first, setdiff(seq_len(nrow(table)), first))
  scale <- metaUI_track_scale(c(table$es, table$LCL, table$UCL, if (!is.null(bound)) c(-bound, bound)))
  types <- unique(stats::na.omit(table$interval_type[table$status == "ok"]))
  ref <- if (length(first) && table$status[first[1]] == "ok") table$es[first[1]] else NULL
  # Warnings are footnotes, so that one long message does not break the rows.
  warned <- order[nzchar(table$warnings[order])]
  rows <- vapply(order, function(i) {
    row <- table[i, ]
    ok <- row$status == "ok"
    label <- paste0(esc(row$Model), if (i %in% first) ' <span class="metaui-primary-tag">primary</span>', if (i %in% warned) paste0('<sup class="metaui-note-mark">', match(i, warned), "</sup>"),
      if (ok && length(types) > 1L) paste0('<span class="metaui-row-note">', esc(row$interval_type), "</span>"),
      paste0('<span class="metaui-level-inline">', if (i %in% first) "primary, ", if (row$aggregated) "study-level" else "effect-level", "</span>"))
    interval <- if (is.na(row$LCL) && is.na(row$UCL)) "" else paste0("[", fmt(row$LCL), ", ", fmt(row$UCL), "]")
    fit <- if (row$aggregated) "study-level" else "effect-level"
    unit <- if (is.na(row$k)) "" else if (row$aggregated || identical(as.numeric(row$k), as.numeric(studies))) "studies" else if ("input_rows" %in% names(row) && identical(as.numeric(row$k), as.numeric(row$input_rows))) "effects" else ""
    track <- if (ok) paste0('<td class="metaui-track-cell">', metaUI_track_row(scale, row$es, row$LCL, row$UCL, bound, ref = ref, diamond = i %in% first), "</td>")
      else paste0('<td class="metaui-reason-cell"><b>Not estimated (', esc(row$status), "):</b> ", esc(row$reason), "</td>")
    paste0('<tr', if (i %in% first) ' class="metaui-primary-row"', "><td>", label, '</td><td class="num">', fmt(row$es),
      '</td><td class="num">', interval, '</td><td class="num metaui-k">', if (is.na(row$k)) "" else row$k,
      '</td><td class="metaui-unit">', unit, '</td><td class="metaui-fit">', fit, "</td>", track, "</tr>")
  }, character(1))
  others <- if (length(first) && nrow(table) > 1L) paste0('<p class="metaui-note metaui-other-estimators">Rows under the primary analysis are other estimators for the same selection. They answer different questions and are not interchangeable with it.',
    if (!is.null(ref)) " Dashed line: the primary estimate.", "</p>",
    '<p class="metaui-note metaui-other-estimators"><i>k</i>: effects or studies the model reports',
    if (any(grepl("trim", table$Model, ignore.case = TRUE))) "; trim-and-fill includes imputed studies", ". Study-level fits first combine the effects within each study.</p>")
  interval_head <- if (length(types) == 1L && grepl("^Confidence", types)) "95% CI" else "95% interval"
  notes <- if (length(warned)) paste0('<ol class="metaui-warnings">', paste0("<li>", esc(table$Model[warned]), ": ", esc(table$warnings[warned]), "</li>", collapse = ""), "</ol>")
  paste0('<table class="table metaui-estimates metaui-custom"><thead><tr><th>Model<span class="metaui-level-inline">and level of fit</span></th><th class="num">Estimate</th><th class="num">',
    interval_head, '</th><th class="num metaui-k" title="Number of effects or studies the model reports; trim-and-fill includes imputed studies."><i>k</i></th><th></th><th class="metaui-fit" title="Effect-level fits use every selected effect; study-level fits first combine the effects within each study.">Level of fit</th><th class="metaui-track-cell">', metaUI_track_axis(scale, axis_title), "</th></tr></thead><tbody>",
    paste(rows, collapse = ""), '</tbody><tfoot class="metaui-phone-axis"><tr><td colspan="6"></td><td class="metaui-track-cell">', metaUI_track_axis(scale, axis_title), "</td></tr></tfoot></table>", others, notes)
}

# The forest plot as a table: the same drawn axis, with exact values beside each interval,
# effects grouped under their study, and marker area following precision.
# What tells a study's effects apart: the categorical filters that differ within it.
metaUI_effect_details <- function(df) {
  cols <- names(df)[startsWith(names(df), "metaUI__filter_") & vapply(df, is.factor, logical(1))]
  out <- character(nrow(df))
  for (group in split(seq_len(nrow(df)), as.character(df$metaUI__study_id))) for (col in cols) {
    values <- as.character(df[[col]][group])
    if (length(unique(values)) > 1L) out[group] <- paste0(out[group], ifelse(nzchar(out[group]), ", ", ""), ifelse(is.na(values), "(Missing)", values))
  }
  stats::setNames(out, paste(df$metaUI__study_id, df$metaUI__effect_id))
}

metaUI_forest_html <- function(rows, axis_title, studies = NULL, details = NULL) {
  fmt <- function(x) metaUI_minus(formatC(x, format = "f", digits = 2))
  esc <- function(x) htmltools::htmlEscape(as.character(x))
  scale <- metaUI_track_scale(c(rows$es, rows$LCL, rows$UCL))
  summaries <- rows[rows$kind == "Model summary", , drop = FALSE]
  effects <- rows[rows$kind == "Selected effect", , drop = FALSE]
  ref <- if (nrow(summaries)) summaries$es[1] else NULL
  width <- effects$UCL - effects$LCL
  size <- pmin(13, pmax(6, 7 * stats::median(width) / width))
  values <- function(row) paste0('<td class="num">', fmt(row$es), '</td><td class="num">[', fmt(row$LCL), ", ", fmt(row$UCL), "]</td>")
  grid <- paste0('<td class="metaui-track-cell">', metaUI_track_row(scale, NA_real_, NA_real_, NA_real_, ref = ref), "</td>")
  summary_rows <- if (nrow(summaries)) paste(vapply(seq_len(nrow(summaries)), function(i) paste0('<tr class="metaui-primary-row"><td>', esc(summaries$label[i]), "</td>", values(summaries[i, ]),
    '<td class="metaui-track-cell">', metaUI_track_row(scale, summaries$es[i], summaries$LCL[i], summaries$UCL[i], ref = ref, diamond = TRUE), "</td></tr>"), character(1)), collapse = "")
  groups <- rle(as.character(effects$study))
  ends <- cumsum(groups$lengths); starts <- ends - groups$lengths + 1L
  if (is.null(studies)) studies <- data.frame(study = character(), es = numeric(), LCL = numeric(), UCL = numeric())
  study_size <- pmin(13, pmax(6, 7 * stats::median(width) / (studies$UCL - studies$LCL)))
  study_rows <- vapply(seq_along(starts), function(s) {
    index <- starts[s]:ends[s]
    one <- function(i, label, class) paste0('<tr class="', class, '" data-es="', effects$es[i], '"><td>', label, "</td>", values(effects[i, ]),
      '<td class="metaui-track-cell">', metaUI_track_row(scale, effects$es[i], effects$LCL[i], effects$UCL[i], size = size[i], ref = ref), "</td></tr>")
    name <- groups$values[s]
    if (length(index) == 1L) return(one(index, esc(name), "metaui-forest-single"))
    labels <- substring(effects$label[index], nchar(name) + 3L)
    # Several effects: one combined row, which opens to show them (metaui.js).
    combined <- match(name, studies$study)
    head <- paste0('<button type="button" class="metaui-forest-toggle" aria-expanded="false">', esc(name), ' <span class="metaui-forest-count">', length(index), " effects</span></button>")
    paste0(if (is.na(combined)) paste0('<tr class="metaui-forest-study"><td colspan="3">', head, "</td>", grid, "</tr>")
      else paste0('<tr class="metaui-forest-study" data-es="', studies$es[combined], '"><td>', head, "</td>", values(studies[combined, ]), '<td class="metaui-track-cell">',
        metaUI_track_row(scale, studies$es[combined], studies$LCL[combined], studies$UCL[combined], size = study_size[combined], ref = ref, combined = TRUE), "</td></tr>"),
      paste(vapply(seq_along(index), function(j) {
        detail <- if (is.null(details)) "" else details[paste(effects$study[index[j]], effects$effect_id[index[j]])]
        one(index[j], paste0("Effect ", esc(labels[j]), if (!is.na(detail) && nzchar(detail)) paste0(' <span class="metaui-forest-count">', esc(detail), "</span>")), "metaui-forest-effect metaui-forest-closed")
      }, character(1)), collapse = ""))
  }, character(1))
  paste0('<p class="metaui-note metaui-key metaui-forest-key"><span><i class="metaui-glyph metaui-glyph-dot"></i>Observed effect, larger when more precise</span>',
    if (any(groups$lengths > 1L)) '<span><i class="metaui-glyph metaui-glyph-square"></i>A study\'s effects combined; open the row for them</span>',
    '<span><i class="metaui-glyph metaui-glyph-diamond"></i>Model summary</span>',
    if (!is.null(ref)) paste0('<span><i class="metaui-glyph metaui-glyph-dash"></i>', esc(summaries$label[1]), " estimate, ", fmt(ref), "</span>"), "</p>",
    '<p class="metaui-note metaui-key metaui-forest-controls">',
    if (any(groups$lengths > 1L)) '<button type="button" class="metaui-forest-all" aria-expanded="false">Show every effect</button>',
    '<button type="button" class="metaui-forest-sort" aria-pressed="false">Sort by estimate</button></p>',
    '<table class="table metaui-estimates metaui-forest metaui-custom"><thead><tr><th>Study</th><th class="num">Estimate</th><th class="num">95% CI</th><th class="metaui-track-cell">',
    metaUI_track_axis(scale, axis_title), "</th></tr></thead><tbody>", summary_rows, paste(study_rows, collapse = ""),
    '</tbody><tfoot><tr><td colspan="3"></td><td class="metaui-track-cell">', metaUI_track_axis(scale, axis_title), "</td></tr></tfoot></table>")
}

# Category counts with bars in the table, including levels the selection leaves empty.
metaUI_bars_html <- function(counts, name) {
  counts <- counts[order(as.character(counts[[name]]) == "(Missing)"), , drop = FALSE]
  levels <- as.character(counts[[name]])
  largest <- max(counts$Count, 1)
  rows <- sprintf('<tr%s><td>%s</td><td class="num">%s</td><td class="num">%s</td><td class="metaui-bar-cell" aria-hidden="true"><span class="metaui-bar" style="width:%s%%"></span></td></tr>',
    ifelse(counts$Count == 0, ' class="metaui-zero"', ifelse(levels == "(Missing)", ' class="metaui-missing"', "")), htmltools::htmlEscape(levels), counts$Count,
    sprintf("%.1f%%", 100 * counts$Count / max(sum(counts$Count), 1)), round(100 * counts$Count / largest, 1))
  paste0('<table class="table metaui-bars metaui-custom"><thead><tr><th><span class="sr-only">', htmltools::htmlEscape(name),
    '</span></th><th class="num">Effects</th><th class="num">Share</th><th class="metaui-bar-cell"></th></tr></thead><tbody>', paste(rows, collapse = ""), "</tbody></table>")
}

# Moderator levels on the same drawn axis; the dots are the selected effects themselves.
metaUI_moderator_html <- function(levels, estimate, lcl, ucl, k, effects, values, labels, axis_title, ref = NULL, se = NULL) {
  fmt <- function(x) metaUI_minus(formatC(x, format = "f", digits = 2))
  scale <- metaUI_track_scale(c(estimate, lcl, ucl, values))
  # Largest estimate first; a missing-data level stays at the end.
  order <- order(levels == "(Missing)", -estimate)
  # Dots follow precision, as in the numeric moderator's plot.
  sizes <- if (is.null(se)) NULL else pmin(8, pmax(3.5, 5 * stats::median(se, na.rm = TRUE) / se))
  rows <- vapply(order, function(i) {
    level <- as.character(effects) == as.character(levels[i])
    level[is.na(level)] <- FALSE
    paste0("<tr><td>", htmltools::htmlEscape(levels[i]), '</td><td class="num">', fmt(estimate[i]),
      '</td><td class="num">[', fmt(lcl[i]), ", ", fmt(ucl[i]), ']</td><td class="num metaui-k">', k[i], '</td><td class="metaui-track-cell">',
      metaUI_track_row(scale, estimate[i], lcl[i], ucl[i], dots = values[level], dot_labels = labels[level], ref = ref, dot_sizes = sizes[level], dot_shift = TRUE), "</td></tr>")
  }, character(1))
  paste0('<table class="table metaui-estimates metaui-moderators metaui-custom"><thead><tr><th>Level</th><th class="num">Estimate</th><th class="num">95% CI</th><th class="num metaui-k"><i>k</i></th><th class="metaui-track-cell">',
    metaUI_track_axis(scale, axis_title), "</th></tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table>")
}

# Duval-Tweedie L0, adapted from meta 8.5.0 trimfill.meta and estimate.missing
# (Guido Schwarzer et al., GPL >= 2). Common-effect trimming, average-rank ties,
# floor(L0 + .5), and return the current solution after 50 iterations.
# Estimation uses metafor; the rank/trim iteration stays explicit.
metaUI_trimfill <- function(df, control = list(), n.iter.max = 50L) {
  y <- df$metaUI__effect_size; se <- df$metaUI__se
  # Keep meta's initial ML fit so its warnings and failures are also reported.
  initial <- metaUI_univariate(y, se, control = control)
  keep <- !is.na(y) & !is.na(se)
  if (any(!keep)) warning(sum(!keep), " observation(s) dropped due to missing values")
  y <- y[keep]; se <- se[keep]; k <- length(y)
  if (k <= 2L) {
    warning("Minimal number of three studies for trim-and-fill method")
    return(NULL)
  }
  left <- sign(metaUI_egger(y, se)$estimate[1]) == 1
  if (!left) y <- -y
  ord <- order(y); y <- y[ord]; se <- se[ord]
  centre <- stats::weighted.mean(y, 1 / se^2)
  k0 <- 0L; last <- -1L; iteration <- 0L
  while (last != k0 && k0 <= k - 1L && iteration < n.iter.max) {
    iteration <- iteration + 1L; last <- k0
    sel <- seq_len(k - k0)
    centre <- stats::weighted.mean(y[sel], 1 / se[sel]^2)
    ranks <- rank(abs(y - centre)) * sign(y - centre)
    l0 <- (4 * sum(ranks[ranks > 0]) - k * (k + 1)) / (2 * k - 1)
    k0 <- floor(max(0, l0 + .5))
  }
  k0 <- min(k0, k - 1L)
  filled <- if (k0 > 0L) seq.int(k - k0 + 1L, k) else integer()
  yy <- c(y[order(ord)], 2 * centre - y[filled])
  ss <- c(se[order(ord)], se[filled])
  if (!left) yy <- -yy
  fit <- metaUI_univariate(yy, ss, control = control)
  fit$k0 <- k0; fit$left <- unname(left); fit$n.iter <- iteration
  fit
}

metaUI_multilevel_fit <- function(df) {
  metafor::rma.mv(df$metaUI__effect_size, V = df$metaUI__variance,
    random = ~ 1 | metaUI__study_id/metaUI__effect_id, data = df,
    test = "t", method = "REML", sparse = TRUE)
}

metaUI_rve_fit <- function(df) {
  robumeta::robu(metaUI__effect_size ~ 1, data = df,
    studynum = df$metaUI__study_id, var.eff.size = df$metaUI__variance, small = FALSE)
}

# One cache per server session. Exact keys include all data attributes and model
# code/configuration; no hash collisions, cross-user state, or unbounded history.
metaUI_fit_cache <- function(max_entries = 3L, limit = 3L, seed = NULL) {
  if (length(max_entries) != 1L || !is.numeric(max_entries) || !is.finite(max_entries) ||
      max_entries < 0 || max_entries > limit || max_entries != as.integer(max_entries))
    stop("fit_cache_entries must be an integer from 0 to ", limit, ".")
  entries <- if (is.null(seed)) list() else list(seed)
  cached <- function(df, models, correlation = .6, aggregation = "aggregate") {
    started <- proc.time()[["elapsed"]]
    key <- list(data = df, models = models, correlation = correlation, aggregation = aggregation)
    match <- which(vapply(entries, function(x) identical(x$key, key), logical(1)))
    hit <- length(match) > 0L
    if (hit) {
      entry <- entries[[match[1]]]
      entries <<- c(list(entry), entries[-match[1]])
      result <- entry$value
    } else {
      result <- metaUI_fit_models(df, models, correlation, aggregation)
      if (max_entries > 0L) entries <<- utils::head(c(list(list(key = key, value = result)), entries), max_entries)
    }
    result$table$cache_hit <- hit
    result$cache_hit <- hit
    result$calculation_seconds <- proc.time()[["elapsed"]] - started
    result
  }
  attr(cached, "clear") <- function() entries <<- list()
  cached
}

# Precomputed default analysis (opening_precompute() in the package). It answers for this
# app only while the files that determine it are unchanged; ui.R also sets the default
# filters, but the fit's cache key already checks those, so only the shown HTML depends on it.
metaUI_opening_files <- function(dir = ".") {
  files <- c(setdiff(list.files(dir, pattern = "[.]R$"), "ui.R"), "dataset.rds")
  unname(tools::md5sum(file.path(dir, files)))
}
metaUI_opening_load <- function(dir = ".") {
  path <- file.path(dir, "opening_results.rds")
  if (!file.exists(path)) return(NULL)
  saved <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(saved) || !identical(saved$files, metaUI_opening_files(dir))) return(NULL)
  if (!identical(saved$ui, unname(tools::md5sum(file.path(dir, "ui.R"))))) saved$html <- NULL
  saved
}

metaUI_reuse_fit <- function(results, models, code, df, fallback) {
  if (!identical(results$fit_data, df)) return(fallback())
  idx <- which(models$code == code & !models$aggregated)
  for (i in idx) if (!is.null(results$fits[[i]]) && results$table$status[i] == "ok" && !results$table$reflected[i]) return(results$fits[[i]])
  fallback()
}

metaUI_heterogeneity <- function(mod, df) {
  # Needs replication both between and within studies.
  identified <- length(unique(df$metaUI__study_id)) > 1L && anyDuplicated(df$metaUI__study_id) > 0L
  data.frame(study_variance = if (identified) mod$sigma2[1] else NA_real_,
    effect_variance = if (identified) mod$sigma2[2] else NA_real_,
    total_variance = sum(mod$sigma2), Q = mod$QE, Q_p = mod$QEp,
    components = if (identified) "Study and within-study effects" else if (anyDuplicated(df$metaUI__study_id)) "Split not identified: only one study" else "Split not identified: one effect per study")
}

metaUI_aggregate <- function(df, correlation = .6, method = "aggregate") {
  if (!is.numeric(correlation) || length(correlation) != 1L || !is.finite(correlation) ||
      correlation < 0 || correlation >= 1) stop("Aggregation correlation must be in [0, 1).")
  if (!method %in% c("aggregate", "first")) stop("Unknown aggregation method.")
  groups <- split(seq_len(nrow(df)), as.character(df$metaUI__study_id))
  dplyr::bind_rows(lapply(groups, function(idx) {
    x <- df[idx, , drop = FALSE]
    out <- x[1, , drop = FALSE]
    if (method == "aggregate") {
      vi <- x$metaUI__variance
      covariance <- tcrossprod(sqrt(vi)) * (correlation + diag(1 - correlation, length(vi)))
      inverse <- chol2inv(chol(covariance))
      out$metaUI__variance <- 1 / sum(inverse)
      out$metaUI__effect_size <- sum(inverse %*% x$metaUI__effect_size) / sum(inverse)
      out$metaUI__se <- sqrt(out$metaUI__variance)
      # Neither a p-value nor a total sample size can be inferred from effect aggregation.
      out$metaUI__pvalue <- NA_real_
      out$metaUI__N <- NA_real_
    }
    out
  }))
}

# Identify default estimators by their complete code, so renaming a model keeps its
# input checks and sign handling. Edited or unrecognised code returns `fallback`.
metaUI_model_role <- function(spec, fallback = spec$name) {
  normalise <- function(x) gsub("\\s+", "", x)
  defaults <- c("Random-Effects Multilevel Model" = metaUI_code_multilevel,
                "Robust Variance Estimation" = metaUI_code_rve, metaUI_default_code)
  hit <- names(defaults)[normalise(defaults) == normalise(spec$code)]
  if (length(hit)) hit[1] else fallback
}

metaUI_model_reason <- function(spec, df) {
  if (!nrow(df)) return("No eligible rows")
  role <- metaUI_model_role(spec)
  if (any(!is.finite(df$metaUI__effect_size)) || any(!is.finite(df$metaUI__variance) | df$metaUI__variance <= 0))
    return("Invalid required effect/variance inputs")
  if (role %in% c("P-uniform star", "Hedges-Vevea Selection Model") &&
      !df$metaUI__direction[1] %in% c("positive", "negative"))
    return("Requires explicit positive or negative effect direction")
  if (role %in% c("P-Curve (first value)", "P-Curve (last value)")) {
    if (df$metaUI__es_type[1] != "SMD") return("P-curve effect estimation only supported for SMD")
    selection <- if (role == "P-Curve (last value)") "last" else "first"
    issue <- tryCatch({metaUI_pcurve_data(df, selection, require_N = TRUE); NULL}, error = function(e) conditionMessage(e))
    if (!is.null(issue)) return(issue)
  }
  if (role %in% c("Precision Effect Test", "Precision Effect Estimate using Standard Error")) {
    predictor <- if (role == "Precision Effect Test") sqrt(df$metaUI__variance) else df$metaUI__variance
    if (length(unique(predictor)) < 2 || nrow(df) <= 2) return("Regression requires variable precision and positive residual degrees of freedom")
  }
  NULL
}

metaUI_fit_models <- function(df, models, correlation = .6, aggregation = "aggregate") {
  metaUI_validate_prepared(df)
  if (!nrow(df)) stop("No eligible rows remain after filtering.")
  df_agg <- metaUI_aggregate(df, correlation, aggregation)
  available_rows <- attr(df, "metaUI_runtime_rows")
  validation <- attr(df, "metaUI_validation")
  if (is.null(available_rows)) available_rows <- if (is.null(validation)) nrow(df) else validation$retained_rows
  preparation_excluded <- if (is.null(validation)) NA_integer_ else nrow(validation$exclusions)
  fits <- vector("list", nrow(models))
  rows <- lapply(seq_len(nrow(models)), function(i) {
    spec <- models[i, , drop = FALSE]
    x <- if (isTRUE(spec$aggregated)) df_agg else df
    reason <- metaUI_model_reason(spec, x)
    bayesian <- grepl("^metaUI_bayesian_fit\\(", spec$code)
    if (bayesian && nrow(x) < 2L) reason <- "Bayesian analysis requires at least two studies"
    warnings <- character()
    elapsed <- 0
    result <- c(es = NA_real_, LCL = NA_real_, UCL = NA_real_, k = NA_real_)
    status <- "unsupported"
    reflected <- FALSE
    if (is.null(reason)) {
      started <- proc.time()[["elapsed"]]
      reason <- tryCatch({
        env <- new.env(parent = environment(metaUI_fit_models))
        env$df <- x
        # Reflect the inputs only for explicitly directional, right-sided models.
        reflected <- metaUI_model_role(spec) %in% c("P-uniform star", "Hedges-Vevea Selection Model") && x$metaUI__direction[1] == "negative"
        if (reflected) env$df$metaUI__effect_size <- -env$df$metaUI__effect_size
        env$mod <- withCallingHandlers(eval(parse(text = spec$code), env), warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
        })
        candidate <- vapply(c("es", "LCL", "UCL", "k"), function(field) {
          value <- if (is.na(spec[[field]])) NA_real_ else as.numeric(eval(parse(text = spec[[field]]), env))
          if (length(value) != 1L) stop("Model extraction must return one value per field")
          value
        }, numeric(1))
        if (any(!is.finite(candidate[c("es", "k")])) || any(is.infinite(candidate[c("LCL", "UCL")])) ) stop("Nonfinite estimate, interval, or count")
        for (field in c("LCL", "UCL")) if (is.na(candidate[field]) && !is.na(spec[[field]]) && spec[[field]] != "NA") stop("Missing model interval")
        result <- candidate
        if (reflected) result[c("es", "LCL", "UCL")] <- c(-result["es"], -result["UCL"], -result["LCL"])
        status <- "ok"
        if (bayesian || (!spec$aggregated && !reflected && spec$code %in% c(metaUI_code_multilevel, metaUI_code_rve))) fits[[i]] <<- env$mod
        ""
      }, error = function(e) { status <<- "failed"; conditionMessage(e) })
      elapsed <- proc.time()[["elapsed"]] - started
    }
    row <- data.frame(Model = spec$name, es = result["es"], LCL = result["LCL"], UCL = result["UCL"],
                      k = result["k"], status = status, reason = reason,
                      warnings = paste(unique(warnings), collapse = "; "),
                      filtered_rows = max(0L, available_rows - nrow(df)), preparation_excluded_rows = preparation_excluded,
                      input_rows = nrow(df), analysis_rows = nrow(x),
                      collapsed_rows = nrow(df) - nrow(x), aggregated = spec$aggregated,
                      fit_seconds = elapsed, reflected = reflected, row.names = NULL)
    row$interval_type <- if (bayesian) "95% central credible interval (prior-dependent)" else
      if (is.na(metaUI_model_role(spec, NA_character_))) "Author-defined interval (type and coverage unknown)" else
      "Confidence interval (coverage model-dependent)"
    row$fit_es <- row$es; row$fit_LCL <- row$LCL; row$fit_UCL <- row$UCL
    if (df$metaUI__es_type[1] == "ZCOR") row[c("es", "LCL", "UCL")] <- lapply(row[c("es", "LCL", "UCL")], tanh)
    row
  })
  list(df_agg = df_agg, table = dplyr::bind_rows(rows), fits = fits, fit_data = df)
}

metaUI_validate_prepared <- function(df) {
  required <- c("metaUI__study_id", "metaUI__effect_id", "metaUI__effect_size", "metaUI__variance",
                "metaUI__se", "metaUI__es_z", "metaUI__es_type", "metaUI__display_scale", "metaUI__direction")
  if (!all(required %in% names(df))) stop("Prepared data columns are missing; run prepare_data() first.")
  if (!nrow(df)) return(invisible(TRUE))
  for (field in c("metaUI__effect_size", "metaUI__variance", "metaUI__se"))
    if (!is.numeric(df[[field]]) || any(!is.finite(df[[field]]))) stop("Invalid prepared numeric inputs: ", field)
  if (any(df$metaUI__variance <= 0 | df$metaUI__se <= 0) ||
      any(abs(df$metaUI__se^2 - df$metaUI__variance) > 1e-6 * pmax(abs(df$metaUI__variance), df$metaUI__se^2))) stop("Prepared SE/variance contract failed.")
  if (anyNA(df$metaUI__study_id) || anyNA(df$metaUI__effect_id) ||
      any(trimws(as.character(df$metaUI__study_id)) == "") || any(trimws(as.character(df$metaUI__effect_id)) == "") ||
      anyDuplicated(df[c("metaUI__study_id", "metaUI__effect_id")])) stop("Prepared IDs must be nonmissing and unique within study.")
  for (field in c("metaUI__es_type", "metaUI__display_scale", "metaUI__direction"))
    if (anyNA(df[[field]]) || length(unique(df[[field]])) != 1L) stop("Mixed or missing prepared scale/direction metadata.")
  metric <- df$metaUI__es_type[1]
  if (!metric %in% c("SMD", "ZCOR") || df$metaUI__display_scale[1] != if (metric == "ZCOR") "r" else "SMD") stop("Prepared scale contract failed.")
  if (!df$metaUI__direction[1] %in% c("unspecified", "positive", "negative")) stop("Invalid prepared direction.")
  invisible(TRUE)
}

metaUI_validate_upload <- function(df, built) {
  metaUI_validate_prepared(df)
  if (!nrow(df) || !all(names(built) %in% names(df))) stop("Uploaded data need all built app columns and at least one effect.")
  for (field in c("metaUI__es_type", "metaUI__display_scale", "metaUI__direction"))
    if (!identical(as.character(df[[field]][1]), as.character(built[[field]][1]))) stop("Uploaded scale/direction differs from the built app; build a fresh app for a new contract.")
  source_y <- df$metaUI__input_effect; source_v <- df$metaUI__input_variance
  if (!is.numeric(source_y) || !is.numeric(source_v) || any(!is.finite(source_y)) || any(!is.finite(source_v) | source_v <= 0)) stop("Invalid uploaded source effect/variance fields.")
  input_scale <- attr(built, "metaUI_validation")$input_scale
  expected_y <- source_y; expected_v <- source_v
  if (identical(input_scale, "COR")) {
    if (any(abs(source_y) >= 1)) stop("Uploaded COR source effects must be in (-1,1).")
    expected_y <- atanh(source_y); expected_v <- source_v / (1 - source_y^2)^2
  }
  if (any(abs(df$metaUI__effect_size - expected_y) > 1e-8 * pmax(1, abs(expected_y))) ||
      any(abs(df$metaUI__variance - expected_v) > 1e-6 * pmax(abs(expected_v), df$metaUI__variance)))
    stop("Uploaded source/fitting-scale fields disagree. Re-prepare original input with the declared transformation.")
  specs <- metaUI_computation_specs(built)
  metaUI_validate_alternatives(df, specs)
  if (length(specs)) {
    attr(df, "metaUI_alternatives") <- specs
    attr(df, "metaUI_input_scale") <- input_scale
    label <- attr(built, "metaUI_primary_label")
    attr(df, "metaUI_primary_label") <- if (is.null(label)) attr(built, "metaUI_validation")$primary_label else label
  }
  centre <- mean(built$metaUI__effect_size); spread <- stats::sd(built$metaUI__effect_size)
  degenerate <- !is.finite(spread) || spread == 0
  z <- if (degenerate) rep(0, nrow(df)) else (df$metaUI__effect_size - centre) / spread
  supplied <- df$metaUI__es_z
  disagreements <- if (!is.numeric(supplied)) nrow(df) else sum(!is.finite(supplied) | abs(supplied - z) > 1e-8 * pmax(1, abs(z)))
  df$metaUI__es_z <- z
  attr(df, "metaUI_upload_derivations") <- list(z_reference_mean = centre, z_reference_sd = spread,
    supplied_z_disagreements = disagreements, rule = if (degenerate) "Built SD unavailable/zero: descriptive z=0, outlier discrimination unavailable" else "Descriptive z=(fitting effect-built mean)/built SD; saved scale preserved")
  df
}

metaUI_pcurve_data <- function(df, selection = "first", require_N = FALSE) {
  metaUI_validate_prepared(df)
  if (!nrow(df)) stop("No eligible rows remain for p-curve.")
  if (df$metaUI__es_type[1] != "SMD") stop("P-curve is supported only for SMD.")
  direction <- df$metaUI__direction[1]
  if (!direction %in% c("positive", "negative")) stop("P-curve requires explicit effect direction; it is not inferred.")
  if (!selection %in% c("first", "last")) stop("Unknown p-curve selection rule.")
  groups <- split(seq_len(nrow(df)), as.character(df$metaUI__study_id))
  idx <- vapply(groups, function(x) if (selection == "first") x[1] else utils::tail(x, 1), integer(1))
  x <- df[idx, , drop = FALSE]
  if (direction == "negative") x$metaUI__effect_size <- -x$metaUI__effect_size
  excluded <- x$metaUI__effect_size <= 0
  x <- x[!excluded, , drop = FALSE]
  if (!nrow(x)) stop("No direction-consistent first/last effects remain for p-curve.")
  if (require_N && any(!is.finite(x$metaUI__N) | x$metaUI__N <= 2)) stop("P-curve effect estimation requires valid N > 2 for selected direction-consistent effects.")
  out <- data.frame(studlab = x$metaUI__study_id, TE = x$metaUI__effect_size,
                    seTE = x$metaUI__se, n = x$metaUI__N)
  attr(out, "direction_exclusions") <- sum(excluded)
  attr(out, "selection_exclusions") <- nrow(df) - length(idx)
  out
}

metaUI_pcurve_fit <- function(df, selection = "first") {
  x <- metaUI_pcurve_data(df, selection, require_N = TRUE)
  mod <- pcurve(x, effect.estimation = TRUE, N = x$n, dmin = 0, dmax = max(x$TE))
  # The refinement step in pcurve() can leave the grid range, so dmin = 0 does not bound the result.
  if (mod$dEstimate < 0) stop("P-curve effect estimate fell below zero, outside the searched range; no estimate is reported.")
  # The fit is on the magnitude scale; its sign comes only from the declared direction.
  if (df$metaUI__direction[1] == "negative") mod$dEstimate <- -mod$dEstimate
  mod
}

# Forest rows retain fitting-scale values and add the reader-facing scale.
# Effect intervals are normal sampling intervals, not shrunk predictions.
# Summaries are the unaggregated default multilevel/RVE fits, found by their code
# (`models` rows align with `estimates`) so renamed models are kept.
# Study-level effects (as used by the study-level models), on the forest plot's display scale.
metaUI_forest_studies <- function(aggregated) {
  se <- aggregated$metaUI__se
  out <- data.frame(study = as.character(aggregated$metaUI__study_id), es = aggregated$metaUI__effect_size,
    LCL = aggregated$metaUI__effect_size - stats::qnorm(.975) * se, UCL = aggregated$metaUI__effect_size + stats::qnorm(.975) * se)
  if (identical(aggregated$metaUI__display_scale[1], "r")) out[c("es", "LCL", "UCL")] <- lapply(out[c("es", "LCL", "UCL")], tanh)
  out
}

metaUI_forest_rows <- function(df, estimates, models = NULL) {
  df <- df[order(as.character(df$metaUI__study_id), suppressWarnings(as.numeric(as.character(df$metaUI__effect_id))), as.character(df$metaUI__effect_id)), , drop = FALSE]
  label <- if ("metaUI__es_label" %in% names(df)) df$metaUI__es_label else df$metaUI__effect_id
  rows <- data.frame(label = paste(df$metaUI__study_id, label, sep = ": "),
    kind = "Selected effect", study = as.character(df$metaUI__study_id),
    effect_id = as.character(df$metaUI__effect_id), fit_es = df$metaUI__effect_size,
    fit_LCL = df$metaUI__effect_size - stats::qnorm(.975) * df$metaUI__se,
    fit_UCL = df$metaUI__effect_size + stats::qnorm(.975) * df$metaUI__se)
  rows$es <- rows$fit_es; rows$LCL <- rows$fit_LCL; rows$UCL <- rows$fit_UCL
  if (df$metaUI__display_scale[1] == "r")
    rows[c("es", "LCL", "UCL")] <- lapply(rows[c("es", "LCL", "UCL")], tanh)
  summary_rows <- if (!is.null(models) && nrow(models) == nrow(estimates))
    models$code %in% c(metaUI_code_multilevel, metaUI_code_rve) & !models$aggregated else
    estimates$Model %in% c("Random-Effects Multilevel Model", "Robust Variance Estimation") & !estimates$aggregated
  summaries <- estimates[summary_rows & estimates$status == "ok", ]
  if (nrow(summaries)) rows <- rbind(rows, data.frame(label = summaries$Model,
    kind = "Model summary", study = NA_character_, effect_id = NA_character_,
    fit_es = summaries$fit_es, fit_LCL = summaries$fit_LCL, fit_UCL = summaries$fit_UCL,
    es = summaries$es, LCL = summaries$LCL, UCL = summaries$UCL))
  rows$row <- rev(seq_len(nrow(rows)))
  rows
}

# label_width (characters) wraps study labels onto at most two lines for narrow (phone)
# plots. Longer labels lose their middle, so the end (year, effect) still tells rows apart.
# top_axis repeats the x axis above plots taller than a screen.
metaUI_forest_plot <- function(rows, scale, colours = metaUI_plot_colours(), label_width = NULL, top_axis = FALSE, family = colours$family) {
  if (!is.null(label_width)) rows$label <- vapply(rows$label, function(label) {
    lines <- strwrap(label, label_width)
    if (length(lines) > 2L) {
      rest <- paste(lines[-1], collapse = " ")
      tail <- substring(rest, nchar(rest) - label_width + 2L)
      lines <- c(lines[1], paste0("\u2026", if (grepl(" ", tail)) sub("^\\S*\\s+", "", tail) else tail))
    }
    paste(lines, collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
  # Bands group the effects of one study; model summaries stand apart in the accent colour.
  effects <- rows[rows$kind == "Selected effect", , drop = FALSE]
  studies <- rle(as.character(if (is.null(effects$study)) effects$label else effects$study))
  ends <- cumsum(studies$lengths); starts <- ends - studies$lengths + 1L
  shaded <- seq_along(starts) %% 2L == 0L
  bands <- data.frame(ymin = effects$row[ends[shaded]] - .5, ymax = effects$row[starts[shaded]] + .5)
  summaries <- rows[rows$kind == "Model summary", , drop = FALSE]
  ggplot2::ggplot(rows, ggplot2::aes(x = .data$es, y = .data$row)) +
    ggplot2::geom_rect(data = bands, ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = .data$ymin, ymax = .data$ymax), inherit.aes = FALSE, fill = colours$shade, colour = NA) +
    ggplot2::geom_vline(xintercept = 0, colour = colours$slate, linewidth = .4) +
    {if (nrow(summaries)) ggplot2::geom_hline(yintercept = max(summaries$row) + .5, colour = colours$ink, linewidth = .4)} +
    ggplot2::geom_segment(ggplot2::aes(x = .data$LCL, xend = .data$UCL, yend = .data$row, colour = .data$kind, linewidth = .data$kind)) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$kind, size = .data$kind, colour = .data$kind, fill = .data$kind)) +
    ggplot2::scale_shape_manual(values = c("Selected effect" = 16, "Model summary" = 23), guide = "none") +
    ggplot2::scale_size_manual(values = c("Selected effect" = 1.9, "Model summary" = 3.4), guide = "none") +
    ggplot2::scale_linewidth_manual(values = c("Selected effect" = .4, "Model summary" = .8), guide = "none") +
    ggplot2::scale_colour_manual(values = c("Selected effect" = colours$ink, "Model summary" = colours$accent), guide = "none") +
    ggplot2::scale_fill_manual(values = c("Selected effect" = colours$ink, "Model summary" = colours$accent), guide = "none") +
    ggplot2::scale_y_continuous(breaks = rows$row, labels = rows$label, expand = ggplot2::expansion(add = .8)) +
    ggplot2::scale_x_continuous(sec.axis = if (top_axis) ggplot2::dup_axis() else ggplot2::waiver(),
      # The PDF device's default font has no true minus sign.
      labels = function(x) { x <- format(x, trim = TRUE, drop0trailing = TRUE); if (nzchar(family)) metaUI_minus(x) else x }) +
    ggplot2::labs(x = paste("Effect size (", scale, ")", sep = ""), y = NULL) +
    metaUI_plot_theme(colours, family) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(colour = colours$ink, hjust = 0, size = if (is.null(label_width)) 11.5 else 10.5, lineheight = .9),
      panel.grid.major.y = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())
}

# Descriptive containment of existing intervals, without refitting or assuming
# custom-model coverage. Equality at a bound is deliberately inconclusive.
metaUI_interval_assessment <- function(estimates, bound = NULL, scale = "SMD", known_models = character()) {
  if (!is.null(bound) && (length(bound) != 1L || !is.numeric(bound) || !is.finite(bound) ||
      bound <= 0 || (scale == "r" && bound >= 1)))
    stop(if (scale == "r") "Enter a bound strictly between 0 and 1 for r." else "Enter a positive finite bound.")
  valid <- estimates$status == "ok" & is.finite(estimates$LCL) & is.finite(estimates$UCL) & estimates$LCL <= estimates$UCL
  threshold <- rep(NA_real_, nrow(estimates))
  threshold[valid] <- pmax(abs(estimates$LCL[valid]), abs(estimates$UCL[valid]))
  assessment <- rep("Not assessed", nrow(estimates))
  assessment[valid] <- "No bound chosen"
  if (!is.null(bound)) {
    assessment[valid] <- "Inconclusive"
    assessment[valid & estimates$LCL > -bound & estimates$UCL < bound] <- "Interval within bounds"
    assessment[valid & estimates$LCL > bound] <- "Exceeds positive bound"
    assessment[valid & estimates$UCL < -bound] <- "Exceeds negative bound"
  }
  bayesian <- if ("interval_type" %in% names(estimates)) grepl("credible", estimates$interval_type) else rep(FALSE, nrow(estimates))
  data.frame(Model = estimates$Model, bound = rep(if (is.null(bound)) NA_real_ else bound, nrow(estimates)),
    scale = rep(scale, nrow(estimates)), assessment = assessment, threshold = threshold,
    interval = ifelse(bayesian, "95% central credible interval (prior-dependent)", ifelse(estimates$Model %in% known_models, "Reported 95% CI", "Author-defined interval; error rate unknown")))
}
