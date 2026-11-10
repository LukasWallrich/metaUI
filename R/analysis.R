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

# The first spot whose text box holds no obstacle; failing that, the spot with the fewest.
metaUI_label_spot <- function(spots, width, height, obstacles, pad = 0) {
  inside <- vapply(seq_len(nrow(spots)), function(i) {
    left <- spots$x[i] - spots$hjust[i] * width
    sum(obstacles$x >= left - pad & obstacles$x <= left + width + pad & abs(obstacles$y - spots$y[i]) <= height / 2)
  }, numeric(1))
  spots[which.min(inside), , drop = FALSE]
}

# Correlations are fitted as Fisher z; readers see r. Slopes and variances have no r equivalent.
metaUI_show <- function(x, df) if (identical(as.character(df$metaUI__es_type[1]), "ZCOR")) tanh(x) else x

# Two decimals, or three where an interval is too narrow for two to show it.
metaUI_digits <- function(lcl, ucl) {
  width <- ucl - lcl
  if (any(is.finite(width) & width < .05)) 3L else 2L
}

# width: the plot's width on screen in pixels, which sets how much of the axis a label covers.
# back: draw on the fitting scale, where the limits are straight, with the axis labelled in r.
metaUI_funnel <- function(fit, xlab, colours = metaUI_plot_colours(), width = NULL, back = FALSE) {
  if (is.null(width)) width <- 700
  # On phones every other tick, counted from zero, so that their labels keep apart.
  thin <- function(ticks, range) {
    if (width >= 560 || sum(ticks >= range[1] & ticks <= range[2]) <= 6L) return(ticks)
    ticks[round(ticks / diff(ticks)[1]) %% 2 == 0]
  }
  centre <- as.numeric(fit$b)
  se <- sqrt(fit$vi); yi <- as.numeric(fit$yi)
  z <- stats::qnorm(.975)
  ymax <- max(se) * 1.05; half <- z * ymax
  limits <- range(yi, centre - half, centre + half, -half, half)
  limits <- limits + c(-1, 1) * .04 * diff(limits)
  region <- data.frame(x = c(limits[1], limits[1], -half, 0, limits[2], limits[2], half, 0),
    y = c(0, ymax, ymax, 0, 0, ymax, ymax, 0), side = rep(1:2, each = 4))
  # Labels go where the data and lines leave room, for any selection.
  along <- seq(0, ymax, length.out = 80)
  obstacles <- data.frame(x = c(yi, rep(centre, 80), centre - z * along, centre + z * along), y = c(se, along, along, along))
  # A point's radius and a little air, so that no dot sits against the text.
  pad <- 6 / max(width - 70, 150) * diff(limits)
  gap <- 1.2 * pad
  text_height <- .075 * ymax
  text_width <- function(chars) chars * 7 / max(width - 70, 150) * diff(limits)
  place <- function(spots, chars) {
    label_width <- text_width(chars)
    left <- spots$x - spots$hjust * label_width
    spots <- spots[left >= limits[1] & left + label_width <= limits[2], , drop = FALSE]
    if (!nrow(spots)) return(NULL)
    spot <- metaUI_label_spot(spots, label_width, text_height, obstacles, pad)
    left <- spot$x - spot$hjust * label_width
    obstacles <<- rbind(obstacles, expand.grid(x = seq(left, left + label_width, length.out = 8), y = spot$y + c(-1, 0, 1) * text_height / 2))
    spot
  }
  label <- function(spot, text, colour, parse = FALSE) {
    if (is.null(spot)) return(NULL)
    ggplot2::annotate("text", x = spot$x, y = spot$y, label = text, hjust = spot$hjust, parse = parse, size = 4.9, colour = colour, family = colours$family)
  }
  # Beside a limit line, the text clears the line at the box's lower edge, where the line is furthest out.
  beside <- function(y) c(centre + z * (y + text_height / 2) + gap, centre - z * (y + text_height / 2) - gap)
  y_pooled <- text_height / 2 + .005 * ymax
  pooled_text <- paste0("pooled ", metaUI_minus(formatC(if (back) tanh(centre) else centre, format = "f", digits = metaUI_digits(fit$ci.lb, fit$ci.ub))))
  # Near the apex if there is room there, else lower down the centre line, where large datasets thin out.
  down <- seq(.3, .95, by = .13) * ymax
  pooled <- place(rbind(data.frame(x = beside(y_pooled), y = y_pooled, hjust = c(0, 1)),
    data.frame(x = rep(centre + c(1, -1) * gap, length(down)), y = rep(down, each = 2), hjust = rep(c(0, 1), length(down)))), nchar(pooled_text))
  # p < .05 in a corner of a grey region, p >= .05 in the white funnel, each wholly inside.
  corner_y <- c(.05, .05, .95, .95) * ymax
  corner <- data.frame(x = c(limits[1] + gap, limits[2] - gap, limits[1] + gap, limits[2] - gap), y = corner_y, hjust = c(0, 1, 0, 1))
  corner <- corner[abs(corner$x) - text_width(6) > z * (corner$y + text_height / 2), , drop = FALSE]
  below <- place(corner, 6)
  inner <- expand.grid(y = c(.95, .86, .77) * ymax, at = c(0, -.45, .45, -.7, .7))
  inner$x <- inner$at * z * inner$y
  inner$hjust <- .5
  inner <- inner[abs(inner$x) + text_width(6) / 2 < z * (inner$y - text_height / 2), c("x", "y", "hjust"), drop = FALSE]
  above <- place(inner, 6)
  heights <- seq(.92, .2, by = -.06) * ymax
  limit <- place(data.frame(x = c(rbind(beside(heights)[seq_along(heights)], beside(heights)[length(heights) + seq_along(heights)])),
    y = rep(heights, each = 2), hjust = rep(c(0, 1), length(heights))), 10)
  ggplot2::ggplot() +
    ggplot2::geom_polygon(data = region, ggplot2::aes(.data$x, .data$y, group = .data$side), fill = colours$shade) +
    ggplot2::annotate("segment", x = centre, xend = centre, y = 0, yend = ymax, linetype = 2, linewidth = .4, colour = colours$accent) +
    ggplot2::annotate("path", x = centre + c(-half, 0, half), y = c(ymax, 0, ymax), linetype = 2, linewidth = .4, colour = colours$accent) +
    ggplot2::geom_point(data = data.frame(yi = yi, se = se), ggplot2::aes(.data$yi, .data$se), colour = colours$slate, alpha = .6, size = 2) +
    label(below, 'italic(p) < ".05"', colours$slate, parse = TRUE) +
    label(limit, "95% limits", colours$accent) +
    label(above, 'italic(p) >= ".05"', colours$slate, parse = TRUE) +
    label(pooled, pooled_text, colours$accent) +
    ggplot2::scale_y_reverse(breaks = metaUI_funnel_ticks(se), expand = c(0, 0)) +
    (if (back) {
      ticks <- thin(pretty(tanh(limits), 8), tanh(limits)); ticks <- ticks[abs(ticks) < 1]
      ggplot2::scale_x_continuous(limits = limits, expand = c(0, 0), breaks = atanh(ticks), labels = metaUI_minus(format(ticks, trim = TRUE)))
    } else ggplot2::scale_x_continuous(limits = limits, expand = c(0, 0), breaks = thin(pretty(limits, 8), limits), labels = function(x) metaUI_minus(format(x, trim = TRUE)))) +
    ggplot2::labs(x = metaUI_label_expr(xlab), y = if (back) "Standard error (Fisher z)" else "Standard error") +
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
# A value that rounds to zero loses its sign: "-0.000" says nothing "0.000" does not.
metaUI_minus <- function(x) gsub("-", "\u2212", sub("^-(0(\\.0+)?)$", "\\1", x), fixed = TRUE)

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
metaUI_zcurve_plot <- function(fit, colours, narrow = FALSE) {
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
  # The threshold's labels sit above the panel, clear of any bar; on phones the statistic takes a line of its own.
  stat <- sprintf('italic(z) > "%s" * "," ~ italic(p) < "%s"', round(threshold, 2), sub("^0", "", format(fit$control$sig_level)))
  top <- if (narrow) -1.7 else -.4
  plot <- plot + ggplot2::annotate("text", x = threshold + .12, y = Inf, vjust = top, hjust = 0, size = 4.9, colour = colours$accent, family = colours$family, parse = TRUE,
      label = if (narrow) '"significant"' else paste('"significant," ~', stat)) +
    (if (narrow) ggplot2::annotate("text", x = threshold + .12, y = Inf, vjust = -.4, hjust = 0, size = 4.9, colour = colours$accent, family = colours$family, parse = TRUE, label = stat)) +
    ggplot2::annotate("text", x = threshold - .12, y = Inf, vjust = top, hjust = 1, size = 4.9, colour = colours$slate, family = colours$family, parse = TRUE, label = '"not significant"') +
    metaUI_plot_theme(colours)
  # The curve's name sits beyond its end, in the right margin, clear of the data whatever
  # the selection; tests beyond the plot's end are counted beneath it.
  right <- max(ggplot2::layer_scales(plot)$x$get_limits())
  line <- plot$layers[[which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1)))[1]]]$data
  end <- line[which.max(line$x), ]
  beyond <- sum(fit$data > right)
  plot <- plot +
    ggplot2::annotate("text", x = Inf, y = end$y, hjust = -.05, size = 4.9, colour = colours$ink, family = colours$family, parse = TRUE, label = 'italic(z) * "-curve fit"') +
    ggplot2::theme(plot.margin = ggplot2::margin(if (narrow) 46 else 26, 66, 4, 4))
  plot$coordinates$clip <- "off"
  x_scale <- plot$scales$get_scales("x")
  # The axis title counts the tests beyond the plot's end, where no tick label can collide with it.
  note <- if (beyond > 0L) paste0(" (", beyond, if (beyond == 1L) " test" else " tests", " beyond ", format(right), " not shown)") else ""
  if (!is.null(x_scale)) x_scale$name <- bquote(italic(z) * "-score" * .(note))
  y_scale <- plot$scales$get_scales("y")
  if (!is.null(y_scale)) {
    # The package fixes its own limits and breaks; end the axis on a labelled tick above every bar.
    heights <- unlist(lapply(plot$layers, function(layer) {
      if (inherits(layer$geom, "GeomRect")) layer$data$ymax else if (inherits(layer$geom, "GeomLine")) layer$data$y
    }))
    ticks <- pretty(c(0, max(heights, na.rm = TRUE)))
    y_scale$limits <- range(ticks)
    y_scale$breaks <- ticks
    y_scale$expand <- ggplot2::expansion(mult = 0)
  }
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
  text <- function(...) ggplot2::annotate("text", size = 4.9, family = colours$family, ...)
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
    ggplot2::annotate("text", x = .03, y = -Inf, vjust = 3.7, hjust = 1, label = "p", fontface = "italic", size = 5.1, colour = colours$ink, family = colours$family) +
    ggplot2::annotate("text", x = .03, y = -Inf, vjust = 3.7, hjust = 0, label = "-value", size = 5.1, colour = colours$ink, family = colours$family) +
    ggplot2::coord_cartesian(xlim = c(.0095, .0505), ylim = c(0, top), expand = FALSE, clip = "off") +
    metaUI_plot_theme(colours) +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 92, 28, 4))
}

metaUI_plot_theme <- function(colours, family = colours$family) {
  ggplot2::theme_minimal(base_size = 13, base_family = family) +
    ggplot2::theme(text = ggplot2::element_text(colour = colours$ink),
      axis.text = ggplot2::element_text(colour = colours$slate, size = 14),
      axis.title = ggplot2::element_text(colour = colours$ink, size = 14.5),
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

# hollow: an outlined diamond, for an interval that includes zero.
metaUI_track_row <- function(scale, es, lcl, ucl, band = NULL, dots = NULL, dot_labels = NULL, size = NULL, ref = NULL, diamond = FALSE, combined = FALSE, dot_sizes = NULL, hollow = FALSE) {
  has_interval <- is.finite(lcl) && is.finite(ucl)
  # A diamond always spans the interval, with its tallest point at the estimate.
  spans <- diamond && is.finite(es) && has_interval && ucl > lcl
  paste0('<div class="metaui-track" aria-hidden="true">', metaUI_track_grid(scale, band),
    if (!is.null(ref) && is.finite(ref)) sprintf('<i class="metaui-track-ref" style="%s"></i>', metaUI_track_style(scale$at(ref))),
    # metaui.js spreads these beneath the interval once the row's width is known.
    if (length(dots)) paste(sprintf('<i class="metaui-track-dot" style="%s%s"%s></i>', metaUI_track_style(scale$at(dots)),
      if (length(dot_sizes)) sprintf(";width:%1$.1fpx;height:%1$.1fpx;margin:%2$.2fpx 0 0 %2$.2fpx", dot_sizes, -dot_sizes / 2) else "",
      if (length(dot_labels)) sprintf(' title="%s"', htmltools::htmlEscape(dot_labels, attribute = TRUE)) else ""), collapse = ""),
    if (spans && hollow) sprintf('<svg class="metaui-track-diamond metaui-track-hollow" style="%1$s" viewBox="0 0 100 10" preserveAspectRatio="none" aria-hidden="true"><polygon points="0.8,5 %2$.1f,0.8 99.2,5 %2$.1f,9.2" vector-effect="non-scaling-stroke"/></svg>',
      metaUI_track_style(scale$at(lcl), scale$at(ucl) - scale$at(lcl)), 100 * (es - lcl) / (ucl - lcl))
    else if (spans) sprintf('<i class="metaui-track-diamond" style="%1$s;clip-path:polygon(0 50%%,%2$.1f%% 0,100%% 50%%,%2$.1f%% 100%%)"></i>',
      metaUI_track_style(scale$at(lcl), scale$at(ucl) - scale$at(lcl)), 100 * (es - lcl) / (ucl - lcl))
    else paste0(
      if (has_interval) sprintf('<i class="metaui-track-ci" style="%s"></i>', metaUI_track_style(scale$at(lcl), scale$at(ucl) - scale$at(lcl))),
      if (is.finite(es)) sprintf('<i class="metaui-track-point%s" style="%s%s"></i>', if (combined) " metaui-track-combined" else "", metaUI_track_style(scale$at(es)),
        if (!is.null(size)) sprintf(";width:%1$.1fpx;height:%1$.1fpx;margin:%2$.2fpx 0 0 %2$.2fpx", size, -size / 2) else "")),
    "</div>")
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

# What a model's k counts. Study-level fits and RVE report studies, whatever their level of fit.
metaUI_count_unit <- function(row, studies = NA) {
  if (is.na(row$k)) return("")
  if (row$aggregated || identical(as.numeric(row$k), as.numeric(studies))) return("studies")
  if ("input_rows" %in% names(row) && identical(as.numeric(row$k), as.numeric(row$input_rows))) return("effects")
  ""
}

# Plain wording for the reasons and warnings that model packages give; downloads keep the original text.
metaUI_reader_messages <- c(
  "Minimal number of three studies" = "Needs at least three studies.",
  "computationally singular" = "This selection has too little variation to fit the model.",
  "Missing model interval" = "The model gave an estimate but no interval.",
  "Nonfinite estimate, interval, or count" = "The model gave no finite estimate.",
  "Model extraction must return one value per field" = "The model gave no usable estimate.",
  "Regression requires variable precision" = "Needs at least three studies with different standard errors.",
  "No eligible rows" = "No effects selected.",
  "Requires explicit positive or negative effect direction" = "Needs an expected direction, which the authors did not declare.",
  "P-curve effect estimation only supported for SMD" = "Available only for standardized mean differences.",
  "Bayesian analysis requires at least two studies" = "Needs at least two studies.",
  "p-value intervals contains no effect sizes" = "No studies fall in one of the p-value ranges the model compares, so its estimate is unstable.",
  "p-value intervals contains three or fewer effect sizes" = "Few studies fall in one of the p-value ranges the model compares, so its estimate may be unstable.",
  "Unreliable fit" = "The fit is unreliable: some of its standard errors could not be computed.",
  "NaNs produced" = "Some standard errors could not be computed, a sign that the fit is unreliable.",
  "Single-level factor(s) found in 'random'" = "There is only one study, so the variance between studies cannot be estimated and is set to 0.",
  "non-finite value supplied by optim" = "The model could not be fitted to so few or so similar effects.",
  "exactly singular" = "This selection has too little variation to fit the model.",
  "There must be at least 10 z-scores in the fitting range" = "Needs at least 10 significant results (z between 1.96 and 6); far more are recommended.",
  "Two or less significant" = "Needs at least three significant results (p < .05).",
  "Small-sample degrees of freedom below 4" = "Too few studies for robust variance estimation (small-sample degrees of freedom below 4): its interval and p-value are unreliable.")
metaUI_reader_message <- function(x) {
  hit <- names(metaUI_reader_messages)[vapply(names(metaUI_reader_messages), grepl, logical(1), x = x, fixed = TRUE)]
  if (length(hit)) unname(metaUI_reader_messages[hit[1]]) else x
}
# A failed row leads with the warning that explains it, when the package gave one.
metaUI_reader_reason <- function(reason, warnings = "") {
  parts <- if (nzchar(warnings)) strsplit(warnings, "; ", fixed = TRUE)[[1]] else character()
  explained <- vapply(parts, function(w) startsWith(metaUI_reader_message(w), "Needs"), logical(1))
  if (any(explained)) metaUI_reader_message(parts[explained][1]) else metaUI_reader_message(reason)
}
# A failed row's reason already says that its standard errors could not be computed.
metaUI_reader_warnings <- function(warnings, failed = FALSE) {
  parts <- strsplit(warnings, "; ", fixed = TRUE)[[1]]
  if (failed) parts <- parts[!grepl("NaNs produced", parts, fixed = TRUE)]
  # "No effects in a range" already covers "few effects in a range".
  if (any(grepl("contains no effect sizes", parts, fixed = TRUE))) parts <- parts[!grepl("three or fewer effect sizes", parts, fixed = TRUE)]
  paste(unique(vapply(parts, metaUI_reader_message, character(1))), collapse = " ")
}

# Reader table of model estimates: one row per model, every model named, with reasons
# where a model was not estimated. Downloads keep the full table.
metaUI_estimates_html <- function(table, first = integer(), axis_title = "Effect size", bound = NULL, studies = NA) {
  digits <- metaUI_digits(table$LCL[table$status == "ok"], table$UCL[table$status == "ok"])
  fmt <- function(x) if (is.na(x)) "" else metaUI_minus(formatC(x, format = "f", digits = digits))
  esc <- function(x) htmltools::htmlEscape(as.character(x))
  # Other estimators sit under the primary analysis, grouped by the level they are fitted at.
  rest <- setdiff(seq_len(nrow(table)), first)
  order <- c(first, rest[order(table$aggregated[rest])])
  scale <- metaUI_track_scale(c(table$es, table$LCL, table$UCL, if (!is.null(bound)) c(-bound, bound)))
  types <- unique(stats::na.omit(table$interval_type[table$status == "ok"]))
  ref <- if (length(first) && table$status[first[1]] == "ok") table$es[first[1]] else NULL
  # Warnings are footnotes, so that one long message does not break the rows.
  # A failed row whose reason is its warning needs no footnote.
  # A warning that becomes empty in plain words (it only repeated the failure) gets no footnote.
  note_text <- vapply(seq_len(nrow(table)), function(i) if (nzchar(table$warnings[i])) metaUI_reader_warnings(table$warnings[i], table$status[i] != "ok") else "", character(1))
  warned <- order[nzchar(note_text[order]) & (table$status[order] == "ok" | vapply(order, function(i) !startsWith(metaUI_reader_reason("", table$warnings[i]), "Needs"), logical(1)))]
  rows <- vapply(order, function(i) {
    row <- table[i, ]
    ok <- row$status == "ok"
    # The footnote mark follows the name, so that it cannot wrap onto a line of its own.
    label <- paste0(esc(row$Model), if (i %in% warned) paste0('<sup class="metaui-note-mark">', match(i, warned), "</sup>"), if (i %in% first) paste0(' <span class="metaui-primary-tag">primary, ', if (row$aggregated) "study-level" else "effect-level", " fit</span>"),
      # Confidence intervals need no note; only the rows that differ from them do.
      if (ok && length(types) > 1L && !startsWith(row$interval_type, "Confidence")) paste0('<span class="metaui-row-note">', esc(row$interval_type), "</span>"))
    # A row that is not estimated shows no numbers, even where an unreliable fit left some.
    interval <- if (!ok || (is.na(row$LCL) && is.na(row$UCL))) "" else paste0("[", fmt(row$LCL), ", ", fmt(row$UCL), "]")
    # Effect-level rows all count the effects they use: RVE reports its studies, which would read
    # as a different sample beside the multilevel rows. Downloads keep each model's own k.
    effect_level <- ok && !isTRUE(row$aggregated) && "input_rows" %in% names(row) && !is.na(row$k)
    unit <- if (!ok) "" else if (effect_level) "effects" else metaUI_count_unit(row, studies)
    k_shown <- if (!ok || is.na(row$k)) "" else if (effect_level) row$input_rows else row$k
    track <- if (ok) paste0('<td class="metaui-track-cell">', metaUI_track_row(scale, row$es, row$LCL, row$UCL, bound, ref = ref, diamond = TRUE), "</td>")
      else paste0('<td class="metaui-reason-cell"><b>', "Not estimated:</b> ", esc(metaUI_reader_reason(row$reason, row$warnings)), "</td>")
    position <- match(i, order)
    opens_group <- !(i %in% first) && (position == length(first) + 1L || table$aggregated[order[position - 1L]] != row$aggregated)
    # Methods explains the two levels; the headings stay short enough for one line on phones.
    level <- if (row$aggregated) "study-level estimators" else "effect-level estimators"
    group <- if (opens_group) paste0('<tr class="metaui-group-row"><td colspan="5">',
        if (length(first) && any(table$aggregated[first] == row$aggregated)) paste("Other", level) else paste0(toupper(substr(level, 1, 1)), substring(level, 2)),
        '</td><td class="metaui-track-cell">', metaUI_track_row(scale, NA_real_, NA_real_, NA_real_, bound, ref = ref), "</td></tr>")
    paste0(group, '<tr', if (i %in% first) ' class="metaui-primary-row"', "><td>", label, '</td><td class="num">', if (ok) fmt(row$es) else "",
      '</td><td class="num">', interval, '</td><td class="num metaui-k">', k_shown,
      '</td><td class="metaui-unit">', unit, "</td>", track, "</tr>")
  }, character(1))
  interval_head <- if (length(types) && all(grepl("^Confidence", types))) "95% CI" else "95% interval"
  others <- if (length(first) && nrow(table) > 1L) paste0('<p class="metaui-note metaui-other-estimators">Other estimators answer different questions and are not interchangeable with the primary analysis.',
    if (!is.null(ref)) paste0(" Each diamond spans a model\u2019s ", interval_head, "; the dashed line marks the primary estimate."),
    if (any(grepl("trim", table$Model, ignore.case = TRUE))) " The trim-and-fill count includes imputed studies.", "</p>")
  notes <- if (length(warned)) paste0('<ol class="metaui-warnings">', paste0("<li>", esc(table$Model[warned]), ": ", esc(note_text[warned]), "</li>", collapse = ""), "</ol>")
  paste0('<table class="table metaui-estimates metaui-models metaui-custom"><thead><tr data-head="Model, estimate [', interval_head, ']"><th>Model</th><th class="num">Estimate</th><th class="num">',
    interval_head, '</th><th class="num metaui-k" title="Number of effects or studies the model reports; trim-and-fill includes imputed studies."><i>k</i></th><th></th><th class="metaui-track-cell">', metaUI_track_axis(scale, axis_title), "</th></tr></thead><tbody>",
    paste(rows, collapse = ""), '</tbody><tfoot class="metaui-phone-axis"><tr><td colspan="5"></td><td class="metaui-track-cell">', metaUI_track_axis(scale, axis_title), "</td></tr></tfoot></table>", others, notes)
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

# Only one page of studies is drawn and sent; sorting and paging come back to the server.
# A study sorts by its combined effect, or by its one effect: "estimate" puts the largest first,
# "estimate_low" the smallest.
metaUI_forest_sorted <- function(groups, starts, effects, studies, sort) {
  if (!sort %in% c("estimate", "estimate_low")) return(seq_along(starts))
  estimate <- ifelse(groups$lengths > 1L, studies$es[match(groups$values, studies$study)], effects$es[starts])
  order(if (sort == "estimate") -estimate else estimate, groups$values)
}
# The studies in the order the forest plot pages through them.
metaUI_forest_order <- function(rows, studies = NULL, sort = "name") {
  effects <- rows[rows$kind == "Selected effect", , drop = FALSE]
  groups <- rle(as.character(effects$study))
  ends <- cumsum(groups$lengths); starts <- ends - groups$lengths + 1L
  groups$values[metaUI_forest_sorted(groups, starts, effects, studies, sort)]
}

# found: a study the reader searched for, marked so that it stands out on its page.
metaUI_forest_html <- function(rows, axis_title, studies = NULL, details = NULL, page = 1L, sort = "name", page_size = 50L, found = NULL) {
  fmt <- function(x, digits = 2L) metaUI_minus(formatC(x, format = "f", digits = digits))
  esc <- function(x) htmltools::htmlEscape(as.character(x))
  # Combined rows count too: correlated weights can put them beyond their effects.
  scale <- metaUI_track_scale(c(rows$es, rows$LCL, rows$UCL, studies$es, studies$LCL, studies$UCL))
  summaries <- rows[rows$kind == "Model summary", , drop = FALSE]
  # A custom or Bayesian summary may not be a confidence interval, so it must not be called one.
  types <- attr(rows, "interval_types")
  other_type <- function(label) { type <- unname(types[as.character(label)]); if (length(type) && !is.na(type) && !grepl("^Confidence", type)) type else "" }
  interval_head <- if (any(nzchar(vapply(summaries$label, other_type, character(1))))) "95% interval" else "95% CI"
  effects <- rows[rows$kind == "Selected effect", , drop = FALSE]
  ref <- if (nrow(summaries)) summaries$es[1] else NULL
  width <- effects$UCL - effects$LCL
  size <- pmin(13, pmax(6, 7 * stats::median(width) / width))
  # Only model summaries are narrow enough to need more decimals; study rows stay aligned.
  values <- function(row, digits = 2L) {
    paste0('<td class="num">', fmt(row$es, digits), '</td><td class="num">[', fmt(row$LCL, digits), ", ", fmt(row$UCL, digits), "]</td>")
  }
  grid <- paste0('<td class="metaui-track-cell">', metaUI_track_row(scale, NA_real_, NA_real_, NA_real_, ref = ref), "</td>")
  summary_rows <- if (nrow(summaries)) paste(vapply(seq_len(nrow(summaries)), function(i) paste0('<tr class="metaui-primary-row"><td>', esc(summaries$label[i]), if (nzchar(other_type(summaries$label[i]))) paste0('<span class="metaui-row-note">', esc(other_type(summaries$label[i])), "</span>"), "</td>", values(summaries[i, ], metaUI_digits(summaries$LCL[i], summaries$UCL[i])),
    '<td class="metaui-track-cell">', metaUI_track_row(scale, summaries$es[i], summaries$LCL[i], summaries$UCL[i], ref = ref, diamond = TRUE), "</td></tr>"), character(1)), collapse = "")
  groups <- rle(as.character(effects$study))
  ends <- cumsum(groups$lengths); starts <- ends - groups$lengths + 1L
  if (is.null(studies)) studies <- data.frame(study = character(), es = numeric(), LCL = numeric(), UCL = numeric())
  study_size <- pmin(13, pmax(6, 7 * stats::median(width) / (studies$UCL - studies$LCL)))
  shown <- metaUI_forest_sorted(groups, starts, effects, studies, sort)
  pages <- max(1L, ceiling(length(shown) / page_size))
  page <- suppressWarnings(as.integer(page))
  page <- if (length(page) == 1L && !is.na(page)) max(1L, min(page, pages)) else 1L
  from <- (page - 1L) * page_size + 1L; to <- min(page * page_size, length(shown))
  study_rows <- vapply(shown[seq(from, length.out = max(0L, to - from + 1L))], function(s) {
    index <- starts[s]:ends[s]
    one <- function(i, label, class) paste0('<tr class="', class, '" data-es="', effects$es[i], '"><td>', label, "</td>", values(effects[i, ]),
      '<td class="metaui-track-cell">', metaUI_track_row(scale, effects$es[i], effects$LCL[i], effects$UCL[i], size = size[i], ref = ref), "</td></tr>")
    name <- groups$values[s]
    mark <- if (identical(name, found)) " metaui-found" else ""
    if (length(index) == 1L) return(one(index, esc(name), paste0("metaui-forest-single", mark)))
    labels <- substring(effects$label[index], nchar(name) + 3L)
    # Several effects: one combined row, which opens to show them (metaui.js).
    combined <- match(name, studies$study)
    head <- paste0('<button type="button" class="metaui-forest-toggle" aria-expanded="false">', esc(name), ' <span class="metaui-forest-count">\u00b7 ', length(index), "\u00a0effects</span></button>")
    paste0(if (is.na(combined)) paste0('<tr class="metaui-forest-study', mark, '"><td colspan="3">', head, "</td>", grid, "</tr>")
      else paste0('<tr class="metaui-forest-study', mark, '" data-es="', studies$es[combined], '"><td>', head, "</td>", values(studies[combined, ]), '<td class="metaui-track-cell">',
        metaUI_track_row(scale, studies$es[combined], studies$LCL[combined], studies$UCL[combined], size = study_size[combined], ref = ref, combined = TRUE), "</td></tr>"),
      paste(vapply(seq_along(index), function(j) {
        detail <- if (is.null(details)) "" else details[paste(effects$study[index[j]], effects$effect_id[index[j]])]
        one(index[j], paste0("Effect ", esc(labels[j]), if (!is.na(detail) && nzchar(detail)) paste0(' <span class="metaui-forest-count">', esc(detail), "</span>")), "metaui-forest-effect metaui-forest-closed")
      }, character(1)), collapse = ""))
  }, character(1))
  # A menu of pages goes straight to any page; the count says which studies it holds.
  pager <- if (pages > 1L) paste0('<p class="metaui-note metaui-key metaui-forest-controls metaui-forest-pager"><button type="button" class="metaui-forest-prev" data-page="', page - 1L, '"', if (page == 1L) " disabled", '>Previous</button>',
    '<button type="button" class="metaui-forest-next" data-page="', page + 1L, '"', if (page == pages) " disabled", '>Next</button>',
    '<label class="metaui-forest-jump">Page <select class="metaui-forest-page">', paste0('<option value="', seq_len(pages), '"', ifelse(seq_len(pages) == page, " selected", ""), ">", seq_len(pages), "</option>", collapse = ""),
    "</select> of ", pages, '</label><span class="metaui-forest-status" aria-live="polite">Studies ', from, "\u2013", to, " of ", length(shown), "</span></p>") else ""
  paste0('<p class="metaui-note metaui-key metaui-forest-key"><span><i class="metaui-glyph metaui-glyph-dot"></i>One effect; larger is more precise</span>',
    if (any(groups$lengths > 1L)) '<span><i class="metaui-glyph metaui-glyph-square"></i>A study\'s effects combined</span>',
    '<span><i class="metaui-glyph metaui-glyph-diamond"></i>Model summary; its width is the ', interval_head, '</span>',
    if (!is.null(ref)) paste0('<span><i class="metaui-glyph metaui-glyph-dash"></i>', esc(summaries$label[1]), ", ", fmt(ref, metaUI_digits(summaries$LCL[1], summaries$UCL[1])), "</span>"), "</p>",
    '<p class="metaui-note metaui-key metaui-forest-controls">',
    if (any(groups$lengths > 1L)) metaUI_switch("Effects within studies", "metaui-forest-all", c(studies = "hidden", effects = "shown"), "studies"),
    metaUI_switch("Order by", "metaui-forest-sort", c(name = "study name", estimate = "largest estimate", estimate_low = "smallest estimate"), if (sort %in% c("estimate", "estimate_low")) sort else "name"), "</p>", pager,
    '<table class="table metaui-estimates metaui-forest metaui-custom"><thead><tr data-head="Study, estimate [', interval_head, ']"><th>Study</th><th class="num">Estimate</th><th class="num">', interval_head, '</th><th class="metaui-track-cell">',
    metaUI_track_axis(scale, axis_title), "</th></tr></thead><tbody>", summary_rows, paste(study_rows, collapse = ""),
    '</tbody><tfoot><tr><td colspan="3"></td><td class="metaui-track-cell">', metaUI_track_axis(scale, axis_title), "</td></tr></tfoot></table>", pager)
}

# Category counts with bars in the table, including levels the selection leaves empty.
metaUI_bars_html <- function(counts, name) {
  other <- attr(counts, "other_levels")
  # Levels without selected effects are counted in one line instead of listed.
  empty <- sum(counts$Count == 0)
  counts <- counts[counts$Count > 0, , drop = FALSE]
  counts <- counts[order(as.character(counts[[name]]) == "(Missing)"), , drop = FALSE]
  levels <- as.character(counts[[name]])
  bucket <- levels == ".metaUI_other"
  levels[bucket] <- paste(length(other), "other categories")
  largest <- max(counts$Count, 1)
  rows <- sprintf('<tr%s><td>%s</td><td class="num">%s</td><td class="num">%s</td><td class="metaui-bar-cell" aria-hidden="true"><span class="metaui-bar" style="width:%s%%"></span></td></tr>',
    ifelse(counts$Count == 0, ' class="metaui-zero"', ifelse(levels == "(Missing)", ' class="metaui-missing"', "")), htmltools::htmlEscape(levels), counts$Count,
    sprintf("%.1f%%", 100 * counts$Count / max(sum(counts$Count), 1)), round(100 * counts$Count / largest, 1))
  paste0('<table class="table metaui-bars metaui-custom"><thead><tr><th><span class="sr-only">', htmltools::htmlEscape(name),
    '</span></th><th class="num">Effects</th><th class="num">Share</th><th class="metaui-bar-cell"></th></tr></thead><tbody>', paste(rows, collapse = ""), "</tbody></table>",
    if (any(bucket)) paste0('<details class="metaui-help metaui-other-levels"><summary>The ', length(other), " other categories</summary><p>", htmltools::htmlEscape(paste(other, collapse = ", ")), "</p></details>"),
    if (empty) paste0('<p class="metaui-note">', empty, if (empty == 1L) " category has" else " categories have", " no selected effects.</p>"))
}

# The effects that enter a moderator test: those with a value, leaving out a kept (Missing) category.
metaUI_moderator_rows <- function(df, moderator) {
  x <- df[[moderator]]
  keep <- !is.na(x) & !(as.character(x) %in% "(Missing)")
  df <- df[keep, , drop = FALSE]
  if (is.factor(df[[moderator]])) df[[moderator]] <- droplevels(df[[moderator]])
  df
}

# Moderator tests ---------------------------------------------------------------
# The moderator is added to the primary model where that is an effect-level metafor::rma.mv model,
# so that the test keeps its dependence structure (a CHE model's within-study correlation, say);
# otherwise to the default multilevel model. Either way, tests and intervals are cluster-robust by
# study (CR2 with small-sample degrees of freedom, clubSandwich), so they allow for dependent
# effects even where the working model gets the dependence wrong.
metaUI_call_name <- function(e) {
  if (!is.call(e)) return("")
  f <- e[[1]]
  if (is.name(f)) as.character(f) else if (is.call(f) && as.character(f[[1]]) %in% c("::", ":::")) as.character(f[[3]]) else ""
}

metaUI_default_moderator_call <- quote(metafor::rma.mv(metaUI__effect_size, V = metaUI__variance,
  random = ~ 1 | metaUI__study_id/metaUI__effect_id, data = df, test = "t", dfs = "contain", method = "REML", sparse = TRUE))

# Model code with the moderator added to its one rma.mv call (a final robust() is dropped, as
# robust inference is added afterwards); NULL when the code has no single such call to extend.
metaUI_moderator_code <- function(code, mods, control = NULL) {
  expr <- tryCatch(parse(text = code), error = function(e) NULL)
  if (is.null(expr) || !length(expr)) return(NULL)
  found <- 0L
  walk <- function(e) {
    if (!is.call(e)) return(e)
    if (metaUI_call_name(e) == "rma.mv") { found <<- found + 1L; e$mods <- mods; if (!is.null(control)) e$control <- control; return(e) }
    for (i in seq_along(e)[-1]) if (!is.null(e[[i]])) e[[i]] <- walk(e[[i]])
    e
  }
  strip <- function(e) {
    if (metaUI_call_name(e) == "{") { e[[length(e)]] <- strip(e[[length(e)]]); e }
    else if (metaUI_call_name(e) == "robust") e[[2]] else e
  }
  expr[[length(expr)]] <- strip(expr[[length(expr)]])
  for (i in seq_along(expr)) expr[[i]] <- walk(expr[[i]])
  if (found == 1L) expr else NULL
}

# primary: list(name, code) of the authors' primary model, or NULL. Returns the fitted model
# (robust where possible), which model it extends, and whether inference is cluster-robust.
metaUI_moderator_fit <- function(df, moderator, primary = NULL, correlation = .6) {
  if (is.factor(df[[moderator]])) df[[moderator]] <- droplevels(df[[moderator]])
  mods <- stats::as.formula(paste0("~`", moderator, "`"))
  env <- new.env(parent = environment(metaUI_moderator_fit))
  env$df <- df
  attr(env$df, "metaUI_correlation") <- correlation
  model <- NULL
  working <- "default"
  # A variance component at zero can stop the default optimizer short; more iterations, then
  # another optimizer, usually reach the same optimum.
  controls <- list(NULL, list(iter.max = 1000, rel.tol = 1e-8), list(optimizer = "BFGS"))
  if (!is.null(primary)) {
    if (identical(primary$code, metaUI_code_multilevel)) working <- "primary" else {
      for (control in controls) {
        expr <- metaUI_moderator_code(primary$code, mods, control)
        if (is.null(expr)) break
        model <- tryCatch(suppressWarnings(eval(expr, env)), error = function(e) NULL)
        if (!is.null(model)) break
      }
      if (inherits(model, "rma.mv") && model$k == nrow(df) && NCOL(model$X) > 1L) working <- "primary" else model <- NULL
    }
  }
  if (is.null(model)) for (i in seq_along(controls)) {
    call <- metaUI_default_moderator_call
    call$mods <- mods
    if (!is.null(controls[[i]])) call$control <- controls[[i]]
    model <- tryCatch(eval(call, env), error = function(e) if (i == length(controls)) stop(e) else NULL)
    if (!is.null(model)) break
  }
  # Level estimates and intervals: CR2 with Satterthwaite df. The overall test is clubSandwich's HTZ
  # test, computed by metaUI_moderator_omnibus() because clubSandwich's own route builds a
  # q x q x studies x studies array (over 3 GB and many minutes for 33 categories on 2,628 effects).
  # The cheap robust() Wald test only fills the object's slots until it is replaced.
  robust <- tryCatch(suppressWarnings(metafor::robust(model, cluster = df$metaUI__study_id, clubSandwich = TRUE, wald_test = "Naive-Fp")),
    error = function(e) NULL)
  omnibus <- FALSE
  if (!is.null(robust)) {
    test <- tryCatch(metaUI_moderator_omnibus(model, df$metaUI__study_id[model$not.na]), error = function(e) NULL)
    if (!is.null(test) && is.finite(test$p_value)) {
      robust$QM <- test$statistic
      robust$QMdf <- c(test$df_num, test$df_denom)
      robust$QMp <- test$p_value
      omnibus <- TRUE
    }
  }
  list(model = if (is.null(robust)) model else robust, working = working, robust = !is.null(robust), omnibus = omnibus || is.null(robust))
}

# clubSandwich's HTZ Wald test (CR2, Tipton & Pustejovsky 2015, doi:10.3102/1076998615606099) of all
# non-intercept coefficients of an inverse-variance weighted rma.mv fit (not its robust() version).
# The same statistic and df as clubSandwich::Wald_test(test = "HTZ"), with the variance sum taken by
# small matrix contractions instead of a q x q x studies x studies array; checked to agree within 1e-8.
metaUI_moderator_omnibus <- function(model, cluster, constraints = NULL, vcov = NULL) {
  if (is.null(vcov)) vcov <- clubSandwich::vcovCR(model, cluster = cluster, type = "CR2")
  if (!inherits(vcov, "clubSandwich") || attr(vcov, "type") != "CR2") stop("Need CR2 vcovCR object.")
  beta <- as.numeric(stats::coef(model))
  p <- length(beta)
  if (is.null(constraints)) constraints <- diag(p)[-1L, , drop = FALSE]
  C <- as.matrix(constraints)
  q <- nrow(C)
  if (!q || ncol(C) != p || anyNA(beta) || qr(C)$rank != q) stop("Invalid constraints or aliased coefficients.")
  if (!inherits(model, "rma.mv") || inherits(model, "robust.rma") || !is.null(model$W))
    stop("Supply the original rma.mv fit with default inverse-variance weights.")
  if (!isTRUE(attr(vcov, "inverse_var"))) stop("Custom working targets are unsupported.")
  cl <- attr(vcov, "cluster")
  if (length(cluster) != nrow(model$X) || anyNA(cluster) || !identical(as.integer(factor(cluster)), as.integer(cl)))
    stop("cluster must match model rows and CR2 clusters, without missing values.")
  M <- attr(vcov, "bread") / attr(vcov, "v_scale")
  E <- Map(function(e, a) M %*% e %*% a, attr(vcov, "est_mats"), attr(vcov, "adjustments"))
  U <- lapply(split(seq_len(nrow(model$X)), cl), function(i) model$X[i, , drop = FALSE])
  G <- Map(function(e, theta) C %*% e %*% t(chol(theta)), E, attr(vcov, "target"))
  L <- t(chol(M))
  H <- Map(function(e, x) C %*% e %*% x %*% L, E, U)
  D <- lapply(G, tcrossprod)
  Omega <- Reduce("+", D) - Reduce("+", lapply(H, tcrossprod))
  eig <- eigen((Omega + t(Omega)) / 2, symmetric = TRUE)
  if (min(eig$values) <= max(eig$values) * 1e-10) stop("Working covariance is singular or ill-conditioned.")
  A <- (eig$vectors * rep(1 / sqrt(eig$values), each = q)) %*% t(eig$vectors)
  H <- lapply(H, function(h) A %*% h)
  D <- lapply(D, function(d) A %*% d %*% A)
  # K[s, a, t, b] = sum over studies of H_i[s, a] H_i[t, b]
  K <- tcrossprod(vapply(H, as.numeric, numeric(q * p)))
  dim(K) <- c(q, p, q, p)
  total <- sum(K * aperm(K, c(3, 2, 1, 4))) + sum(K^2)
  for (i in seq_along(H)) {
    HH <- tcrossprod(H[[i]])
    Di <- D[[i]]
    total <- total + sum(Di * t(Di)) - 2 * sum(Di * t(HH)) + sum(diag(Di))^2 - 2 * sum(diag(Di)) * sum(diag(HH))
  }
  if (!is.finite(total) || total <= 0) stop("HTZ variance calculation is degenerate.")
  nu <- q * (q + 1) / total
  df <- nu - q + 1
  b <- drop(C %*% beta)
  S <- C %*% vcov %*% t(C)
  if (rcond(S) < 1e-12) stop("Robust covariance is singular or ill-conditioned.")
  Q <- drop(crossprod(b, solve(S, b)))
  statistic <- max(df / nu, 0) * Q / q
  data.frame(statistic = statistic, df_num = q, df_denom = df,
    p_value = if (df > 0) stats::pf(statistic, q, df, lower.tail = FALSE) else NA_real_)
}

# Estimate and interval of each level of a categorical moderator, from the model with an intercept.
metaUI_moderator_levels <- function(model, levels) {
  if (NCOL(model$X) != length(levels)) stop("Moderator levels and model coefficients do not match.")
  newmods <- rbind(rep(0, length(levels) - 1L), diag(length(levels) - 1L))
  pred <- as.data.frame(stats::predict(model, newmods = newmods))
  data.frame(level = levels, b = pred$pred, ci.lb = pred$ci.lb, ci.ub = pred$ci.ub)
}

# Moderator levels on the same drawn axis; the dots are the selected effects themselves.
# Without the individual effects, the axis fits the level estimates and their intervals; with
# them, it covers every effect, which can leave narrow intervals as slivers.
metaUI_moderator_html <- function(levels, estimate, lcl, ucl, k, effects, values, labels, axis_title, ref = NULL, se = NULL, show_effects = TRUE, keep_order = FALSE, ref_label = "Random-Effects Multilevel Model") {
  digits <- metaUI_digits(lcl, ucl)
  fmt <- function(x) metaUI_minus(formatC(x, format = "f", digits = digits))
  scale <- metaUI_track_scale(c(estimate, lcl, ucl, ref, if (show_effects) values))
  # Largest estimate first, unless the author ordered the levels (metaUI_author_order); a missing-data level stays at the end.
  order <- order(levels == "(Missing)", if (keep_order) seq_along(levels) else -estimate)
  # Dots follow precision, as in the numeric moderator's plot.
  sizes <- if (is.null(se)) NULL else pmin(6.5, pmax(4, 4.5 * stats::median(se, na.rm = TRUE) / se))
  rows <- vapply(order, function(i) {
    level <- as.character(effects) == as.character(levels[i])
    level[is.na(level)] <- FALSE
    paste0("<tr><td>", htmltools::htmlEscape(levels[i]), '</td><td class="num">', fmt(estimate[i]),
      '</td><td class="num">[', fmt(lcl[i]), ", ", fmt(ucl[i]), ']</td><td class="num metaui-k">', k[i], '</td><td class="metaui-track-cell">',
      metaUI_track_row(scale, estimate[i], lcl[i], ucl[i], dots = if (show_effects) values[level], dot_labels = if (show_effects) labels[level], ref = ref, dot_sizes = if (show_effects) sizes[level], diamond = TRUE,
        hollow = isTRUE(lcl[i] <= 0 && ucl[i] >= 0)), "</td></tr>")
  }, character(1))
  paste0('<p class="metaui-note metaui-key metaui-forest-controls">',
    metaUI_switch("Individual effects", "metaui-moderator-effects", c(hide = "hidden", show = "shown"), if (show_effects) "show" else "hide"), "</p>",
    '<table class="table metaui-estimates metaui-moderators', if (!show_effects) " metaui-moderators-plain", ' metaui-custom"><thead><tr data-head="Level, estimate [95% CI]"><th>Level</th><th class="num">Estimate</th><th class="num">95% CI</th><th class="num metaui-k"><i>k</i></th><th class="metaui-track-cell">',
    metaUI_track_axis(scale, axis_title), "</th></tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table>",
    '<p class="metaui-note metaui-key">Diamonds: level estimates from the moderator model named above; each spans its 95% CI, and is filled where the CI excludes zero, outlined where it includes it.',
    if (show_effects) '<span class="metaui-dots-note"> Grey dots beneath: selected effects, larger when more precise, stacked only so that none hides another.</span>',
    if (!is.null(ref)) paste0(" Dashed line: overall estimate of the ", htmltools::htmlEscape(ref_label), ", as on the Summary (", fmt(ref), ")."),
    if (show_effects) '<span class="metaui-hist-note" hidden> Levels with many effects show bars instead of dots: the number of effects in each range (tallest bar: <span class="metaui-hist-max"></span> effects).</span>',
    "</p>")
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

# P-uniform* reports only a 95% profile-likelihood interval. This repeats its profile
# (the same likelihood and significance threshold, alpha = .05 two-sided) at another level.
metaUI_puni_star_interval <- function(mod, df, level = .90, alpha = .05) {
  ml_star <- utils::getFromNamespace("ml_star", "puniform")
  ml_star_tau <- utils::getFromNamespace("ml_star_tau", "puniform")
  yi <- df$metaUI__effect_size; vi <- df$metaUI__variance
  ycv <- stats::qnorm(alpha / 2, lower.tail = FALSE) * sqrt(vi)
  ll <- stats::optim(c(mean(yi), stats::sd(yi)), ml_star, yi = yi, vi = vi, ycv = ycv, lower = c(-Inf, 0),
    method = "L-BFGS-B", control = list(fnscale = -1, maxit = 300))$value
  profile <- function(d) -2 * (stats::optimize(ml_star_tau, c(0, 2), d = d, yi = yi, vi = vi, ycv = ycv, maximum = TRUE)$objective - ll) -
    stats::qchisq(level, 1)
  c(stats::uniroot(profile, c(mod$est - 3, mod$est))$root, stats::uniroot(profile, c(mod$est, mod$est + 3))$root)
}

# Containment degrees of freedom: t tests count studies rather than effects, so that many effects
# from few studies do not narrow the interval.
metaUI_multilevel_fit <- function(df) {
  metafor::rma.mv(df$metaUI__effect_size, V = df$metaUI__variance,
    random = ~ 1 | metaUI__study_id/metaUI__effect_id, data = df,
    test = "t", dfs = "contain", method = "REML", sparse = TRUE)
}

# The within-study correlation is the one declared for the app (metaUI_fit_models attaches it), .6 if none.
metaUI_rve_fit <- function(df) {
  rho <- attr(df, "metaUI_correlation")
  fit <- robumeta::robu(metaUI__effect_size ~ 1, data = df,
    studynum = df$metaUI__study_id, var.eff.size = df$metaUI__variance,
    rho = if (is.null(rho)) .6 else rho, small = TRUE)
  # Tipton (2015): even corrected tests are unreliable below 4 small-sample degrees of freedom.
  dfs <- as.numeric(fit$reg_table$dfs[1])
  if (is.finite(dfs) && dfs < 4) warning("Small-sample degrees of freedom below 4 (df = ", formatC(dfs, format = "f", digits = 1), ")", call. = FALSE)
  fit
}

# One cache per server session. Exact keys include all data attributes and model
# code/configuration; no hash collisions, cross-user state, or unbounded history.
metaUI_fit_cache <- function(max_entries = 3L, limit = 3L, seed = NULL) {
  if (length(max_entries) != 1L || !is.numeric(max_entries) || !is.finite(max_entries) ||
      max_entries < 0 || max_entries > limit || max_entries != as.integer(max_entries))
    stop("fit_cache_entries must be an integer from 0 to ", limit, ".")
  # Zero means every analysis runs afresh, so a saved fit must not answer either.
  entries <- if (is.null(seed) || max_entries == 0L) list() else list(seed)
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
  # Variance components of any model with the default random structure (a CHE model, say), so that
  # heterogeneity can come from the primary model without keeping its whole fit.
  components <- vector("list", nrow(models))
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
    prediction <- c(NA_real_, NA_real_)
    interval90 <- c(NA_real_, NA_real_)
    if (is.null(reason)) {
      started <- proc.time()[["elapsed"]]
      reason <- tryCatch({
        env <- new.env(parent = environment(metaUI_fit_models))
        env$df <- x
        attr(env$df, "metaUI_correlation") <- correlation
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
        # The 90% interval serves only the equivalence assessment; without one, that model is not assessed there.
        fields90 <- intersect(c("LCL90", "UCL90"), names(spec))
        if (length(fields90) == 2L && !anyNA(unlist(spec[fields90]))) interval90 <- tryCatch({
          value <- suppressWarnings(vapply(fields90, function(field) as.numeric(eval(parse(text = spec[[field]]), env))[1], numeric(1)))
          if (any(!is.finite(value)) || value[1] > value[2]) c(NA_real_, NA_real_) else if (reflected) -rev(unname(value)) else unname(value)
        }, error = function(e) c(NA_real_, NA_real_))
        status <- "ok"
        # NaN standard errors mean the fit sits where its curvature is invalid (a variance at its
        # boundary, say): its estimate and interval are not trustworthy. Downloads keep them, marked.
        if (any(grepl("NaNs produced", warnings, fixed = TRUE))) {
          status <- "failed"
          stop("Unreliable fit: some standard errors could not be computed (NaNs produced). Do not interpret its estimate or interval.")
        }
        # Where true effects are expected to lie: the prediction interval of intercept-only metafor models.
        if (inherits(env$mod, "rma") && is.null(env$mod$formula.mods) && NCOL(env$mod$X) == 1L) {
          pred <- tryCatch(suppressWarnings(stats::predict(env$mod)), error = function(e) NULL)
          if (!is.null(pred$pi.lb) && all(is.finite(c(pred$pi.lb, pred$pi.ub)))) prediction <- if (reflected) -c(pred$pi.ub, pred$pi.lb) else c(pred$pi.lb, pred$pi.ub)
        }
        if (bayesian || (!spec$aggregated && !reflected && spec$code %in% c(metaUI_code_multilevel, metaUI_code_rve))) fits[[i]] <<- env$mod
        if (!spec$aggregated && !reflected && inherits(env$mod, "rma.mv") && identical(unname(env$mod$s.names), c("metaUI__study_id", "metaUI__study_id/metaUI__effect_id")))
          components[[i]] <<- list(sigma2 = env$mod$sigma2, QE = env$mod$QE, QEp = env$mod$QEp)
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
      if (!is.null(spec[["interval_type"]]) && !is.na(spec[["interval_type"]])) as.character(spec[["interval_type"]]) else
      if (is.na(metaUI_model_role(spec, NA_character_))) "Author-defined interval (type and coverage unknown)" else
      "Confidence interval (coverage model-dependent)"
    row$PI_LCL <- prediction[1]; row$PI_UCL <- prediction[2]
    row$LCL90 <- interval90[1]; row$UCL90 <- interval90[2]
    row$fit_es <- row$es; row$fit_LCL <- row$LCL; row$fit_UCL <- row$UCL
    shown <- c("es", "LCL", "UCL", "PI_LCL", "PI_UCL", "LCL90", "UCL90")
    if (df$metaUI__es_type[1] == "ZCOR") row[shown] <- lapply(row[shown], tanh)
    row
  })
  list(df_agg = df_agg, table = dplyr::bind_rows(rows), fits = fits, components = components, fit_data = df)
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

# What a reader may change in the workbook's dataset sheet. "Edit": read on upload. "Derived":
# recomputed from the edited columns on upload, so edits to them are ignored. "Fixed": the same in
# every row. "Other": carried with the data; the default models do not use them.
metaUI_upload_roles <- function(built) {
  specs <- metaUI_computation_specs(built)
  input_scale <- attr(built, "metaUI_validation")$input_scale
  if (is.null(input_scale)) input_scale <- built$metaUI__es_type[1]
  scale <- c(SMD = "standardized mean difference", COR = "correlation r", ZCOR = "Fisher z")[[input_scale]]
  fitting <- if (built$metaUI__es_type[1] == "ZCOR") "Fisher z" else "standardized mean difference"
  edit <- c(metaUI__study_id = "Study name or ID. Rows with the same value form one study.",
    metaUI__effect_id = "Effect ID, unique within its study. Leave it empty in a new row to number the row automatically.",
    metaUI__es_label = "Label of the effect, shown in the forest plot. Optional.",
    metaUI__input_effect = paste0("Effect size as supplied (", scale, ")."),
    metaUI__input_variance = paste0("Sampling variance of that effect size, on the same scale: the square of its standard error."),
    metaUI__pvalue = "Reported p-value. Optional.",
    metaUI__N = "Sample size. Optional.",
    metaUI__url = "Web address or DOI of the source. Optional.",
    metaUI__article_label = "Source (article) of the study. Optional.")
  filters <- grep("^metaUI__filter_", names(built), value = TRUE)
  filter_notes <- vapply(filters, function(col) paste0("Filter and moderator \"", sub("^metaUI__filter_", "", col), "\": ",
    if (is.numeric(built[[col]])) "a number, or empty if unknown." else "a category. A new category is added to the filter."), character(1))
  alt_edit <- unlist(lapply(specs, function(x) stats::setNames(paste0(x$name, ": effect size and its sampling variance as supplied (", c("effect", "variance"), ")."), c(x$input_effect, x$input_variance))))
  derived <- c(metaUI__effect_size = paste0("Effect size on the scale the models use (", fitting, "), from metaUI__input_effect."),
    metaUI__variance = "Sampling variance on that scale, from metaUI__input_variance.",
    metaUI__se = "Standard error: the square root of metaUI__variance.",
    metaUI__es_z = "z-score of the effect against the authors' data, used by the z-score filter.")
  alt_derived <- unlist(lapply(specs, function(x) stats::setNames(paste0(x$name, ": ", c("effect size", "variance"), " on the scale the models use."), c(x$fit_effect, x$fit_variance))))
  fixed <- c(metaUI__es_type = "Effect-size scale the models use.", metaUI__display_scale = "Scale results are shown on.",
    metaUI__direction = "Expected direction of the effect, declared by the authors.")
  notes <- c(edit[names(edit) %in% names(built)], filter_notes, alt_edit)
  roles <- data.frame(column = c(names(notes), names(derived), names(alt_derived), names(fixed)),
    role = c(rep("Edit", length(notes)), rep("Derived", length(derived) + length(alt_derived)), rep("Fixed", length(fixed))),
    notes = unname(c(notes, derived, alt_derived, fixed)), stringsAsFactors = FALSE)
  roles <- roles[roles$column %in% names(built), , drop = FALSE]
  other <- setdiff(names(built), roles$column)
  rbind(roles, data.frame(column = other, role = rep("Other", length(other)),
    notes = rep("Kept with the data. The app's default models do not use it; the authors' own models may.", length(other)), stringsAsFactors = FALSE))
}

# The dataset sheet in the order of the roles above, so that the columns to edit come first.
metaUI_workbook_dataset <- function(data, built) {
  order <- intersect(metaUI_upload_roles(built)$column, names(data))
  data[c(order, setdiff(names(data), order))]
}

# The instructions sheet that precedes the dataset sheet.
metaUI_workbook_instructions <- function(built) {
  roles <- metaUI_upload_roles(built)
  steps <- c("The dataset sheet holds the data behind these results. To analyse changed data, edit, add or delete rows there, save the workbook, and upload it in the app (under \"Download data or code, upload data\", choose the file and select Upload and analyze).",
    "Change only the columns marked Edit. Columns marked Derived are recomputed from them when you upload, so changes to Derived columns are ignored, and new rows can leave them empty.",
    "Columns marked Fixed are the same in every row (the effect-size scale and the expected direction); new rows can leave them empty.",
    "In a new row, leave metaUI__effect_id empty to number the effect automatically. A new category in a filter column is added to that filter and selected.",
    "The filters sheet records the selection behind these results; the app restores it after the upload. The other sheets are results and are not read back.")
  rbind(data.frame(Column = "", Role = paste("Step", seq_along(steps)), Notes = steps, stringsAsFactors = FALSE),
    data.frame(Column = roles$column, Role = roles$role, Notes = roles$notes, stringsAsFactors = FALSE))
}

# Workbook rows, as a reader sees them: row 1 holds the column names.
metaUI_upload_rows <- function(bad) {
  rows <- which(bad) + 1L
  paste0(if (length(rows) > 1L) "rows " else "row ", paste(utils::head(rows, 5L), collapse = ", "), if (length(rows) > 5L) ", ..." else "")
}

# The edited columns are authoritative: fitting-scale effect and variance, SE and z-score are
# recomputed from them; fixed columns and empty effect IDs are filled in for new rows.
metaUI_validate_upload <- function(df, built) {
  df <- as.data.frame(df)
  if (!nrow(df)) stop("The dataset sheet has no rows.", call. = FALSE)
  roles <- metaUI_upload_roles(built)
  needed <- roles$column[roles$role %in% c("Edit", "Other") & !roles$column %in% c("metaUI__effect_id", "metaUI__es_label")]
  missing <- setdiff(needed, names(df))
  if (length(missing)) stop("The dataset sheet lacks ", if (length(missing) > 1L) "the columns " else "the column ", paste(missing, collapse = ", "),
    ". Start from a workbook downloaded from this app and keep its columns.", call. = FALSE)
  for (field in c("metaUI__es_type", "metaUI__display_scale", "metaUI__direction")) {
    value <- as.character(built[[field]][1])
    given <- if (field %in% names(df)) as.character(df[[field]]) else rep(NA_character_, nrow(df))
    other <- !is.na(given) & given != value
    if (any(other)) stop(field, " must be \"", value, "\" or empty (it is not in ", metaUI_upload_rows(other),
      "). This app cannot analyse another effect-size scale or direction.", call. = FALSE)
    df[[field]] <- value
  }
  study <- df$metaUI__study_id
  empty <- is.na(study) | trimws(as.character(study)) == ""
  if (any(empty)) stop("Every row needs a study in metaUI__study_id (empty in ", metaUI_upload_rows(empty), ").", call. = FALSE)
  if (!"metaUI__effect_id" %in% names(df)) df$metaUI__effect_id <- NA
  ids <- df$metaUI__effect_id
  added <- is.na(ids) | trimws(as.character(ids)) == ""
  if (any(added)) {
    if (is.numeric(built$metaUI__effect_id) && is.numeric(ids) || all(is.na(ids))) {
      ids <- as.numeric(ids)
      ids[added] <- max(c(0, built$metaUI__effect_id, ids), na.rm = TRUE) + seq_len(sum(added))
      if (!is.numeric(built$metaUI__effect_id)) ids <- as.character(ids)
    } else {
      ids <- as.character(ids)
      taken <- unique(c(as.character(built$metaUI__effect_id), ids))
      fresh <- setdiff(paste0("added-", seq_len(sum(added) + length(taken))), taken)
      ids[added] <- fresh[seq_len(sum(added))]
    }
    df$metaUI__effect_id <- ids
  }
  repeated <- duplicated(df[c("metaUI__study_id", "metaUI__effect_id")])
  if (any(repeated)) stop("Effect IDs must be unique within each study (repeated in ", metaUI_upload_rows(repeated), ").", call. = FALSE)
  if ("metaUI__es_label" %in% names(built)) {
    if (!"metaUI__es_label" %in% names(df)) df$metaUI__es_label <- NA
    unlabelled <- is.na(df$metaUI__es_label) | trimws(as.character(df$metaUI__es_label)) == ""
    if (any(unlabelled)) df$metaUI__es_label[unlabelled] <- if (is.numeric(df$metaUI__es_label))
      stats::ave(seq_len(nrow(df)), as.character(df$metaUI__study_id), FUN = seq_along)[unlabelled] else as.character(df$metaUI__effect_id[unlabelled])
  }
  number <- function(field, label, positive = FALSE) {
    x <- suppressWarnings(as.numeric(df[[field]]))
    bad <- !is.finite(x) | (positive & x <= 0)
    if (any(bad)) stop(field, " (", label, ") must be ", if (positive) "a positive number" else "a number", " in every row (not in ", metaUI_upload_rows(bad), ").", call. = FALSE)
    x
  }
  input_scale <- attr(built, "metaUI_validation")$input_scale
  # Source to fitting scale, as prepare_data() did: atanh(r), with delta-method variance.
  to_fit <- function(y, v, scale, what) {
    if (identical(scale, "COR")) {
      outside <- abs(y) >= 1
      if (any(outside)) stop(what, " are correlations and must lie between -1 and 1 (not in ", metaUI_upload_rows(outside), ").", call. = FALSE)
      list(y = atanh(y), v = v / (1 - y^2)^2)
    } else list(y = y, v = v)
  }
  differs <- function(field, new, tolerance) {
    old <- if (field %in% names(df)) suppressWarnings(as.numeric(df[[field]])) else rep(NA_real_, length(new))
    is.na(old) | abs(old - new) > tolerance * pmax(1, abs(new))
  }
  fit <- to_fit(number("metaUI__input_effect", "effect size"), number("metaUI__input_variance", "sampling variance", TRUE), input_scale, "Effect sizes")
  recomputed <- differs("metaUI__effect_size", fit$y, 1e-8) | differs("metaUI__variance", fit$v, 1e-6) | differs("metaUI__se", sqrt(fit$v), 1e-6)
  df$metaUI__effect_size <- fit$y; df$metaUI__variance <- fit$v; df$metaUI__se <- sqrt(fit$v)
  specs <- metaUI_computation_specs(built)
  for (spec in specs) {
    alt <- to_fit(number(spec$input_effect, paste(spec$name, "effect size")), number(spec$input_variance, paste(spec$name, "variance"), TRUE), spec$input_scale, spec$name)
    recomputed <- recomputed | differs(spec$fit_effect, alt$y, 1e-8) | differs(spec$fit_variance, alt$v, 1e-6)
    df[[spec$fit_effect]] <- alt$y; df[[spec$fit_variance]] <- alt$v
  }
  # Optional inputs, as prepare_data() treats them: impossible values count as missing.
  if ("metaUI__pvalue" %in% names(df)) { p <- suppressWarnings(as.numeric(df$metaUI__pvalue)); p[!is.finite(p) | p < 0 | p > 1] <- NA_real_; df$metaUI__pvalue <- p }
  if ("metaUI__N" %in% names(df)) { n <- suppressWarnings(as.numeric(df$metaUI__N)); n[!is.finite(n) | n <= 0] <- NA_real_; df$metaUI__N <- n }
  metaUI_validate_prepared(df)
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
  supplied <- if ("metaUI__es_z" %in% names(df)) suppressWarnings(as.numeric(df$metaUI__es_z)) else rep(NA_real_, nrow(df))
  disagreements <- sum(!is.finite(supplied) | abs(supplied - z) > 1e-8 * pmax(1, abs(z)))
  df$metaUI__es_z <- z
  attr(df, "metaUI_upload_derivations") <- list(z_reference_mean = centre, z_reference_sd = spread,
    supplied_z_disagreements = disagreements, recomputed_rows = sum(recomputed), added_ids = sum(added),
    rule = if (degenerate) "Built SD unavailable/zero: descriptive z=0, outlier discrimination unavailable" else "Descriptive z=(fitting effect-built mean)/built SD; saved scale preserved")
  df
}

metaUI_pcurve_data <- function(df, selection = "first", require_N = FALSE) {
  metaUI_validate_prepared(df)
  if (!nrow(df)) stop("No eligible rows remain for p-curve.")
  # The curve and its tests use Wald z statistics (Fisher z for correlations); only the effect
  # estimate is on the d scale, so only it needs standardized mean differences.
  if (require_N && df$metaUI__es_type[1] != "SMD") stop("P-curve effect estimation is available only for standardized mean differences.")
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

metaUI_forest_rows <- function(df, estimates, models = NULL, primary = NULL) {
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
  # The authors' primary model leads, as it does on the Summary, and sets the reference line.
  chosen <- which((summary_rows | estimates$Model %in% primary) & estimates$status == "ok")
  summaries <- estimates[chosen[order(!estimates$Model[chosen] %in% primary)], ]
  if (nrow(summaries)) rows <- rbind(rows, data.frame(label = summaries$Model,
    kind = "Model summary", study = NA_character_, effect_id = NA_character_,
    fit_es = summaries$fit_es, fit_LCL = summaries$fit_LCL, fit_UCL = summaries$fit_UCL,
    es = summaries$es, LCL = summaries$LCL, UCL = summaries$UCL))
  rows$row <- rev(seq_len(nrow(rows)))
  # Kept apart from the columns, which are also the plot data download.
  if ("interval_type" %in% names(summaries)) attr(rows, "interval_types") <- stats::setNames(summaries$interval_type, summaries$Model)
  rows
}

# The forest rows of the given studies, in that order, with the model summaries.
metaUI_forest_subset <- function(rows, order) {
  effects <- rows[rows$kind == "Selected effect" & rows$study %in% order, , drop = FALSE]
  effects <- effects[order(match(effects$study, order), seq_len(nrow(effects))), , drop = FALSE]
  rbind(effects, rows[rows$kind == "Model summary", , drop = FALSE])
}
# Pages for the PDF download: whole studies, in the order on screen, up to per_page effects on a page
# (a study with more effects has a page to itself).
metaUI_forest_chunks <- function(rows, order, per_page) {
  counts <- as.vector(table(factor(rows$study[rows$kind == "Selected effect"], levels = order)))
  page <- integer(length(order)); current <- 1L; used <- 0L
  for (i in seq_along(order)) {
    if (used > 0L && used + counts[i] > per_page) { current <- current + 1L; used <- 0L }
    page[i] <- current; used <- used + counts[i]
  }
  lapply(split(order, page), function(studies) metaUI_forest_subset(rows, studies))
}

# label_width (characters) wraps study labels onto at most two lines for narrow (phone)
# plots. Longer labels lose their middle, so the end (year, effect) still tells rows apart.
# top_axis repeats the x axis above plots taller than a screen.
# axis_title names the effect size as on screen; without it the axis gives the scale.
# capacity: rows a page has room for, so that a short last page keeps the spacing of the others.
# limits: the x range, so that the pages of one download share their axis.
metaUI_forest_plot <- function(rows, scale, colours = metaUI_plot_colours(), label_width = NULL, top_axis = FALSE, family = colours$family, axis_title = NULL, capacity = NULL, limits = NULL) {
  # As on screen: model summaries first, and an effect is named by its study, with its number
  # only where the study has several.
  rows <- rbind(rows[rows$kind == "Model summary", , drop = FALSE], rows[rows$kind != "Model summary", , drop = FALSE])
  capacity <- max(nrow(rows), capacity)
  rows$row <- rev(seq_len(nrow(rows))) + capacity - nrow(rows)
  if (!is.null(rows$study)) {
    effect <- rows$kind == "Selected effect" & !is.na(rows$study)
    several <- rows$study %in% rows$study[effect][duplicated(rows$study[effect])]
    rows$label[effect] <- ifelse(several[effect],
      paste0(rows$study[effect], ", effect ", substring(rows$label[effect], nchar(rows$study[effect]) + 3L)), rows$study[effect])
  }
  # The estimate and its interval, as in the screen's columns.
  digits <- metaUI_digits(rows$LCL, rows$UCL)
  fmt <- function(x) { x <- formatC(x, format = "f", digits = digits); if (nzchar(family)) metaUI_minus(x) else x }
  values <- ifelse(is.finite(rows$es), paste0(fmt(rows$es), " [", fmt(rows$LCL), ", ", fmt(rows$UCL), "]"), "")
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
  # As on screen, a diamond spans the interval with its tallest point at the estimate.
  diamonds <- do.call(rbind, lapply(seq_len(nrow(summaries)), function(i) data.frame(id = i,
    x = c(summaries$LCL[i], summaries$es[i], summaries$UCL[i], summaries$es[i]), y = summaries$row[i] + c(0, .32, 0, -.32))))
  ggplot2::ggplot(rows, ggplot2::aes(x = .data$es, y = .data$row)) +
    ggplot2::geom_rect(data = bands, ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = .data$ymin, ymax = .data$ymax), inherit.aes = FALSE, fill = colours$shade, colour = NA) +
    ggplot2::geom_vline(xintercept = 0, colour = colours$slate, linewidth = .4) +
    {if (nrow(summaries)) ggplot2::geom_hline(yintercept = min(summaries$row) - .5, colour = colours$ink, linewidth = .4)} +
    ggplot2::geom_segment(data = effects, ggplot2::aes(x = .data$LCL, xend = .data$UCL, yend = .data$row), colour = colours$ink, linewidth = .4) +
    ggplot2::geom_point(data = effects, shape = 16, size = 1.9, colour = colours$ink) +
    {if (nrow(summaries)) ggplot2::geom_polygon(data = diamonds, ggplot2::aes(x = .data$x, y = .data$y, group = .data$id), inherit.aes = FALSE, fill = colours$accent)} +
    ggplot2::scale_y_continuous(breaks = rows$row, labels = rows$label, limits = c(1, capacity), expand = ggplot2::expansion(add = .8),
      sec.axis = ggplot2::dup_axis(labels = values, name = NULL)) +
    ggplot2::scale_x_continuous(limits = limits, sec.axis = if (top_axis) ggplot2::dup_axis() else ggplot2::waiver(),
      # The PDF device's default font has no true minus sign.
      labels = function(x) { x <- format(x, trim = TRUE, drop0trailing = TRUE); if (nzchar(family)) metaUI_minus(x) else x }) +
    ggplot2::labs(x = if (is.null(axis_title)) paste("Effect size (", scale, ")", sep = "") else metaUI_label_expr(axis_title), y = NULL) +
    metaUI_plot_theme(colours, family) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(colour = colours$ink, hjust = 0, size = if (is.null(label_width)) 11.5 else 10.5, lineheight = .9),
      axis.text.y.right = ggplot2::element_text(colour = colours$ink, hjust = 0, size = if (is.null(label_width)) 11 else 10),
      panel.grid.major.y = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())
}

# Descriptive containment of each model's 90% interval, without refitting or assuming
# custom-model coverage. A 90% CI inside the bounds is the conventional pair of one-sided
# tests at .05 (TOST). Equality at a bound is deliberately inconclusive.
metaUI_interval_assessment <- function(estimates, bound = NULL, scale = "SMD", known_models = character()) {
  if (!is.null(bound) && (length(bound) != 1L || !is.numeric(bound) || !is.finite(bound) ||
      bound <= 0 || (scale == "r" && bound >= 1)))
    stop(if (scale == "r") "Enter a bound strictly between 0 and 1 for r." else "Enter a positive finite bound.")
  lower <- if ("LCL90" %in% names(estimates)) estimates$LCL90 else rep(NA_real_, nrow(estimates))
  upper <- if ("UCL90" %in% names(estimates)) estimates$UCL90 else rep(NA_real_, nrow(estimates))
  valid <- estimates$status == "ok" & is.finite(lower) & is.finite(upper) & lower <= upper
  threshold <- rep(NA_real_, nrow(estimates))
  threshold[valid] <- pmax(abs(lower[valid]), abs(upper[valid]))
  assessment <- rep("Not assessed", nrow(estimates))
  assessment[valid] <- "No bound chosen"
  if (!is.null(bound)) {
    assessment[valid] <- "Inconclusive"
    assessment[valid & lower > -bound & upper < bound] <- "Interval within bounds"
    assessment[valid & lower > bound] <- "Exceeds positive bound"
    assessment[valid & upper < -bound] <- "Exceeds negative bound"
  }
  bayesian <- if ("interval_type" %in% names(estimates)) grepl("credible", estimates$interval_type) else rep(FALSE, nrow(estimates))
  data.frame(Model = estimates$Model, bound = rep(if (is.null(bound)) NA_real_ else bound, nrow(estimates)),
    scale = rep(scale, nrow(estimates)), assessment = assessment, threshold = threshold,
    interval = ifelse(estimates$status != "ok", "Not estimated", ifelse(!valid, "No 90% interval", ifelse(bayesian, "90% central credible interval (prior-dependent)",
      ifelse(estimates$Model %in% known_models, "90% CI", "Author-defined 90% interval; error rate unknown")))))
}
