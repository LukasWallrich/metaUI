#' Choose a generated app's design
#'
#' Create settings for [generate_shiny()] or [design_app()]. Colours must be
#' opaque six-digit hex values. Text and accent colours must have at least 4.5:1
#' contrast against the page, panels and derived tints. The default font is the
#' open-licensed Source Sans and Source Serif pair, which ships inside each app.
#' The other fonts come from each reader's device, use fallbacks and may be
#' substituted on other platforms.
#' @param accent Colour for actions, links and primary estimates.
#' @param background Page background colour.
#' @param surface Panel background colour.
#' @param text Main text colour.
#' @param font One of `source`, `system`, `arial`, `verdana` or `georgia`.
#' @param text_size Base interface text size in pixels, from 14 to 20.
#' @param spacing `comfortable` or `compact`.
#' @param radius Corner radius of buttons and fields in pixels, from 0 to 16.
#' @return A named list of validated design settings.
#' @export
#' @examples
#' design <- app_design(accent = "#69429A", font = "georgia", radius = 4)
#' # generate_shiny(dataset, "My analysis", design = design)
app_design <- function(accent = "#1D5C48", background = "#FFFFFF",
                       surface = "#FFFFFF", text = "#1A2530",
                       font = "source", text_size = 16,
                       spacing = "comfortable", radius = 3) {
  colours <- list(accent = accent, background = background, surface = surface, text = text)
  for (key in names(colours)) {
    value <- colours[[key]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || !grepl("^#[0-9a-fA-F]{6}$", value))
      stop(key, " must be a six-digit hex colour, such as #096B74.")
  }
  ratios <- design_contrasts(colours)
  if (any(ratios < 4.5)) {
    failing <- which.min(ratios)
    stop(names(ratios)[failing], " needs at least 4.5:1 contrast (currently ", round(ratios[failing], 2), ":1).")
  }
  if (!is.character(font) || length(font) != 1L || is.na(font) || !font %in% names(design_fonts()))
    stop("font must be source, system, arial, verdana or georgia.")
  if (!is.character(spacing) || length(spacing) != 1L || is.na(spacing) || !spacing %in% c("comfortable", "compact"))
    stop("spacing must be comfortable or compact.")
  for (key in c("text_size", "radius")) {
    value <- get(key)
    bounds <- if (key == "text_size") c(14, 20) else c(0, 16)
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value) || value < bounds[1] || value > bounds[2])
      stop(key, " must be a finite number from ", bounds[1], " to ", bounds[2], ".")
  }
  c(lapply(colours, toupper), list(font = font, text_size = as.numeric(text_size), spacing = spacing, radius = as.numeric(radius)))
}

design_fonts <- function() c(source = '"Source Sans 3", "Segoe UI", system-ui, -apple-system, Roboto, Arial, sans-serif',
  system = 'system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Arial, sans-serif',
  arial = 'Arial, Helvetica, "Liberation Sans", sans-serif', verdana = 'Verdana, Geneva, "DejaVu Sans", sans-serif',
  georgia = 'Georgia, "Times New Roman", "Liberation Serif", serif')

design_palettes <- function() lapply(c(forest = "#1D5C48", teal = "#096B74", blue = "#235A97", plum = "#69429A"), function(accent)
  list(accent = accent, background = "#FFFFFF", surface = "#FFFFFF", text = "#1A2530"))

design_palette_name <- function(design) {
  palettes <- design_palettes()
  matches <- vapply(palettes, function(palette) identical(unlist(design[names(palette)]), unlist(palette)), logical(1))
  if (any(matches)) names(palettes)[which(matches)[1]] else "custom"
}

design_contrast <- function(a, b) {
  luminance <- function(x) {
    rgb <- as.numeric(grDevices::col2rgb(x)) / 255
    sum(ifelse(rgb <= .04045, rgb / 12.92, ((rgb + .055) / 1.055)^2.4) * c(.2126, .7152, .0722))
  }
  values <- sort(c(luminance(a), luminance(b)))
  (values[2] + .05) / (values[1] + .05)
}

design_mix <- function(a, b, weight) {
  rgb <- grDevices::col2rgb(a) * weight + grDevices::col2rgb(b) * (1 - weight)
  grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)
}

design_contrasts <- function(colours) {
  bases <- c(surface = colours$surface, background = colours$background,
    `panel tint` = design_mix(colours$accent, colours$surface, .06),
    `table shading` = design_mix(colours$text, colours$surface, .08))
  values <- unlist(lapply(c("text", "accent"), function(key) {
    stats::setNames(vapply(bases, function(base) design_contrast(colours[[key]], base), numeric(1)), paste(key, "against", names(bases)))
  }))
  values
}

validate_design <- function(design) {
  if (is.null(design)) return(app_design())
  if (!is.list(design) || (length(design) && (is.null(names(design)) || anyNA(names(design)) || any(!nzchar(names(design))) || anyDuplicated(names(design)))))
    stop("design must be a named list, such as the result of app_design().")
  if (length(setdiff(names(design), names(formals(app_design))))) stop("Unknown design settings: ", paste(setdiff(names(design), names(formals(app_design))), collapse = ", "))
  do.call(app_design, design)
}

# Shared by the helper preview and generated apps. Only validated values enter CSS.
# metaui.css draws everything from these tokens, so overriding them restyles the app.
design_css <- function(design) {
  d <- validate_design(design)
  serif <- if (d$font == "source") 'var(--metaui-serif)' else 'inherit'
  tokens <- c(ink = d$text, slate = design_mix(d$text, d$surface, .78), accent = d$accent,
    background = d$background, panel = d$surface,
    attention = d$accent, `attention-bg` = design_mix(d$accent, d$surface, .08),
    rule = design_mix(d$text, d$surface, .18), surface = design_mix(d$text, d$surface, .045),
    tint = design_mix(d$accent, d$surface, .1), bars = design_mix(d$text, d$surface, .32),
    hover = design_mix(d$accent, d$text, .75), button = if (design_contrast(d$accent, "#FFFFFF") >= 4.5) "#FFFFFF" else "#000000",
    sans = design_fonts()[[d$font]], size = paste0(d$text_size, "px"), radius = paste0(d$radius, "px"),
    row = if (d$spacing == "compact") "5px" else "9px")
  variables <- paste(sprintf("--metaui-%s:%s;", names(tokens), tokens), collapse = "")
  paste0(":root{", variables, "}\n",
    'h2,h3,.modal-title,.metaui-primary-value,.metaui-breakdown-item h4,.metaui-empty .metaui-empty-title{font-family:', serif, ';}\n',
    if (d$spacing == "compact") '.well{padding:16px;}.well .shiny-input-container{margin-bottom:12px;}\n')
}

design_r_code <- function(design) {
  d <- validate_design(design)
  args <- vapply(names(d), function(key) paste0("  ", key, " = ", paste(deparse(d[[key]]), collapse = "")), character(1))
  paste0("design <- metaUI::app_design(\n", paste(args, collapse = ",\n"), "\n)\n")
}
