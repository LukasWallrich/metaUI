#' Choose a generated app's design
#'
#' Create settings for [generate_shiny()] or [design_app()]. Colours must be
#' opaque six-digit hex values. Text and accent colours must have at least 4.5:1
#' contrast against the page, panels and derived tints. Fonts come from each
#' reader's device and use fallbacks; no author-machine font files are needed.
#' Named fonts may be substituted on other platforms.
#' @param accent Colour for actions, links and primary estimates.
#' @param background Page background colour.
#' @param surface Panel background colour.
#' @param text Main text colour.
#' @param font One of `system`, `arial`, `verdana` or `georgia`.
#' @param text_size Base interface text size in pixels, from 14 to 20.
#' @param spacing `comfortable` or `compact`.
#' @param radius Panel corner radius in pixels, from 0 to 16.
#' @return A named list of validated design settings.
#' @export
#' @examples
#' design <- app_design(accent = "#69429A", font = "georgia", radius = 4)
#' # generate_shiny(dataset, "My analysis", design = design)
app_design <- function(accent = "#096B74", background = "#EDF3F7",
                       surface = "#FFFFFF", text = "#182F46",
                       font = "system", text_size = 15,
                       spacing = "comfortable", radius = 10) {
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
    stop("font must be system, arial, verdana or georgia.")
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

design_fonts <- function() c(system = 'system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Arial, sans-serif',
  arial = 'Arial, Helvetica, "Liberation Sans", sans-serif', verdana = 'Verdana, Geneva, "DejaVu Sans", sans-serif',
  georgia = 'Georgia, "Times New Roman", "Liberation Serif", serif')

design_palettes <- function() lapply(c(teal = "#096B74", blue = "#235A97", plum = "#69429A"), function(accent)
  list(accent = accent, background = "#EDF3F7", surface = "#FFFFFF", text = "#182F46"))

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
design_css <- function(design) {
  d <- validate_design(design)
  tokens <- c(ink = d$text, slate = d$text, accent = d$accent,
    background = d$background, panel = d$surface,
    attention = d$accent, `attention-bg` = design_mix(d$accent, d$surface, .06),
    rule = design_mix(d$text, d$surface, .18), surface = design_mix(d$text, d$surface, .045),
    tint = design_mix(d$accent, d$surface, .06), tabs = design_mix(d$text, d$surface, .08),
    hover = design_mix(d$accent, d$text, .75), button = if (design_contrast(d$accent, "#FFFFFF") >= 4.5) "#FFFFFF" else "#000000")
  variables <- paste(sprintf("--metaui-%s:%s;", names(tokens), tokens), collapse = "")
  small <- max(13, d$text_size - 1)
  compact <- d$spacing == "compact"
  paste0(":root{", variables, "}\n",
    'body{background:var(--metaui-background);font-family:', design_fonts()[[d$font]], ';font-size:', d$text_size, 'px;}\n',
    'h1,h2,h3,h4,h5,h6,.btn,.form-control,.irs{font-family:inherit;}\n',
    '.well,.tab-content,.metaui-header{background:var(--metaui-panel);}\n',
    '.well{border-radius:', d$radius, 'px;padding:', if (compact) 16 else 23, 'px;}\n',
    '.tab-content{border-radius:0 0 ', d$radius, 'px ', d$radius, 'px;}\n',
    '.btn,.form-control,.metaui-primary,#effectestimate_box{border-radius:', min(d$radius, 6), 'px;}\n',
    '.btn,.table,.metaui-help,.well .control-label,.nav-tabs>li>a{font-size:', small, 'px;}\n',
    '.metaui-note,.metaui-help-text,#calculation_status{font-size:', max(12, d$text_size - 2), 'px;}\n',
    '.metaui-actions #go{font-size:', d$text_size + 1, 'px;}\n',
    '.well .shiny-input-container{margin-bottom:', if (compact) 12 else 22, 'px;}\n',
    '.table>tbody>tr>td,.table>thead>tr>th{padding:', if (compact) '6px 8px' else '11px 10px', ';}\n',
    '.metaui-primary,.metaui-basis,.table>tbody>tr:hover{background:var(--metaui-tint);}\n',
    '.metaui-primary{border-color:var(--metaui-rule);border-left-color:var(--metaui-accent);}\n',
    '.metaui-primary-label,.metaui-primary-value,.metaui-selection-details>summary,.nav-tabs>li.active>a,.metaui-help>summary:hover{color:var(--metaui-accent);}\n',
    '.nav-tabs{background:var(--metaui-tabs);}.nav-tabs>li>a{background:transparent;}.nav-tabs>li>a,.table>thead>tr>th,#calculation_status{color:var(--metaui-ink);}\n',
    '.nav-tabs>li.active>a,.nav-tabs>li.active>a:hover,.nav-tabs>li.active>a:focus{background:var(--metaui-panel);color:var(--metaui-accent);}\n',
    '.nav-tabs>li>a:hover,.table>thead>tr>th,.table-striped>tbody>tr:nth-of-type(odd){background:var(--metaui-surface);}\n',
    '.metaui-actions #resetFilters,.metaui-export-actions .btn,.metaui-share .btn{background:var(--metaui-panel);color:var(--metaui-accent);border-color:var(--metaui-rule);}\n',
    '.btn-primary{color:var(--metaui-button);}.btn-primary:hover,.btn-primary:focus{color:var(--metaui-button);background:var(--metaui-hover);border-color:var(--metaui-hover);}\n',
    '.btn-primary.metaui-needs-run{background:var(--metaui-attention);border-color:var(--metaui-attention);color:var(--metaui-button);outline:2px dashed var(--metaui-accent);outline-offset:3px;}\n',
    '.metaui-help>summary:hover{background:var(--metaui-tint);}.table>tbody>tr>td{border-color:var(--metaui-rule);}\n',
    'body table.dataTable tbody tr{background:var(--metaui-panel);color:var(--metaui-ink);}body .dataTables_wrapper .dataTables_length,body .dataTables_wrapper .dataTables_filter,body .dataTables_wrapper .dataTables_info{color:var(--metaui-ink);}body .dataTables_wrapper input,body .dataTables_wrapper select{background:var(--metaui-panel);color:var(--metaui-ink);border-color:var(--metaui-rule);}\n',
    '.modal-content,.shiny-notification{background:var(--metaui-panel);color:var(--metaui-ink);border-color:var(--metaui-rule);}.modal-header,.modal-footer{border-color:var(--metaui-rule);}.modal .close{color:var(--metaui-ink);opacity:1;text-shadow:none;}\n',
    '.form-control,.selectize-input,.selectize-input.full,.selectize-input.input-active,.selectize-dropdown{background:var(--metaui-panel);color:var(--metaui-ink);border-color:var(--metaui-rule);}.form-control[disabled],.form-control[readonly]{background:var(--metaui-surface);color:var(--metaui-ink);}.form-control::placeholder{color:var(--metaui-slate);opacity:1;}.selectize-input input{color:var(--metaui-ink)!important;}.selectize-dropdown .active,.selectize-dropdown .selected{background:var(--metaui-tint);color:var(--metaui-ink);}.selectize-control.single .selectize-input:after{border-top-color:var(--metaui-ink);}\n',
    '.btn-default:not(.btn-primary),.metaui-data-tools .btn:not(.btn-primary),.modal-footer .btn:not(.btn-primary){background:var(--metaui-panel);color:var(--metaui-accent);border-color:var(--metaui-rule);}.btn-default:not(.btn-primary):hover,.btn-default:not(.btn-primary):focus,.metaui-data-tools .btn:not(.btn-primary):hover{background:var(--metaui-tint);color:var(--metaui-accent);border-color:var(--metaui-accent);}\n',
    'body table.dataTable thead th,body table.dataTable thead td,body table.dataTable tfoot th,body table.dataTable tfoot td{background:var(--metaui-surface);color:var(--metaui-ink);border-color:var(--metaui-rule);}body .dataTables_wrapper .dataTables_paginate .ellipsis{color:var(--metaui-ink);}\n',
    'body table.dataTable.stripe tbody tr.odd,body table.dataTable.display tbody tr.odd,body table.dataTable.display tbody tr.odd>.sorting_1,body table.dataTable.display tbody tr.even>.sorting_1{background:var(--metaui-surface);color:var(--metaui-ink);}\n',
    'body table.dataTable thead>tr>th.sorting,body table.dataTable thead>tr>th.sorting_asc,body table.dataTable thead>tr>th.sorting_desc{background-image:none!important;position:relative;padding-right:24px;}body table.dataTable thead>tr>th.sorting:after,body table.dataTable thead>tr>th.sorting_asc:after,body table.dataTable thead>tr>th.sorting_desc:after{position:absolute;right:7px;top:50%;bottom:auto;transform:translateY(-50%);color:var(--metaui-ink);opacity:1;}body table.dataTable thead>tr>th.sorting:before,body table.dataTable thead>tr>th.sorting_asc:before,body table.dataTable thead>tr>th.sorting_desc:before{display:none;}body table.dataTable thead>tr>th.sorting:after{content:"\\2195";}body table.dataTable thead>tr>th.sorting_asc:after{content:"\\2191";}body table.dataTable thead>tr>th.sorting_desc:after{content:"\\2193";}\n',
    'body .dataTables_wrapper .dataTables_paginate .paginate_button,body .dataTables_wrapper .dataTables_paginate .paginate_button.disabled,body .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover,body .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:focus{color:var(--metaui-ink)!important;background:var(--metaui-panel)!important;border-color:var(--metaui-rule)!important;}body .dataTables_wrapper .dataTables_paginate .paginate_button.current,body .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover,body .dataTables_wrapper .dataTables_paginate .paginate_button.current:focus,body .dataTables_wrapper .dataTables_paginate .paginate_button:hover,body .dataTables_wrapper .dataTables_paginate .paginate_button:focus{color:var(--metaui-accent)!important;background:var(--metaui-tint)!important;border-color:var(--metaui-accent)!important;}\n',
    '#effectestimate_box{scrollbar-color:var(--metaui-slate) var(--metaui-background);}\n',
    '.irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single{color:var(--metaui-button);}\n')
}

design_r_code <- function(design) {
  d <- validate_design(design)
  args <- vapply(names(d), function(key) paste0("  ", key, " = ", paste(deparse(d[[key]]), collapse = "")), character(1))
  paste0("design <- metaUI::app_design(\n", paste(args, collapse = ",\n"), "\n)\n")
}
