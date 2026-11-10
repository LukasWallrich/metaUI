# Wrap in function so that it can be saved more easily

labels_and_options <- function(dataset_name, correlation = .6, primary_model = NULL, bayesian = FALSE, scale = "SMD", custom_models = FALSE, model_descriptions = character(), effects = NA, studies = NA, strip = "", citation = "", intro = "", random_default = FALSE) {
  glue::glue('

      # TK - defaults? Advanced options?
      aggregation_method <- c("aggregate", "first")
      correlation_dependent <- {correlation}

      ci_width <- "95%"

      # Name of the authors\' primary model, exactly as in models.R, or NULL for none.
      # It is shown first; other rows are labelled explorations. Never substituted.
      primary_model <- {if (is.null(primary_model)) "NULL" else paste(deparse(primary_model), collapse = "")}

      # Fixed texts
      # Optional introduction shown under the title; leave empty for none.
      welcome_text <- HTML({paste(deparse(intro), collapse = "\n")})
      dataset_name <- {paste(deparse(as.character(dataset_name)), collapse = "\n")}
      not_analysed <- HTML(paste0("<p class=\'metaui-empty-title\'>{effects} effects from {studies} studies</p>", {paste(deparse(strip), collapse = "\n")}, "<p class=\'metaui-fitting\'>Each dot is one effect. Fitting the models&hellip;</p>", {paste(deparse(if (nzchar(citation)) paste0("<p class=\'metaui-note\'>", citation, "</p>") else ""), collapse = "\n")}))

      fitting_note <- HTML("<p class=\'metaui-fitting\'>Fitting the models&hellip;</p>")

      summary_overview_main <- HTML("<h3>Selected sample</h3>")
      summary_table_main <- HTML("<h3>Effect size estimates</h3>")
       # Confidence level needs to be changed in all entries in model.R if you want to adjust it
      summary_table_notes <- HTML(glue::glue("<p>CI = <<ci_width>> confidence interval; its actual coverage depends on each model\\\'s assumptions. <i>k</i> is the number of effects or studies the model reports.{if (bayesian) " Bayesian rows show central credible intervals." else ""}</p>", .open = "<<", .close = ">>"))
      model_help <- HTML("<h4>Default models</h4>
        {metaUI_default_model_help(random_default)}
        <dt>Trim-and-fill</dt><dd>What would the average be if the funnel plot were symmetric? Studies are added as mirror images on the sparse side and the average is estimated again. The side to fill is chosen from the funnel plot&rsquo;s asymmetry (the sign of Egger&rsquo;s test), not from the direction the authors expected. Asymmetry can have causes other than publication bias. <span class=\'metaui-spec\'>Duval-Tweedie L0 estimator with common-effect trimming; random-effects model (ML) with Hartung-Knapp intervals. <i>k</i> includes the added studies.</span></dd>
        <dt>P-uniform* and the Hedges-Vevea selection model</dt><dd>What would the average be if significant results in the expected direction were more likely to be published? Both model this selection directly, so they need the direction the authors expected and are reported as unsupported without one. <span class=\'metaui-spec\'>puniform::puni_star (ML); weightr::weightfunct with one step at one-sided <i>p</i> = .025 and random effects.</span></dd>
        <dt>Precision Effect Test (PET); PEESE</dt><dd>What average effect would a study with no sampling error show, extending the link between effect size and its standard error (PET) or variance (PEESE)? <span class=\'metaui-spec\'>Weighted regressions of study-level effects on their standard error or variance; the intercept is reported.</span> Both rows are shown. The table also marks the conditional PET-PEESE estimate (<a href=\'https://doi.org/10.1002/jrsm.1095\'>Stanley &amp; Doucouliagos, 2014</a>): PEESE when PET&rsquo;s intercept differs from zero, PET otherwise. Following <a href=\'https://doi.org/10.1177/1948550617693062\'>Stanley (2017)</a>, the test is one-sided at the 10% level in the direction the authors expected, or two-sided at the 10% level when they stated none. The rule performs poorly when there are few studies, when all studies are small, or when effects vary a lot between studies (Stanley, 2017).</dd>{if (bayesian) "
        <dt>Bayesian normal-normal model</dt><dd>What is the average effect, given the studies and a stated prior belief about how much true effects differ between studies? <span class=\'metaui-spec\'>One combined effect per study; flat prior on the average and half-normal prior on the between-study SD (its scale is on the Bayesian sensitivity tab). The posterior is computed by numerical integration over the between-study SD (2,000-point grid; the average given that SD is exactly normal). The estimate is the posterior median; the interval is the central 95% credible interval, which depends on the prior.</span></dd>" else ""}
        </dl>{metaUI_custom_model_help(custom_models, model_descriptions)}")

      sample_overview_main <- HTML("<h3>Sample breakdown</h3>")
      sample_table <- HTML("<h3>Effects and studies</h3>")
      study_details_hint <- HTML("<p class=\'metaui-note\'>Select a study name for its reference and, where the authors could include it, its abstract.</p>")
      sample_moderation_main <- HTML("<h3>Simple tests of moderation</h3>")
      sample_moderation_notes <- HTML("<p class=\'metaui-note\'>One moderator at a time; correlations between moderators are ignored. Each moderator is added to the primary model where that is a multilevel metafor model, and otherwise to the random-effects {if (random_default) "" else "multilevel "}model; tests and intervals are cluster-robust by study (CR2, with small-sample degrees of freedom), so they allow for dependent effects. The overall test uses the matching small-sample correction (HTZ); with many categories that hold few studies it has little power. These are exploratory tests without adjustment for multiple testing: trying many moderators or selections makes some false positives likely. <i>k</i> = number of effects.</p>")

      pcurve_selection <- HTML("<p class=\'metaui-note\'>P-curve uses the first effect per study, an exploratory selection rule applied before direction screening. It excludes and counts effects that are zero or opposite to the expected direction, and uses normal Wald statistics, not the source p-values.{if (scale == "ZCOR") " For correlations these are computed on Fisher&rsquo;s z." else if (scale %in% c("OR", "RR")) " For ratios these are computed on the log scale, and effects at no difference (a ratio of 1) count as zero." else ""}</p>")
      zcurve_selection <- HTML("<p class=\'metaui-note\'>Z-curve uses the first effect per study, an exploratory selection rule, and normal Wald statistics, not the source p-values.</p>")

      qrppb_main <- HTML("<h3>Publication bias and selective reporting</h3><p class=\'metaui-note\'>Exploratory small-study and selection diagnostics. A non-significant result does not show that there is no bias: these tests often have little power.</p>")
      funnel_main <- HTML("<h4>Funnel plot of study-level effects</h4>")

      eggers_main <- HTML("<h4>Egger\'s test of funnel plot asymmetry</h4>")

      pcurve_main <- HTML("<h4>P-curve of effects</h4>")

      zcurve_main <- HTML("<h4>Z-curve of effects</h4>")

      diagnostics_main <- HTML("<h3>Distribution of effect sizes</h3><p class=\'metaui-note\'>Dots mark effects more than 1.5 interquartile ranges (IQR) beyond the middle 50%. Hover over or tap a dot for its study and <i>z</i>-score; the list below the plot gives them all.</p>")
      diagnostics_het <- HTML("<h3>Heterogeneity (REML {if (random_default) "random-effects" else "multilevel"} model)</h3>")

      forest_note <- HTML("<p class=\'metaui-note\'>Effects: observed, with normal 95% sampling intervals and no shrinkage. Combined rows: the study-level effects used by the study-level models (see Methods); with correlated effects of unequal precision they can lie away from the simple average. Summaries: the primary model, where the authors chose one, with multilevel and robust variance estimation (RVE).<span class=\'metaui-forest-numbering\'> Effects keep their numbers from the full dataset, so a selection can leave gaps.</span>{if (scale == "ZCOR") " Correlations are shown as r; the plot data also include fitting-scale values." else if (scale %in% c("OR", "RR")) " Ratios are drawn on a log scale, around 1, so that each interval is symmetric around its estimate; the plot data also include the log values the models use." else ""}</p>")
      zscore_help <- HTML("<p class=\'metaui-help-text\'>z = (effect \\u2212 mean of all effects in the full dataset) / their SD, on the fitting scale. This is a simple standardization, not a model-based outlier test; Outlier diagnostics gives the values used.</p>")
      # Result tabs show not_analysed until the first analysis has finished. A tab with
      # precomputed content (opening = TRUE) shows it from the first paint instead.
      # Only the Summary tab carries the strip of effects, so the page holds it once.
      results_panel <- function(..., opening = FALSE, placeholder = fitting_note) if (opening) tagList(...) else tagList(
        conditionalPanel("!output.metaui_ready", div(class = "metaui-empty", role = "status", placeholder)),
        conditionalPanel("output.metaui_ready && output.no_effects", div(class = \'metaui-empty\', role = \'status\', p(class = \'metaui-empty-title\', \'No effects match this selection\'), p(\'Change the filters, or reset them to select every effect again.\'))),
        conditionalPanel("output.metaui_ready && !output.no_effects", ...))
      data_help <- HTML("<p class=\'metaui-help-text\'>The workbook holds all rows (not only the selection), the last applied filters and the last results.{if (bayesian) " With Bayesian models, it waits for two extra prior-sensitivity fits." else ""}<br/>Upload is for reanalysis: edit a downloaded workbook and upload it to analyze your version. Its first sheet, instructions, marks the columns you can edit; the others are recomputed from them. Rows may be edited, added or removed, and new categories join their filter. Saved filters and the equivalence bound are restored and analyzed.</p><p class=\'metaui-help-text\'>The R script and Quarto file reproduce the applied selection and its model rows from the workbook of the same analysis, without metaUI. They do not reproduce the plots or diagnostic tabs.</p>")


  ')
}



# Methods text for models the authors added: their own descriptions, then a pointer for the rest.
# Escaped for an HTML string inside a double-quoted R string in labels_and_options.R.
# The opening of Methods' list of default models: the multilevel model and RVE, or, with one
# effect per study, the standard random-effects model that replaces them.
metaUI_default_model_help <- function(random_default = FALSE) {
  if (random_default) return(paste0("<p>The models make different assumptions, so their estimates need not agree. The first estimates the average effect from every selected effect. ",
    "The others explore possible publication or reporting bias, each in its own way. Each entry gives the question first, then the specification.</p><dl>",
    "<dt>Random-Effects Model</dt><dd>What is the average effect, if true effects differ between studies? Each study contributes one effect. ",
    "<span class='metaui-spec'>metafor::rma.uni, REML; Hartung-Knapp interval (a <i>t</i> distribution with <i>k</i> &minus; 1 degrees of freedom and an adjusted standard error).</span></dd>"))
  paste0("<p>The models make different assumptions, so their estimates need not agree. The first two estimate the average effect from every selected effect. ",
    "The others use one combined effect per study to explore possible publication or reporting bias, each in its own way. Each entry gives the question first, then the specification.</p><dl>",
    "<dt>Random-Effects Multilevel Model</dt><dd>What is the average effect, if true effects differ both between studies and between effects within a study? ",
    "<span class='metaui-spec'>metafor::rma.mv, REML; random effects for studies and for effects within studies; sampling errors treated as independent; <i>t</i>-based intervals with containment degrees of freedom (based on the number of studies).</span></dd>",
    "<dt>Robust Variance Estimation</dt><dd>What is the average effect, allowing for unknown correlations between effects from the same study? ",
    "<span class='metaui-spec'>robumeta::robu with correlated-effects weights, which use the within-study correlation given above to estimate between-study variation, and the small-sample correction of Tipton (2015). Below 4 small-sample degrees of freedom its interval is flagged as unreliable.</span></dd>")
}

metaUI_custom_model_help <- function(custom_models, descriptions) {
  if (!custom_models) return("")
  esc <- function(x) gsub("\\\\", "&#92;", htmltools::htmlEscape(x, attribute = TRUE))
  described <- descriptions[!is.na(descriptions) & nzchar(descriptions)]
  paste0(if (length(described)) paste0("<h4>Models added by the authors</h4><dl>", paste0("<dt>", esc(names(described)), "</dt><dd>", esc(described), "</dd>", collapse = ""), "</dl>") else "",
    if (length(described) < length(descriptions) || !length(descriptions)) "<p>Rows not described here were added by the authors; their code is in models.R in the app source.</p>" else "")
}

# Wrapped in function so that it is only created once custom variables are set

generate_ui_filters <- function(data, filter_popups, any_filters, opts = opts) {
  if (!any_filters) return("")
  filter_cols <- colnames(data) %>% stringr::str_subset("metaUI__filter_")

  purrr::map_chr(filter_cols, function(filter_col) {
    add_popup <- stringr::str_remove(filter_col, "metaUI__filter_") %in% names(filter_popups)
    rm_prefix <- "metaUI__filter_"
    help <- if (add_popup) {
      name <- stringr::str_remove(filter_col, rm_prefix)
      # Serialise the complete tag as an R string, preserving author HTML and quotes.
      help_id <- paste0("metaui-help-", match(filter_col, filter_cols))
      tag <- tags$button(type = "button", class = "metaui-filter-help",
        `aria-expanded` = "false", `aria-controls` = help_id,
        `aria-label` = paste("Help for", name), "?")
      panel <- tags$div(id = help_id, class = "metaui-filter-help-panel", hidden = NA,
        filter_popups[[name]])

      paste0("HTML(", paste(deparse(as.character(tag)), collapse = "\n"), ")")
    } else ""

    if (is.numeric(data[[filter_col]])) {
      # Bounds round outward to a tick interval, so the default range retains every row.
      spec <- metaUI_slider_spec(data[[filter_col]])
      out <- glue::glue('
      metaUI_slider_input({metaUI_r_string(metaUI_filter_id(filter_col))},
      tagList({metaUI_r_string(stringr::str_remove(filter_col, "metaUI__filter_"))},
      {help}),
          spec = list(min = {spec$min}, max = {spec$max}, step = {spec$step}, ticks = {spec$ticks})
      )
                 ')

    } else if (is.factor(data[[filter_col]])) {

      # Serialise as R code so levels containing quotes or backslashes stay valid
      levels_ <- levels(data[[filter_col]]) %>% na.omit() %>% as.character()
      choices <- paste(deparse(levels_), collapse = "\n")

      out <- glue::glue('{if (length(levels_) < opts$selection_list_threshold) "checkboxGroupInput(" else "shinyWidgets::pickerInput(multiple = TRUE, options = list(`actions-box` = TRUE, `dropup-auto` = FALSE, `selected-text-format` = \\"count\\", `count-selected-text` = \\"{0} of {1} selected\\", `none-selected-text` = \\"None selected\\"), "} {metaUI_r_string(metaUI_filter_id(filter_col))},
      tagList({metaUI_r_string(stringr::str_remove(filter_col, rm_prefix))},
      {help}),
          choices = {choices},
        selected = {choices}
        )')
    } else {
      stop("Filter/moderator variables must be numeric or factors. This check failed first for ", filter_col)
    }

    if (add_popup) out <- paste0(out, ",\nHTML(",
      paste(deparse(as.character(panel)), collapse = "\n"), ")")

    if (is.numeric(data[[filter_col]])) {
    # Add option to exclude/include NA values if there are any
    if (TRUE) { # Always available for later uploads with missing values
      out <- glue::glue('
          {out},
          metaUI_missing_toggle({metaUI_r_string(paste0(metaUI_filter_id(filter_col), "_include_NA"))}, {sum(is.na(data[[filter_col]]))})
        ')
    }
    }
    out
  }) %>% glue::glue_collapse(sep = ",\n") %>% paste0(",\n")
}


generate_sample_description_ui <- function(data, any_filters) {
  if (!any_filters) return("")

  filter_ids <- colnames(data) %>%
    stringr::str_subset("metaUI__filter_")
  filter_names <- stringr::str_remove(filter_ids, "metaUI__filter_")

  filter_ids <-  metaUI_filter_id(filter_ids)

  numeric <- vapply(colnames(data)[stringr::str_detect(colnames(data), "metaUI__filter_")], function(col) is.numeric(data[[col]]), logical(1))
  items <- purrr::pmap_chr(list(filter_ids, filter_names, numeric), \(fid, fn, is_numeric) {
    if (is_numeric) glue::glue('
            div(class = "metaui-breakdown-item", h4({metaUI_r_string(fn)}), div(class = "metaui-breakdown-numeric",
              tableOutput("summary_{fid}_table"),
              shinycssloaders::withSpinner(plotOutput("summary_{fid}_plot", height = 170))))
                ')
    else glue::glue('
            div(class = "metaui-breakdown-item", h4({metaUI_r_string(fn)}), htmlOutput("summary_{fid}_table"))
                ')
  }) %>% glue::glue_collapse(sep = ",\n")
  paste0('div(class = "metaui-breakdowns",\n', items, "),\n")


}

# A string as R source, for labels written into generated code (quotes and backslashes escaped).
metaUI_r_string <- function(x) paste(deparse(as.character(x)), collapse = "")

generate_moderator_selection <- function(data) {
  filter_cols <- colnames(data) %>%
    stringr::str_subset("metaUI__filter_") %>%
    setNames(., stringr::str_remove(., "metaUI__filter_"))

  filter_vector <- purrr::imap_chr(filter_cols, \(fc, fn) {
    paste0(metaUI_r_string(fn), " = ", metaUI_r_string(fc))
  })  %>% glue::glue_collapse(sep = ", ")

  # Create a selection drop-down with filter_cols as values
  glue::glue('
    selectInput("moderator", "Moderator",
      choices = c({filter_vector}),
      selected = {metaUI_r_string(filter_cols[1])}
    )')
}

metaUI_asset_version <- function(file) substr(unname(tools::md5sum(system.file("template_code", file, package = "metaUI"))), 1, 8)

get_favicon_tag <- function(dataset_name) {
  favicon <- paste(readLines(system.file("template_code", "favicon.svg", package = "metaUI")), collapse = "")
  glue::glue('tagList(
    tags$head(tags$link(rel="icon", href="data:image/svg+xml,{utils::URLencode(favicon, reserved = TRUE)}", type="image/svg+xml")),
    tags$span({paste(deparse(dataset_name), collapse = "\n")}))')

}

generate_mod_tab <- function(data, any_filters) {
  if (!any_filters) return("")
  glue::glue('
    tabPanel(
      "Moderation",
      results_panel(
      sample_moderation_main,
      {generate_moderator_selection(data)},
      uiOutput("moderation_panel"),
      sample_moderation_notes
      )
    ),
    ')
}

generate_ui <- function(data, dataset_name, about, filter_popups, opts = list()) {

  # Check whether data contains any filters
  filter_cols <- colnames(data) %>% stringr::str_subset("metaUI__filter_")
  if (length(filter_cols) == 0) any_filters <- FALSE else any_filters <- TRUE
  # The equivalence bound: symmetric around 0, or for ratios around 1 on the log scale (as R code).
  sesoi_text <- if (metaUI_scale(data$metaUI__es_type[1])$ratio) c('"Smallest ratio of interest, x (bounds 1/x and x):"',
    '"Bounds of 1/x and x, symmetric around 1 on the log scale: 1.25 and 0.8 give the same pair (0.8 to 1.25)."') else
    c('"Smallest effect size of interest: \\u00b1"', '"A symmetric bound, on the scale of the estimates."')

  out <- glue::glue('

  fluidPage(lang = "en",
    theme = shinythemes::shinytheme("{opts$shiny_theme}"),
    # The version in each address makes browsers fetch a changed file rather than reuse a cached one.
    tags$head(tags$link(rel = "stylesheet", href = "metaui.css?v={metaUI_asset_version("metaui.css")}"), tags$script(src = "metaui.js?v={metaUI_asset_version("metaui.js")}")),
    {if (!is.null(opts$design)) \'tags$head(tags$link(rel = "stylesheet", href = "app-design.css")),\' else ""}
    shinyjs::useShinyjs(),
    tags$a(class = "metaui-skip", href = "#metaui_results", "Skip to results"),
    # Application title
    div(class = "metaui-header",
    # The app name is the one top-level heading of the page.
    tags$head(tags$title({paste(deparse(paste0(dataset_name, " - metaUI")), collapse = "\n")})),
    tags$h1(class = "metaui-app-title", {get_favicon_tag(dataset_name)}),
    if (nzchar(welcome_text)) div(class = "metaui-intro", welcome_text)),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$h2(class = "metaui-panel-title", "Selection"),
        # States which selection and data the visible results use, under its heading and above the filters that change it.
        conditionalPanel(if (is.null(metaUI_opening$html)) "output.metaui_ready" else "true",
        div(id = "metaui_basis", class = "metaui-basis", role = "status", `aria-live` = "polite",
          metaUI_opening_output(textOutput("selection_overview"), metaUI_opening$html$selection_overview),
          tags$details(class = "metaui-selection-details", tags$summary("Selection details"), textOutput("selection_status"), textOutput("calculation_status")))),
        # Phones collapse the filters so that results follow closely; metaui.js toggles them.
        tags$button(type = "button", class = "btn metaui-filters-toggle", `aria-expanded` = "false",
          `aria-controls` = "metaui_filter_panel", "Filters and downloads"),
        div(id = "metaui_filter_panel", class = "metaui-filter-panel",
        {if (length(metaUI_computation_specs(data))) paste0(
          \'selectInput("computation", "Effect-size computation", choices = \',
          paste(deparse(metaUI_computations(data)), collapse = "\n"), \', selected = "primary"),\nuiOutput("computation_note"),\') else ""}
        tags$h3(class = "metaui-filters-title", "Filters"),
        div(id = "filters",
        {generate_ui_filters(data, filter_popups, any_filters, opts = opts)}
        tags$details(class = "metaui-help metaui-advanced", tags$summary("Exclude extreme effects"), div(id = "z_score_filter"), zscore_help))),
        div(class = "metaui-actions",
          div(id = "apply_state_box", class = "metaui-apply-state", role = "status", `aria-live` = "polite",
            textOutput("apply_state")),
          shinyjs::disabled(actionButton("go", "Update results", class = "btn-primary", title = "Applies the filters; stays quiet while the results already match them")),
          actionButton("resetFilters", "Reset")),
        uiOutput("share_selection"), textOutput("link_notice"),
        tags$details(class = "metaui-help metaui-data-tools",
        tags$summary("Download data or code, upload data"),
        actionButton("downloadData", "Selected data and results (.xlsx)", icon = icon("download")),
        div(class = "metaui-code-actions", tags$span(class = "metaui-code-label", "Code for the applied selection"),
          actionButton("downloadCode", "R script", icon = icon("download")),
          actionButton("downloadQuarto", "Quarto file", icon = icon("download"))),
        conditionalPanel("false",
          downloadButton("executeDownload", "Execute the download"),
          downloadButton("executeCodeDownload", "Execute the R script download"),
          downloadButton("executeQuartoDownload", "Execute the Quarto download")
        ),
      fileInput("uploadData", "Upload a metaUI .xlsx",
                multiple = FALSE,
                accept = c(".xlsx")
      ),
      actionButton("executeUpload", "Upload and analyze", icon = icon("upload")),
      data_help)
      ),
      mainPanel(
        width = 9,
        div(id = "metaui_results",
        # Shown while the filters differ from the displayed results, or while a link or upload is applied.
        div(id = "metaui_stale_banner", class = "metaui-stale-banner", role = "status",
          textOutput("stale_banner", inline = TRUE),
          tags$button(type = "button", class = "btn btn-default metaui-banner-go", "Update results")),
        # A link that cannot be restored says so above the results, which then show every effect.
        uiOutput("link_banner"),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Summary",
            results_panel(opening = !is.null(metaUI_opening$html), placeholder = not_analysed,
            summary_table_main,
            metaUI_opening_output(uiOutput("primary_estimate"), metaUI_opening$html$primary_estimate),
            conditionalPanel("!output.no_effects",
            div(id = "effectestimate_box", role = "region", `aria-label` = "Model estimates", metaUI_opening_output(htmlOutput("effectestimate"), metaUI_opening$html$effectestimate)),
            tags$details(class = "metaui-help metaui-methods", tags$summary("Methods and assumptions"),
              summary_table_notes,
              uiOutput("scale_note"),
              tags$p("The downloaded summary also holds fitting-scale values and fit times."),
              model_help),
            tags$details(class = "metaui-help metaui-equivalence", tags$summary("Assess practical equivalence"),
            htmltools::tagAppendAttributes(numericInput("sesoi", {sesoi_text[1]},
              value = NULL, min = 0), class = "metaui-sesoi"),
            p(class = "metaui-note", {sesoi_text[2]}, " There is no universal default; justify it substantively. The assessment uses each model\\u2019s 90% interval, not the 95% interval shown elsewhere: a 90% interval inside the bounds is the usual equivalence test, two one-sided tests at .05 (TOST). The last column gives the narrowest bound that each 90% interval lies within."),
            uiOutput("equivalence_verdict"),
            div(class = "metaui-scroll", tableOutput("equivalence")),
            uiOutput("equivalence_note")))
            )
          ),
          tabPanel(
            "Sample",
            results_panel(
            summary_overview_main,
            div(class = "metaui-scroll metaui-sample-note", tableOutput("sample_overview")),
            p(class = "metaui-note", {paste(deparse(paste0(if ("metaUI__article_label" %in% names(data)) "Sources are papers or reports. " else "",
              "Studies are the samples that the models treat as independent of each other. Effects are the individual effect sizes; one study can report several.")), collapse = "")}),
            tags$p(class = "metaui-note", tags$a(href = "#metaui_sample_table", "Go to the table of effects and studies")),
            {if (any_filters) "sample_overview_main, p(class = \\"metaui-note\\", \\"Hollow bars: effects without a value.\\"),"  else ""}
            {generate_sample_description_ui(data, any_filters)}
            div(id = "metaui_sample_table", sample_table),
            if (!is.null(metaUI_study_info)) div(class = "metaui-export-actions metaui-reference-actions",
              tags$span(class = "metaui-export-label", "References for this selection:"),
              downloadButton("references_txt", "Text (.txt)"), downloadButton("references_bib", "BibTeX (.bib)"), downloadButton("references_xlsx", "Excel (.xlsx)")),
            p(class = "metaui-note metaui-key metaui-forest-controls", HTML(metaUI_switch("One row per", "metaui-sample-view", {if ("metaUI__article_label" %in% names(data)) \'c(effects = "effect", studies = "study", sources = "source")\' else \'c(effects = "effect", studies = "study")\'}, "effects"))),
            if (!is.null(metaUI_study_info)) study_details_hint,
            DT::dataTableOutput("sample_table")
            )
          ),
          {if (!is.null(opts$bayesian)) \'tabPanel("Bayesian sensitivity", results_panel(p(class = "metaui-note", "Study-level normal-normal model: flat prior on the mean; half-normal prior on heterogeneity at half, baseline and double its scale. Effects are combined within studies by the within-study correlation the authors set or by the first effect per study. tau is on the fitting scale. Intervals are 95% central credible intervals and depend on the prior."), tableOutput("bayesian_sensitivity"))),\' else ""}
          {if (length(metaUI_computation_specs(data))) \'tabPanel("Effect computations", results_panel(p(class = "metaui-note", "The selected effects under each author-defined computation; multilevel and RVE models only. The z-score filter uses the primary computation."), div(class = "metaui-scroll", tableOutput("computation_comparison")))),\' else ""}
          {generate_mod_tab(data, any_filters)}
          tabPanel("Forest plot", results_panel(div(class = "metaui-forest-tab", tags$h3("Effects by study"), uiOutput("forest_panel"),
            # Readers looking for one study jump to its page, where it is marked.
            div(class = "metaui-forest-find", selectizeInput("forest_find", "Find a study", choices = NULL,
              options = list(placeholder = "Type part of a name", maxOptions = 50)),
              htmltools::tagAppendAttributes(uiOutput("forest_found"), `aria-live` = "polite")),
            shiny::htmlOutput("foreststudies"), forest_note))),
          tabPanel(
            "Publication bias", results_panel(qrppb_main, uiOutput("bias_power_note"), funnel_main, uiOutput("funnel_key"), div(class = "metaui-plot", plotOutput("funnel", width = "100%", height = 400) %>% shinycssloaders::withSpinner()),
            eggers_main, uiOutput("eggers_reading"), div(class = "metaui-scroll", tableOutput("eggers")),
            pcurve_main, pcurve_selection, uiOutput("pcurve_reading"), div(class = "metaui-scroll metaui-plot", plotOutput("pcurve", height = "auto") %>% shinycssloaders::withSpinner()), uiOutput("pcurve_notes"),
            # A warning qualifies the reading, so it comes first.
            zcurve_main, zcurve_selection, div(class = "metaui-note metaui-warning-text", textOutput("zcurve_warnings")), uiOutput("zcurve_reading"), div(class = "metaui-scroll metaui-plot", plotOutput("zcurve", height = "auto") %>% shinycssloaders::withSpinner()),
            uiOutput("zcurve_statistics"))
          ),
          tabPanel(
            "Outlier diagnostics",
            results_panel(
            diagnostics_main,
            div(class = "metaui-plot", plotly::plotlyOutput("violin", height = 400)) %>% shinycssloaders::withSpinner(),
            uiOutput("outlier_list"),
            uiOutput("distribution_note"),
            diagnostics_het,
            uiOutput("heterogeneity_reading"),
            div(class = "metaui-scroll", tableOutput("heterogeneity") %>% shinycssloaders::withSpinner()))
          ),
          tabPanel("About", div(class = "metaui-about", HTML({paste(deparse(as.character(about)), collapse = "\n")})))
        ))
      )
    )
  )
  ')
  out
}


generate_server <- function(metaUI__df, opts = list()) {

  # Check whether data contains any filters
  filter_cols <- colnames(metaUI__df) %>% stringr::str_subset("metaUI__filter_")
  if (length(filter_cols) == 0) any_filters <- FALSE else any_filters <- TRUE


glue_string <- ('
    function(input, output, session) {

  metaui_plotly_font <- "\\"Source Sans 3\\", Arial, sans-serif"
  plot_colours <- metaUI_plot_colours(accent = "<<if (is.null(opts$design)) "#1d5c48" else opts$design$accent>>",
    ink = "<<if (is.null(opts$design)) "#1a2530" else opts$design$text>>",
    grid = "<<if (is.null(opts$design)) "#e3e7ea" else design_mix(opts$design$text, opts$design$surface, .14)>>",
    background = "<<if (is.null(opts$design)) "#FFFFFF" else opts$design$surface>>",
    slate = "<<if (is.null(opts$design)) "#55626e" else design_mix(opts$design$text, opts$design$surface, .75)>>",
    shade = "<<if (is.null(opts$design)) "#f0f2f4" else design_mix(opts$design$text, opts$design$surface, .07)>>",
    bars = "<<if (is.null(opts$design)) "#b4bcc4" else design_mix(opts$design$text, opts$design$surface, .32)>>",
    attention = "<<if (is.null(opts$design)) "#8a4b00" else opts$design$accent>>")
  # Readers whose browser asks for dark colours (metaui.js sends metaui_dark) see plots drawn dark.
  plot_colours_dark <- <<if (is.null(opts$design)) \'metaUI_plot_colours(accent = "#6cc4a1", ink = "#e3e7ea", grid = "#2c333a", background = "#191c20", slate = "#a1acb6", shade = "#23292f", bars = "#5f6a75", attention = "#f0a765")\' else "plot_colours">>
  plot_palette <- reactive(if (isTRUE(input$metaui_dark)) plot_colours_dark else plot_colours)
  # Odds and risk ratios are fitted as logs and shown as ratios, on log axes around 1.
  ratio_scale <- metaUI_scale(metaUI__df$metaUI__es_type[1])$ratio

  # Set app states
    state_values <- reactiveValues(
      uploaded_data = NULL,
      upload_info = NULL,
      ever_analyzed = FALSE,
      restore_source = "upload",
      link_notice = NULL
    )

  # Create slider to filter by z-scores

  insertUI(
    selector = "#z_score_filter",
    where = "beforeEnd",
    ui = metaUI_slider_input("outliers_z_scores", "z-score range",
      metaUI_slider_spec(metaUI__df$metaUI__es_z, integer = FALSE))
  )
  filters <- list()
 <FILTER>
  filters <- list(<<
    filter_cols <- colnames(metaUI__df) %>% stringr::str_subset("metaUI__filter_")
    filter_ids <- metaUI_filter_id(filter_cols)
    filters_types <- purrr::map_chr(filter_cols, \\(x) {
      if (is.numeric(metaUI__df[[x]])) "numeric" else "selection"
    })
   filters <- purrr::pmap(list(filter_cols, filter_ids, filters_types), ~list(col = ..1, id = ..2, type =  ..3))
       purrr::pmap(list(filter_cols, filter_ids, filters_types), ~ paste0("list(col = ", metaUI_r_string(..1), ", id = ", metaUI_r_string(..2), ", type = ", metaUI_r_string(..3), ")"))  %>%
        glue::glue_collapse(sep = ",\n")
      >>)
 </FILTER>

  # Apply reactive filtering of dataset when clicking on the button
  df_filtered <- eventReactive(input$go, {
    df_reactive()
  })

  # Reset filters
       observeEvent(input$resetFilters, {
        shinyjs::reset("filters")
        # Categories that uploaded data added are selected too, once the reset (a round trip
        # through the browser) has run.
        shinyjs::delay(600, for (id in ls(changed_choices)) if (isTRUE(changed_choices[[id]])) {
          f <- Filter(function(i) i$id == id, filters)[[1]]
          if (length(levels(metaUI__df[[f$col]])) >= <<opts$selection_list_threshold>>) shinyWidgets::updatePickerInput(session, inputId = id, selected = levels(shiny::isolate(state_values$uploaded_data)[[f$col]]))
          else updateCheckboxGroupInput(inputId = id, selected = levels(shiny::isolate(state_values$uploaded_data)[[f$col]]))
        })
        if (length(metaUI_computation_specs(metaUI__df))) updateSelectInput(session, "computation", selected = "primary")
      })


  # An empty selection cannot be applied: every tab would have nothing to show.
  pending_rows <- reactive(tryCatch(nrow(df_reactive()), error = function(e) NA_integer_))
  observe({
    shinyjs::toggleState("go", condition = length(input[["outliers_z_scores"]]) == 2L && !identical(pending_rows(), 0L))
  })

  # Readers land on results: the default analysis starts once the inputs are ready.
  # Restored links and uploads run their own analysis after restoring their filters.
  opening_run <- reactiveVal(FALSE)
  observeEvent(input[["outliers_z_scores"]], {
    if (isTruthy(input$go) || !is.null(state_values$pending_upload_filters)) return()
    opening_run(TRUE)
    shinyjs::runjs("$(\'#go\').trigger(\'click\', [{auto: true}]);")
  }, once = TRUE, priority = -1)

  capture_filters <- function() {
    req(length(input[["outliers_z_scores"]]) == 2L)
    filter_selections <- tibble::tibble(id = "outliers_z_scores", selection = input[["outliers_z_scores"]])
    <FILTER>
    for (i in filters) {
      values <- input[[i$id]]
      filter_selections <- rbind(filter_selections, tibble::tibble(id = i$id, selection = if (length(values)) as.character(values) else NA_character_))
      if (i$type == "numeric") filter_selections <- rbind(filter_selections,
        tibble::tibble(id = paste0(i$id, "_include_NA"), selection = as.character(!identical(input[[paste0(i$id, "_include_NA")]], FALSE))))
    }
    </FILTER>
    if (length(metaUI_computation_specs(metaUI__df))) filter_selections <- rbind(filter_selections,
      tibble::tibble(id = "computation", selection = if (is.null(input$computation)) "primary" else input$computation))
    filter_selections
  }

  <FILTER>
  # The count beside "Include missing values" is for the effects that the other filters, as now set, keep.
  missing_counts <- shiny::debounce(reactive({
    df <- if (is.null(state_values$uploaded_data)) metaUI__df else state_values$uploaded_data
    z <- input[["outliers_z_scores"]]
    keep <- if (length(z) == 2L) (df$metaUI__es_z >= z[1] & df$metaUI__es_z <= z[2]) %in% TRUE else rep(TRUE, nrow(df))
    kept_by <- lapply(filters, function(i) {
      value <- df[[i$col]]; set <- input[[i$id]]
      if (i$type != "numeric") return(value %in% set)
      if (length(set) != 2L) return(rep(TRUE, nrow(df)))
      (!is.na(value) & value >= set[1] & value <= set[2]) | (is.na(value) & !identical(input[[paste0(i$id, "_include_NA")]], FALSE))
    })
    numeric <- which(vapply(filters, function(i) i$type == "numeric", logical(1)))
    counts <- vapply(numeric, function(j) sum(is.na(df[[filters[[j]]$col]]) & keep & Reduce(`&`, kept_by[-j], TRUE)), integer(1))
    stats::setNames(counts, vapply(filters[numeric], function(i) i$id, character(1)))
  }), 300)
  observe({
    counts <- missing_counts()
    for (id in names(counts)) updateCheckboxInput(inputId = paste0(id, "_include_NA"), label = paste0("Include missing values (", counts[[id]], ")"))
  })
  </FILTER>

  df_reactive <- reactive({
    applied_filters <- capture_filters()
    df <- if (is.null(state_values$uploaded_data)) metaUI__df else state_values$uploaded_data


    source_df <- df
    available_rows <- nrow(df)
    counts <- list()
    <FILTER>
    for (i in filters) {
      before <- nrow(df)
      if (i$type == "numeric") {
        value <- df[[i$col]]; bounds <- input[[i$id]]
        keep <- (!is.na(value) & value >= bounds[1] & value <= bounds[2]) |
          (is.na(value) & !identical(input[[paste0(i$id, "_include_NA")]], FALSE))
      } else {
        keep <- df[[i$col]] %in% input[[i$id]]
      }
      df <- df[keep, , drop = FALSE]
      counts[[i$id]] <- before - nrow(df)
    }
    </FILTER>

    metaUI_validate_prepared(df)
    # Filter by zscore
    before <- nrow(df)
    df <- df[df$metaUI__es_z >= input$outliers_z_scores[1] & df$metaUI__es_z <= input$outliers_z_scores[2], ]
    counts$outliers_z_scores <- before - nrow(df)
    if (length(metaUI_computation_specs(metaUI__df))) df <- metaUI_select_computation(df,
      if (is.null(input$computation)) "primary" else input$computation, contract = source_df)
    attr(df, "metaUI_source_data") <- source_df
    attr(df, "metaUI_applied_filters") <- applied_filters
    attr(df, "metaUI_upload_info") <- state_values$upload_info # label results with the data they used
    attr(df, "metaUI_filter_report") <- list(available_rows = available_rows,
      retained_rows = nrow(df), total_excluded = available_rows - nrow(df), successive_filter_exclusions = counts)
    df
  })

  # The browser asks after showing a tab; the answer arrives once the outputs in that tab are under way.
  observeEvent(input$metaui_drawn, session$sendCustomMessage("metaui_drawn", input$metaui_drawn))

  output$selection_overview <- renderText({
    req(isTruthy(input$go))
    df <- df_filtered()
    counts <- attr(df, "metaUI_filter_report")
    studies <- length(unique(df$metaUI__study_id))
    n <- function(x) format(x, big.mark = ",")
    if (filters_changed()) paste0("Results show the previous selection: ", n(counts$retained_rows), " of ", n(counts$available_rows),
      " effects.")
    else paste0("Results use ", n(counts$retained_rows), " of ", n(counts$available_rows), " effects (", n(studies), if (studies == 1L) " study)" else " studies)")
  })

  output$selection_status <- renderText({
    if (!is.null(state_values$pending_upload_filters)) return("Restoring saved filters.")
    validate(need(isTruthy(input$go), "No analysis yet."))
    df <- df_filtered()
    counts <- attr(df, "metaUI_filter_report")
    # Input IDs replace spaces with underscores; show the column labels instead
    filter_cols <- grep("^metaUI__filter_", names(metaUI__df), value = TRUE)
    labels <- c(stats::setNames(sub("^metaUI__filter_", "", filter_cols), metaUI_filter_id(filter_cols)),
      outliers_z_scores = "z-score range")
    filter_names <- unname(labels[names(counts$successive_filter_exclusions)])
    excluded <- unlist(counts$successive_filter_exclusions)
    # Only filters that removed effects are listed.
    details <- paste(paste(filter_names[excluded > 0], excluded[excluded > 0], sep = ": "), collapse = "; ")
    upload <- attr(df, "metaUI_upload_info")
    paste(if (is.null(upload)) "Built dataset (as published with this app)." else paste0("Uploaded data: ", upload$file,
      " (", upload$rows, " rows), not the authors\' dataset. ", upload$z_rule,
      "; supplied z disagreements: ", upload$z_disagreements, ". ", upload$changes, upload$checkbox_note),
      if (length(metaUI_computation_specs(metaUI__df))) paste0("Computation: ",
        names(metaUI_computations(metaUI__df))[match(attr(df, "metaUI_computation"), metaUI_computations(metaUI__df))], ".") else "",
      "Selected", format(counts$retained_rows, big.mark = ","), "of", format(counts$available_rows, big.mark = ","),
      "rows.", if (counts$total_excluded == 0) "No effects excluded." else paste("Excluded", format(counts$total_excluded, big.mark = ","), paste0("by successive filters (", details, ").")))
  })

  outputOptions(output, "selection_status", suspendWhenHidden = FALSE)

  # Compare live inputs with the selections behind the displayed results
  filters_changed <- reactive({
    if (!isTruthy(input$go) || !is.null(state_values$pending_upload_filters)) return(FALSE)
    !identical(capture_filters(), attr(df_filtered(), "metaUI_applied_filters")) ||
      !identical(state_values$upload_info, attr(df_filtered(), "metaUI_upload_info"))
  })

  observe({
    changed <- filters_changed()
    shinyjs::toggleClass("go", "metaui-needs-run", condition = changed)
    shinyjs::toggleClass("metaui_basis", "metaui-stale", condition = changed)
    shinyjs::toggleClass("apply_state_box", "metaui-stale", condition = changed)
  })

  output$stale_banner <- renderText({
    if (!is.null(state_values$pending_upload_filters)) return(paste0("Applying the ", if (state_values$restore_source == "upload") "uploaded" else "linked", " selection\\u2026"))
    if (!isTruthy(input$go) || !filters_changed()) return("")
    if (identical(pending_rows(), 0L)) "No effects match the new filters; these results are for the previous selection."
    else "These results are for the previous selection."
  })
  outputOptions(output, "stale_banner", suspendWhenHidden = FALSE)
  observe({
    restoring <- !is.null(state_values$pending_upload_filters)
    shinyjs::toggleClass("metaui_stale_banner", "metaui-visible", condition = restoring || filters_changed())
    shinyjs::toggleClass("metaui_stale_banner", "metaui-restoring", condition = restoring || identical(pending_rows(), 0L))
  })

  output$apply_state <- renderText({
    if (!is.null(state_values$pending_upload_filters)) return(paste("Restoring", state_values$restore_source, "filters, then analyzing."))
    if (isTruthy(input$go) && filters_changed()) {
      pending <- pending_rows()
      if (identical(pending, 0L)) "No effects match these filters. Widen them to update the results."
      else if (is.na(pending)) "New selection. Update results to apply it." else paste0("New selection: ", format(pending, big.mark = ","), if (pending == 1L) " effect" else " effects", ". Update results to apply it.")
    }
    else ""
  })
  # The status box is hidden while empty; keep it rendering so the notice can appear.
  outputOptions(output, "apply_state", suspendWhenHidden = FALSE)

  # Data for forest plot and table ------------------------------------------
  # Starts with the default analysis precomputed at build (metaUI_opening), if still valid.
  fit_cached <- metaUI_fit_cache(<<opts$fit_cache_entries>>, seed = metaUI_opening$fit)

  estimatesreactive <- reactive({
    df <- df_filtered()

    # Check if there are any studies left after filtering
    if (nrow(df) == 0) return(NULL)

   state_values$ever_analyzed <- TRUE

    fit_cached(df, models_to_run, correlation_dependent, aggregation_method[1])
  })

  # Results replace the opening view only when they exist, so readers never see empty sections.
  output$metaui_ready <- reactive({
    req(isTruthy(input$go))
    if (nrow(df_filtered())) try(estimatesreactive(), silent = TRUE)
    TRUE
  })
  outputOptions(output, "metaui_ready", suspendWhenHidden = FALSE)

  output$calculation_status <- renderText({
    result <- estimatesreactive()
    req(result)
    if (isTRUE(result$precomputed)) "Precomputed when this app was built." else if (result$cache_hit) "Reused identical earlier fits." else sprintf("Fitted in %.2f s.", result$calculation_seconds)
  })
  outputOptions(output, "calculation_status", suspendWhenHidden = FALSE)

  estimatesfiltered <- eventReactive(input$go, {
    estimatesreactive()$table
  })

  # Aggregated values meta-analysis ----------------------------------------------


  # Six entries cover all computations of one selection; 0 keeps the cache opt-out.
  comparison_cache <- metaUI_fit_cache(if (<<opts$fit_cache_entries>> == 0) 0L else 6L, limit = 6L)
  comparison_models <- models_to_run[models_to_run$code %in% metaUI_summary_codes, , drop = FALSE]
  computations_comparison <- eventReactive(input$go, {
    df <- df_filtered()
    if (!nrow(df)) return(data.frame(computation = "All", Model = "Unavailable", es = NA_real_, LCL = NA_real_, UCL = NA_real_, status = "unsupported", reason = "No eligible rows selected"))
    if (!nrow(comparison_models)) return(data.frame(computation = "All", Model = "Unavailable", es = NA_real_, LCL = NA_real_, UCL = NA_real_, status = "unsupported", reason = "No multilevel or RVE specification is present."))
    choices <- metaUI_computations(metaUI__df)
    do.call(rbind, lapply(seq_along(choices), function(i) {
      result <- comparison_cache(metaUI_select_computation(df, unname(choices[i]), contract = attr(df, "metaUI_source_data")), comparison_models,
        correlation_dependent, aggregation_method[1])$table
      data.frame(computation = names(choices)[i], result, row.names = NULL)
    }))
  })
  output$computation_comparison <- renderTable({
    computations_comparison()[c("computation", "Model", "es", "LCL", "UCL", "status", "reason")]
  }, digits = 4)
  output$computation_note <- renderUI({
    id <- if (is.null(input$computation)) "primary" else input$computation
    specs <- metaUI_computation_specs(metaUI__df)
    spec <- Filter(function(x) x$id == id, specs)
    p(class = "metaui-note", if (length(spec)) spec[[1]]$justification else "As supplied by the authors.",
      "The z-score filter uses the primary computation.")
  })

  bayesian_options <- <<paste(deparse(opts$bayesian), collapse = "\n")>>
  bayesian_sensitivity <- eventReactive(input$go, {
    result <- estimatesreactive()
    if (is.null(result)) return(data.frame(status = "unsupported", reason = "No eligible rows selected"))
    idx <- if (is.null(bayesian_options)) integer() else which(models_to_run$code == metaUI_bayesian_spec(bayesian_options)$code)
    primary <- if (length(idx)) result$fits[[idx[1]]] else NULL
    metaUI_bayesian_sensitivity(result$df_agg, bayesian_options, primary)
  })
  output$bayesian_sensitivity <- renderTable(bayesian_sensitivity(), digits = 4)

  df_agg <- reactive({
    estimatesreactive()$df_agg
  })

  sample_overview <- reactive({
    df <- df_filtered()

    overview <- tibble::tribble(
      ~Sources, ~Studies, ~Effects,
      if ("metaUI__article_label" %in% names(df)) length(unique(df$metaUI__article_label)) else 0L, length(unique(df$metaUI__study_id)),
      length(df$metaUI__study_id)
    ) # No sample-size total: samples may overlap, and summing them needs a documented independent-sample rule.

    if (overview$Sources == 0) {
      overview$Sources <- "not specified"
    }

    message(paste("The current dataset contains", overview$Sources, "sources,", overview$Studies,
      "study clusters and", overview$Effects, "effects.",
      sep = " "
    ))

    if (overview$Sources == "not specified") {
      overview$Sources <- NULL
    }

    overview
  })

  output$sample_overview <- renderTable(sample_overview())

  known_interval_models <- <<paste(deparse(opts$known_interval_models), collapse = "\n")>>
  chosen_bound <- reactive({
    value <- input$sesoi
    if (is.null(value) || is.na(value)) return(NULL)
    # Validate the bound alone; empty selections have no estimates table.
    metaUI_interval_assessment(data.frame(Model = character(), status = character(), LCL = numeric(), UCL = numeric()),
      value, metaUI__df$metaUI__display_scale[1])
    # For ratios, 0.8 and 1.25 name the same bounds; the larger is kept.
    if (ratio_scale) max(value, 1 / value) else value
  })
  interval_assessment <- reactive({
    if (is.null(estimatesfiltered())) return(data.frame(assessment = "Not assessed", reason = "No eligible rows selected"))
    tryCatch(metaUI_interval_assessment(estimatesfiltered(), chosen_bound(),
      metaUI__df$metaUI__display_scale[1], known_interval_models),
      error = function(e) data.frame(assessment = "Not assessed", reason = conditionMessage(e), bound = input$sesoi))
  })
  # Downloads keep every column; the reader sees what the chosen bound decides.
  output$equivalence <- renderTable({
    table <- interval_assessment()
    # Without a valid bound there is nothing to tabulate; the reason is given below.
    if (!"threshold" %in% names(table)) return(NULL)
    bound <- tryCatch(chosen_bound(), error = function(e) NULL)
    # Rounded up, so that the shown bound still contains the interval; downloads keep every digit.
    threshold <- ifelse(is.na(table$threshold), "", formatC(ceiling(table$threshold * 1000 + 1e-9) / 1000, format = "f", digits = 3))
    within <- if (ratio_scale) "90% interval lies within 1/x to x, x =" else "90% interval lies within \\u00b1"
    if (is.null(bound)) return(stats::setNames(data.frame(table$Model, threshold), c("Model", within)))
    stats::setNames(data.frame(table$Model, table$assessment, threshold), c("Model", "Assessment", within))
  }, align = "?")
  # The answer leads, directly under the bound it answers.
  output$equivalence_verdict <- renderUI({
    table <- interval_assessment()
    bound <- tryCatch(chosen_bound(), error = function(e) NULL)
    req(!is.null(bound), "assessment" %in% names(table))
    # Only models with an interval are assessed; the others are counted separately.
    assessed <- table$assessment != "Not assessed"
    p(class = "metaui-reading", HTML(paste0("<b>", sum(grepl("within bounds", table$assessment, fixed = TRUE)), " of ", sum(assessed), "</b> 90% ", if (sum(assessed) == 1L) "interval lies" else "intervals lie", " within ", if (ratio_scale) paste0(format(signif(min(bound, 1 / bound), 3)), " to ", format(signif(max(bound, 1 / bound), 3))) else paste0("\\u00b1", format(bound)), ".",
      if (any(!assessed)) paste0(" ", sum(!assessed), if (sum(!assessed) == 1L) " model was" else " models were", " not estimated or gave no 90% interval, so ", if (sum(!assessed) == 1L) "it is" else "they are", " not assessed.") else "")))
  })
  output$equivalence_note <- renderUI({
    table <- interval_assessment()
    tagList(
      if ("reason" %in% names(table)) p(class = "metaui-reason", table$reason),
      # Defines the labels in the table, which the downloads keep word for word.
      if (!"reason" %in% names(table) && any(table$assessment %in% c("Interval within bounds", "Exceeds positive bound", "Exceeds negative bound", "Exceeds upper bound", "Exceeds lower bound", "Inconclusive")))
        p(class = "metaui-note", if (ratio_scale) "Interval within bounds: the whole 90% interval lies between the lower and the upper bound. Exceeds upper (lower) bound: the whole interval lies above the upper bound (below the lower one). Inconclusive: the interval crosses a bound."
          else "Interval within bounds: the whole 90% interval lies between the negative and the positive bound. Exceeds positive (negative) bound: the whole interval lies above the positive bound (below the negative one). Inconclusive: the interval crosses a bound."),
      tags$details(class = "metaui-help", tags$summary("How to read this assessment"), tags$p(if (ratio_scale) "An interval that lies strictly inside the bounds rejects, at .05 each, both an effect at or below the lower bound and one at or above the upper bound." else "An interval that lies strictly inside the bounds rejects, at .05 each, both an effect at or below the negative bound and one at or above the positive bound.", " This pair of one-sided tests (TOST) is the usual equivalence test, and its 90% interval matches it; requiring the 95% interval would make the test stricter than its stated level. For default models, the intervals rest on the model assumptions. Custom intervals have unknown coverage. For Bayesian rows, a 90% credible interval inside the bounds means at least 90% posterior probability within them; it depends on the prior and is not a TOST. The multilevel model uses residual degrees of freedom; RVE has no small-sample correction, so its intervals can be too narrow. Bias-adjusted models rest on their own assumptions. This assesses the average effect; individual true effects can still exceed the bound.")))
  })

  # MODEL COMPARISON -----------------------------------------------------
  # Authors may declare one primary model (labels_and_options.R). It is listed first
  # and never replaced by another model when it cannot be estimated.
  primary_index <- function(table) if (is.null(primary_model)) integer() else which(table$Model == primary_model)
  display_order <- function(table) { first <- primary_index(table); c(first, setdiff(seq_len(nrow(table)), first)) }
  ci_label <- paste(gsub(" ", "", ci_width), "CI")

  output$no_effects <- reactive(isTruthy(input$go) && !nrow(df_filtered()))
  outputOptions(output, "no_effects", suspendWhenHidden = FALSE)

  output$primary_estimate <- renderUI({
    if (!nrow(df_filtered())) {
      counts <- attr(df_filtered(), "metaUI_filter_report")$successive_filter_exclusions
      filter_cols <- grep("^metaUI__filter_", names(metaUI__df), value = TRUE)
      labels <- c(stats::setNames(sub("^metaUI__filter_", "", filter_cols), metaUI_filter_id(filter_cols)), outliers_z_scores = "z-score range")
      last <- names(counts)[max(which(unlist(counts) > 0), 0)]
      return(div(class = "metaui-empty", role = "status", tags$p(class = "metaui-empty-title", "No effects match this selection"),
        tags$p(if (length(last)) paste0("The ", unname(labels[last]), " filter removed the last remaining effects. "), "Change it, or ", tags$button(type = "button", class = "metaui-reset-link", "reset the filters"), " to select every effect again.")))
    }
    table <- estimatesfiltered()
    req(table)
    # Small selections are where the caveat matters most, with or without a declared primary model.
    few_studies <- function(estimated) {
      studies <- length(unique(df_filtered()$metaUI__study_id))
      if (estimated && studies < 10L) tags$p(class = "metaui-primary-warning", paste0("With only ", studies, if (studies == 1L) " study" else " studies",
        ", these estimates are fragile: the variation between studies is poorly estimated, intervals can be too narrow or very wide, and bias-adjusted estimates are unstable."))
    }
    if (is.null(primary_model)) return(div(class = "metaui-primary metaui-primary-none",
      tags$p("The authors have not marked a primary model. The rows below are alternative estimators for the same selection; they answer different questions and the app does not choose between them."),
      few_studies(any(table$status == "ok"))))
    index <- primary_index(table)
    if (!length(index)) return(div(class = "metaui-primary metaui-primary-missing", role = "alert",
      tags$p(tags$b("Primary model not found: "), primary_model, ". Check the name in labels_and_options.R against models.R.")))
    row <- table[index[1], ]
    digits <- metaUI_digits(table$LCL[table$status == "ok"], table$UCL[table$status == "ok"])
    fmt <- function(x) metaUI_minus(formatC(x, format = "f", digits = digits))
    interval <- gsub(" ", "\\u00a0", if (grepl("^Confidence", row$interval_type)) ci_label else "95% interval", fixed = TRUE)
    unit <- metaUI_count_unit(row, length(unique(df_filtered()$metaUI__study_id)))
    div(class = "metaui-primary", role = "region", `aria-label` = "Primary analysis",
      if (row$status == "ok") tags$p(class = "metaui-primary-value", HTML(paste0("Estimated effect: ", metaUI_label_html(if (metaUI__df$metaUI__es_type[1] == "ZCOR") "r" else metaUI_eff_size_type_label), " = <b>", fmt(row$es), "</b>, ",
        if (is.na(row$LCL) || is.na(row$UCL)) "no interval reported" else paste0(interval, "\\u00a0[", fmt(row$LCL), ", ", fmt(row$UCL), "]"))))
      else tags$p(class = "metaui-primary-gap", tags$b("Not estimated for this selection: "), metaUI_reader_reason(row$reason, row$warnings), " The app does not substitute another model."),
      # The headline already gives the sign and the interval; this adds what it cannot.
      { reading <- if (row$status == "ok") trimws(paste0(
          { others <- table[-index[1], , drop = FALSE]; others <- others[others$status == "ok", , drop = FALSE]
            if (nrow(others) > 1L) paste0("Across the ", nrow(others), " other estimators, estimates range from ", fmt(min(others$es)), " to ", fmt(max(others$es)), ".") else "" },
          # An introduction from the authors states the direction in their own words.
          if (nzchar(welcome_text)) "" else switch(metaUI__df$metaUI__direction[1], positive = if (ratio_scale) " Ratios above 1 are in the direction the authors expected." else " Positive values are in the direction the authors expected.",
            negative = if (ratio_scale) " Ratios below 1 are in the direction the authors expected." else " Negative values are in the direction the authors expected.", ""))) else ""
        if (nzchar(reading)) tags$p(class = "metaui-primary-reading", reading) },
      # How far true effects spread around the average, where the model gives a prediction interval.
      # With fewer than three studies the spread between studies is barely estimable, so no interval is given.
      if (row$status == "ok" && isTRUE(is.finite(row$PI_LCL)) && isTRUE(is.finite(row$PI_UCL)) && length(unique(df_filtered()$metaUI__study_id)) >= 3L) tags$p(class = "metaui-primary-reading",
        paste0("In a comparable new study, the effect is expected to lie between ", fmt(row$PI_LCL), " and ", fmt(row$PI_UCL), " (95% prediction interval).")),
      # The tinted first row of the table below names the model and its k; the line here names it only when no estimate is shown.
      { label <- trimws(paste0(if (row$status != "ok") paste0(row$Model, ". Primary analysis, chosen by the authors.") else "",
          if (grepl("hedges|cohen|standardi[sz]ed mean|^smd$", metaUI_eff_size_type_label, ignore.case = TRUE)) " Effects are standardized mean differences, in standard deviations." else
          # The headline names r alone; a qualified label (adjusted for attenuation, say) is given here.
          if (metaUI__df$metaUI__es_type[1] == "ZCOR" && !tolower(metaUI_eff_size_type_label) %in% c("r", "correlation r", "correlation")) paste0(" Effects are correlations: ", metaUI_eff_size_type_label, ".") else
          if (ratio_scale) paste0(" Effects are ", metaUI_scale(metaUI__df$metaUI__es_type[1])$name, "s: 1 means no difference between the groups.") else ""))
        if (nzchar(label)) tags$p(class = "metaui-primary-label", label) },
      if (row$status == "ok" && nzchar(row$warnings)) tags$p(class = "metaui-primary-warning", paste("Warnings:", metaUI_reader_warnings(row$warnings))),
      few_studies(row$status == "ok"))
  })

  observe({
    table <- estimatesfiltered()
    shinyjs::toggleClass("effectestimate_box", "metaui-has-primary", condition = !is.null(table) && length(primary_index(table)) > 0)
  })

  output$scale_note <- renderUI({
    metric <- metaUI__df$metaUI__es_type[1]
    direction <- metaUI__df$metaUI__direction[1]
    scale_text <- if (metric == "ZCOR") paste("Correlations are fitted as Fisher z and shown as r.",
      "Slopes of numeric moderators, heterogeneity variances and the Egger test have no r equivalent and stay on the Fisher z scale.") else
      if (ratio_scale) { name <- metaUI_scale(metric)$name
        paste0(if (length(metaUI_count_columns(metaUI__df))) paste0("Each ", name, " was computed from the study&rsquo;s 2&times;2 table of counts (metafor::escalc), adding 0.5 to every cell of a table with a zero cell. ", metaUI_double_zero_text(metaUI__df)) else "",
          "Models are fitted to log ", name, "s, on which sampling errors are roughly normal; estimates, confidence and prediction intervals are shown as ", name, "s by exponentiating. ",
          if (metric == "OR") "An " else "A ", name, " of 1 means no difference; plots use a log axis, so that ratios such as 0.5 and 2 lie equally far from 1. ",
          "Slopes of numeric moderators (change in the log ", name, " per unit), heterogeneity variances and the Egger test have no ratio equivalent and stay on the log scale. ",
          "Downloads give es, LCL, UCL and the prediction and 90% intervals as ", name, "s; fit_es, fit_LCL and fit_UCL, and the effect sizes and variances in the dataset, are log ", name, "s.") } else
      paste0("Effects are fitted and shown on the scale the authors chose: ", metaUI_label_html(metaUI_eff_size_type_label), ".")
    direction_text <- if (direction == "unspecified") paste("The authors stated no expected direction. Methods that need one (by default p-uniform*,",
      "the Hedges-Vevea selection model and p-curve) are reported as unsupported rather than guessed.") else
      paste0("Direction the authors expected: ", direction, if (ratio_scale) (if (direction == "positive") " (ratios above 1)" else " (ratios below 1)") else "", ". Direction-dependent methods (p-uniform*, the Hedges-Vevea selection model, p-curve) analyze effects in that direction",
        if (direction == "negative") "; their inputs are sign-flipped for fitting and their estimates flipped back for display" else "", ". PET-PEESE tests PET\'s intercept one-sided in that direction. Other models are two-sided.")
    # With one effect per study there is no within-study dependence to describe.
    dependence_text <- if (!anyDuplicated(df_filtered()$metaUI__study_id)) paste("In this selection every study contributes one effect, and studies are assumed independent.",
        if (any(models_to_run$code == metaUI_code_multilevel)) paste("Effect-level and study-level models therefore use the same data, and the multilevel model is the standard random-effects model (REML).",
          "Its interval uses a t distribution with k \\u2212 1 degrees of freedom; trim-and-fill uses the Hartung-Knapp adjustment, so their intervals can differ slightly.") else
          "Effect-level and study-level models therefore use the same data.") else
      paste0("Effects from the same study are assumed to correlate at ", correlation_dependent, ". ",
      if (aggregation_method[1] == "first") "Study-level models use only the first selected effect from each study; RVE uses the correlation for its weights." else
      "Study-level models use it to combine the effects within each study (GLS average), and RVE for its weights.",
      " Effect-level models (by default multilevel and RVE) use every selected effect. Studies are assumed independent.")
    tagList(tags$h4("Scale, direction and dependence"), tags$p(HTML(scale_text)), tags$p(direction_text), tags$p(dependence_text))
  })

  # MODEL COMPARISON TABLE -------------------------------------------------------------------
  # Display formatting only: downloads keep the full estimatesfiltered() table.
  # Summary rows are back-transformed to r for correlations; say so on the axis.
  output$effectestimate <- renderText({
    table <- estimatesfiltered()
    req(table)
    metaUI_estimates_html(table, primary_index(table),
      axis_title = metaUI_eff_size_type_label,
      bound = tryCatch(chosen_bound(), error = function(e) NULL),
      studies = length(unique(df_filtered()$metaUI__study_id)), independent = !anyDuplicated(df_filtered()$metaUI__study_id), ratio = ratio_scale)
  })


  <FILTER>
  # Sample overview per filter/moderator -----------------------------------------------------------

       <FILTER>
    <<purrr::map_chr(filters, \\(f) {
      if (f$type == "numeric") {
        glue::glue("
    output$`summary_{f$id}_table` <- renderTable({{ # {{ escapes the glue syntax
    df <- df_filtered()
    summary <- summarise_numeric(df[[{metaUI_r_string(f$col)}]], {metaUI_r_string(stringr::str_remove(f$col, \'metaUI__filter_\'))})
    summary$Value <- sub(\'[.]00$\', \'\', summary$Value)
    summary[summary$Statistic != \'Var\', , drop = FALSE]
  }})

  output$`summary_{f$id}_plot` <- renderPlot({{
    plot_colours <- plot_palette() # light or dark, as the browser asks; downloads stay light
    df <- df_filtered()
    ggplot2::ggplot(df, ggplot2::aes(x = .data[[{metaUI_r_string(f$col)}]])) +
      ggplot2::geom_histogram(na.rm = TRUE, bins = 24, fill = plot_colours$bars, colour = plot_colours$background, linewidth = .3) +
      ggplot2::scale_y_continuous(limits = function(x) range(pretty(c(0, x), 3)), breaks = function(x) unique(floor(pretty(x, 3))), name = \'Effects\', expand = ggplot2::expansion(mult = 0)) +
      metaUI_plot_theme(plot_colours) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank())
  }}, bg = \'transparent\', alt = paste0(\'Histogram of \', {metaUI_r_string(stringr::str_remove(f$col, \'metaUI__filter_\'))}, \' in the selected effects; the table beside it gives the summary values.\'))
  ")
      } else {
        glue::glue("
          output$`summary_{f$id}_table` <- renderText({{
    df <- df_filtered()
    metaUI_bars_html(summarise_categorical(df[[{metaUI_r_string(f$col)}]], {metaUI_r_string(stringr::str_remove(f$col, \'metaUI__filter_\'))}), {metaUI_r_string(stringr::str_remove(f$col, \'metaUI__filter_\'))})
  }})
        ")
      }
    }) %>% paste(collapse = "\n")>>

  </FILTER>

  # Sample table -----------------------------------------------------------

  selected_references <- reactive(metaUI_selected_references(metaUI_study_info, df_filtered()$metaUI__study_id))
  output$references_txt <- downloadHandler(filename = function() download_name("references.txt"), content = function(file) {
    references <- selected_references()$Reference
    writeLines(enc2utf8(paste0(references[nzchar(references)], "\n")), file, useBytes = TRUE)
  })
  output$references_bib <- downloadHandler(filename = function() download_name("references.bib"), content = function(file) {
    refs <- selected_references()
    missing <- refs$Reference[is.na(refs$bibtex)]
    writeLines(enc2utf8(c(unique(stats::na.omit(refs$bibtex)), if (length(missing)) c("", "% References without a BibTeX entry:", paste("%", missing)))), file, useBytes = TRUE)
  })
  output$references_xlsx <- downloadHandler(filename = function() download_name("references.xlsx"), content = function(file) {
    writexl::write_xlsx(selected_references()[c("Study", "Reference", "Link")], file)
  })

  output$sample_table <- DT::renderDataTable({
    df <- df_filtered()

    out <- df %>%
      dplyr::select(dplyr::any_of("metaUI__article_label"),
        Study =
          "metaUI__study_id", metaUI__key = "metaUI__study_id", N = "metaUI__N",
        "Effect size" = "metaUI__effect_size", "Reported p" = "metaUI__pvalue",
        dplyr::starts_with("metaUI__filter_"), dplyr::any_of("metaUI__url")
      ) %>%
      dplyr::rename_with(~ stringr::str_replace(.x, "metaUI__filter_", "") %>%
        stringr::str_replace("metaUI__article_label", "Source") %>%
        stringr::str_replace("metaUI__url", "URL")) %>%
      dplyr::mutate(`Reported p` = fmt_p(.data[["Reported p"]], include_equal = FALSE),
        `Effect size` = metaUI_show(.data[["Effect size"]], df)) %>%
      dplyr::arrange(.data$Study)

    # Study labels that already name their source make the Source column a repeat.
    source <- if ("Source" %in% names(out)) as.character(out$Source) else NULL
    if (!is.null(source) && all(is.na(source) | startsWith(out$Study, source))) out$Source <- NULL
    view <- if (identical(input$sample_view, "sources") && !is.null(source)) "sources" else if (identical(input$sample_view, "studies")) "studies" else "effects"
    if (view == "studies") out <- metaUI_study_rows(out)
    if (view == "sources") {
      # One row per source: its studies are counted, and the source label stands in the first column.
      source[is.na(source)] <- "(Missing)"
      study_count <- tapply(out$Study, source, function(x) length(unique(x)))
      out$Source <- NULL
      out$Study <- source; out$metaUI__key <- source
      out <- metaUI_study_rows(out)
      out <- cbind(out[1], Studies = as.vector(study_count[out$Study]), out[-1])
    }
    # Source labels are data: they are shown as text (the table does not escape this column, as
    # in the sources view it holds the escaped study cells built below).
    if ("Source" %in% names(out)) out$Source <- ifelse(is.na(out$Source), NA_character_, htmltools::htmlEscape(as.character(out$Source)))
    unit <- view

    if ("URL" %in% names(out)) {
      # Only web addresses become links; a study with several shows "several" as text.
      out$URL <- vapply(out$URL, function(url) if (is.na(url) || url == "") "" else if (!grepl("^https?://", url)) htmltools::htmlEscape(url) else
        as.character(tags$a(href = url, target = "_blank", rel = "noopener", url)), character(1))
    }

    # Columns with no values for this dataset only take room from the others.
    out <- out[, c(TRUE, vapply(out[-1], function(x) !length(x) || !all(is.na(x) | x == ""), logical(1))), drop = FALSE]

    # Studies with author-supplied information get a disclosure button; metaui.js shows
    # the hidden details column as a child row. Nothing is fetched while the app runs.
    # The year stays with the last word of the title.
    out$Study <- sub(" [(]([0-9]{4}[a-z]?)[)]$", "\\u00a0(\\\\1)", out$Study)
    cells <- metaUI_study_cells(out$Study, if (view == "sources") rep(NA_character_, nrow(out)) else out$metaUI__key, metaUI_study_info, metaUI_study_details_html, metaUI_study_reference_text)
    out$Study <- cells$label
    out$metaUI__details <- cells$details
    out$metaUI__key <- NULL
    if (view == "sources") names(out)[names(out) == "Study"] <- "Source"

    # Non-integer numeric columns show 2 decimals where a value needs them (whole numbers such as
    # sample sizes and years stay whole), with true minus signs; sorting stays on the numbers.
    decimals <- setdiff(names(out)[vapply(out, function(x) is.numeric(x) && any(x != round(x), na.rm = TRUE), logical(1))], "Effect size")
    # Study rows give their range of effect sizes as text, already formatted.
    three_decimals <- DT::JS("function(data, type) { return type === \'display\' && typeof data === \'number\' ? data.toFixed(3).replace(\'-\', \'\\u2212\') : data; }")
    two_decimals <- DT::JS("function(data, type) { if (type !== \'display\' || data === null || data === \'\') return data; var x = Number(data); return String(Number(x.toFixed(2))).replace(\'-\', \'\\u2212\'); }")
    targets <- function(columns) match(intersect(columns, names(out)), names(out)) - 1

    table <- out %>% DT::datatable(
      rownames = FALSE, style = "default", class = "compact",
      options = list(autoWidth = FALSE, dom = \'<"metaui-dt-top"lfp>r<"metaui-dt-scroll"t><"metaui-dt-bottom"ip>\', columnDefs = list(list(targets = ncol(out) - 1, visible = FALSE, searchable = FALSE),
          list(targets = targets(decimals), render = two_decimals, className = "dt-right"),
          list(targets = targets("Effect size"), render = three_decimals, className = "dt-right"),
          list(targets = union(unname(which(vapply(out, is.numeric, logical(1)))) - 1, targets(c("N", "Effects", "Studies"))), className = "dt-right"),
          list(targets = "_all", defaultContent = "(Missing)")),
        language = list(search = paste("Search", unit), lengthMenu = "Show _MENU_ per page", info = paste("_START_ to _END_ of _TOTAL_", unit), infoFiltered = "(from _MAX_)",
          infoEmpty = paste("0", unit), zeroRecords = paste("No", unit, "match this search."), emptyTable = paste("No", unit, "selected."))),
      escape = setdiff(names(out), c("URL", "Study", "Source", "metaUI__details")), # built (and escaped) above
      caption = tags$caption(
        style = "caption-side: bottom; text-align: left; margin: 8px 0;",
        HTML(paste0(if (view != "effects") paste0("One row per ", if (view == "sources") "source" else "study", ". Values that differ between its effects show their range, or \\u201cseveral\\u201d. "), "Effect size: ", metaUI_label_html(metaUI_eff_size_type_label), "."))
      )
    )
    table
  })

 <FILTER>

  # Moderators  -----------------------------------------------------------


  # Categorical moderators are drawn in their table; numeric ones get a scatter beside theirs.
  # The finding stands over its table; the plot of a numeric moderator has its key beneath it.
  output$moderation_panel <- renderUI({
    # The finding states a failed fit once; nothing else is shown then.
    model <- tryCatch(moderator_model(), error = function(e) NULL)
    if (is.null(model) || isTRUE(model$constant)) shiny::htmlOutput("moderation_text")
    else if (is.numeric(df_filtered()[[input$moderator]])) div(class = "metaui-moderation-grid",
      div(shiny::htmlOutput("moderation_text"), shiny::htmlOutput("moderation_table")),
      div(shinycssloaders::withSpinner(plotly::plotlyOutput("moderation_plot", height = 340)),
        p(class = "metaui-note metaui-key", "Dots: selected effects, larger when more precise; whole-number moderators are spread slightly so that dots do not stack. Line and band: the moderator model named above, with its 95% CI.")))
    else div(class = "metaui-moderation-wide", shiny::htmlOutput("moderation_text"), shiny::htmlOutput("moderation_table"))
  })

  output$moderation_plot <- plotly::renderPlotly({
    plot_colours <- plot_palette() # light or dark, as the browser asks; downloads stay light
    df <- df_filtered()
    req(is.numeric(df[[input$moderator]]))
    df$mod <- df[[input$moderator]]
    # The line and band are the moderator model tested above; dots follow precision.
    model <- moderator_model()
    fitted <- model$grid
    # Ratios stay on the log scale they are fitted on, with the axis labelled in ratios.
    if (!ratio_scale) fitted[c("pred", "ci.lb", "ci.ub")] <- lapply(fitted[c("pred", "ci.lb", "ci.ub")], metaUI_show, df)
    df$shown <- if (ratio_scale) df$metaUI__effect_size else metaUI_show(df$metaUI__effect_size, df)
    df$hover <- paste0(df$metaUI__study_id, ": ", round(metaUI_show(df$metaUI__effect_size, df), 2))
    p <- ggplot2::ggplot(data = df, ggplot2::aes(y = shown, x = mod)) +
      ggplot2::geom_hline(yintercept = 0, colour = plot_colours$slate, linewidth = .3) +
      ggplot2::geom_ribbon(data = fitted, ggplot2::aes(x = mod, ymin = ci.lb, ymax = ci.ub), inherit.aes = FALSE, fill = plot_colours$accent, alpha = .14) +
      # Whole-number moderators (years, say) stack their dots: spread them a little, for sight only.
      suppressWarnings(ggplot2::geom_point(ggplot2::aes(size = 1 / metaUI__se, text = hover), shape = 21, fill = plot_colours$slate, colour = plot_colours$background, stroke = .3, alpha = .65, na.rm = TRUE,
        position = ggplot2::position_jitter(width = if (all(df$mod == round(df$mod), na.rm = TRUE)) .3 else 0, height = 0, seed = 1))) +
      ggplot2::geom_line(data = fitted, ggplot2::aes(x = mod, y = pred), inherit.aes = FALSE, colour = plot_colours$accent, linewidth = .7) +
      ggplot2::scale_size(range = c(1.8, 3.6), guide = "none") +
      ggplot2::scale_x_continuous(breaks = { span <- range(df$mod, na.rm = TRUE); ticks <- pretty(span, 8); ticks[ticks >= span[1] & ticks <= span[2]] }) +
      { span <- range(df$shown, fitted$ci.lb, fitted$ci.ub)
        if (ratio_scale) { ticks <- metaUI_ratio_ticks(span, 7); ggplot2::scale_y_continuous(breaks = log(ticks), labels = metaUI_tick_labels(ticks)) }
        else { ticks <- pretty(span, 8); ggplot2::scale_y_continuous(breaks = ticks[ticks >= span[1] & ticks <= span[2]]) } } +
      ggplot2::xlab(input$moderator %>% stringr::str_remove("metaUI__filter_")) +
      ggplot2::ylab(metaUI_eff_size_type_label) +
      metaUI_plot_theme(plot_colours, family = "") +
      ggplot2::theme(legend.position = "none")
    built <- plotly::ggplotly(p, tooltip = "text")
    built$x$layout$yaxis$ticktext <- metaUI_minus(built$x$layout$yaxis$ticktext)
    built$x$layout$xaxis$ticktext <- metaUI_minus(built$x$layout$xaxis$ticktext)
    built %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(xaxis = list(fixedrange = TRUE, showgrid = FALSE, tickfont = list(size = 13, color = plot_colours$slate), title = list(font = list(size = 13.5))),
        yaxis = list(fixedrange = TRUE, tickfont = list(size = 13, color = plot_colours$slate), title = list(text = metaUI_label_html(metaUI_eff_size_type_label), font = list(size = 13.5)), nticks = 8), font = list(family = metaui_plotly_font), margin = list(t = 6),
        paper_bgcolor = plot_colours$background, plot_bgcolor = plot_colours$background)
  })


  # While the reader has not picked a moderator, the default moves to the first one that varies in the selection.
  moderator_default <- <<paste(deparse(filter_cols[1]), collapse = "")>>
  moderator_picked <- reactiveVal(FALSE)
  observeEvent(input$moderator, if (!identical(input$moderator, moderator_default) && !identical(input$moderator, isolate(moderator_auto()))) moderator_picked(TRUE), ignoreInit = TRUE)
  moderator_auto <- reactiveVal(NULL)
  observeEvent(df_filtered(), {
    df <- df_filtered()
    if (moderator_picked() || !nrow(df)) return()
    varies <- vapply(filters, function(f) length(unique(stats::na.omit(df[[f$col]]))) >= 2L, logical(1))
    if (!any(varies)) return()
    first <- filters[[which(varies)[1]]]$col
    if (!identical(first, input$moderator)) { moderator_auto(first); updateSelectInput(session, "moderator", selected = first) }
  })

  # A fit that fails (no convergence on a small selection, say) is reported with its reason.
  moderator_model <- reactive({
    model <- tryCatch(moderator_fit(), error = function(e) e)
    if (inherits(model, "shiny.silent.error")) stop(model)
    if (inherits(model, "error")) shiny::validate(shiny::need(FALSE, paste("Moderator model could not be estimated for this selection:", conditionMessage(model))))
    model
  })

  # One moderator test, kept as the few numbers and the text that the tab shows, so that it
  # can be saved at build for the default selection and reused within a session.
  moderator_primary <- if (!is.null(primary_model) && sum(models_to_run$name == primary_model) == 1L &&
    !models_to_run$aggregated[models_to_run$name == primary_model]) list(name = primary_model, code = models_to_run$code[models_to_run$name == primary_model]) else NULL
  moderator_summary <- function(df, moderator) {
    name <- stringr::str_remove(moderator, "metaUI__filter_")
    # Effects without a value for the moderator, including a kept (Missing) category, are left out of its test.
    total <- nrow(df)
    df <- metaUI_moderator_rows(df, moderator)
    # A moderator with one value has nothing to fit, test or draw.
    if (length(unique(stats::na.omit(df[[moderator]]))) < 2L)
      return(list(constant = TRUE, moderation_text = HTML(paste0(\'<p class="metaui-moderation-finding">The selected effects do not vary on <b>\', htmltools::htmlEscape(name), \'</b>, so its relationship with the effect sizes cannot be tested. Widen the selection to test it.</p>\'))))
    numeric <- is.numeric(df[[moderator]])
    fitted <- metaUI_moderator_fit(df, moderator, moderator_primary, correlation_dependent)
    model_sig <- fitted$model
    # Denominator degrees of freedom from the small-sample correction are not whole numbers.
    df_text <- function(x) if (abs(x - round(x)) < 1e-8) format(round(x)) else formatC(x, format = "f", digits = 1)
    moderation_text <- if (fitted$omnibus) paste0(\'<p class="metaui-moderation-finding"><b>\', htmltools::htmlEscape(name), "</b> is <b>", if (model_sig[["QMp"]] < .05) "a" else "not a",
      " significant", if (numeric) " linear" else "", " moderator</b> at the 5% level: <span class=\'metaui-nowrap\'><i>F</i>(", df_text(model_sig[["QMdf"]][1]), ", ", df_text(model_sig[["QMdf"]][2]), ")\\u00a0=\\u00a0",
      formatC(model_sig[["QM"]], format = "f", digits = 2), ", <i>p</i>\\u00a0", sub(" ", "\\u00a0", fmt_p(model_sig[["QMp"]])), "</span>.</p>") else
      paste0(\'<p class="metaui-moderation-finding">No overall test could be computed for <b>\', htmltools::htmlEscape(name), "</b> in this selection, usually because some categories hold too few studies. Compare the levels\\u2019 intervals below.</p>")
    # Which model the test extends, and whether its inference allows for dependent effects.
    moderation_text <- paste0(moderation_text, \'<p class="metaui-note">\', "Tested in ",
      if (fitted$working == "primary") paste0("the primary model (", htmltools::htmlEscape(primary_model), ")") else if (!anyDuplicated(df$metaUI__study_id)) "the random-effects model" else "the random-effects multilevel model",
      if (fitted$robust) paste0(" with cluster-robust standard errors by study (CR2).", if (fitted$omnibus && model_sig[["QMdf"]][2] < 4)
        " Its small-sample degrees of freedom are below 4, which happens when categories hold few studies, so the test has little power and its p-value is rough." else "") else
      ". Cluster-robust standard errors could not be computed for this selection, so the test is model-based and can be too liberal when effects from the same study are dependent.", "</p>")
    if (nrow(df) < total) moderation_text <- paste0(moderation_text, \'<p class="metaui-note">\', format(nrow(df), big.mark = ","), " of ", format(total, big.mark = ","),
      " selected effects have a value for ", htmltools::htmlEscape(name), " and enter this test; effects without one are left out.</p>")
    studies <- length(unique(df$metaUI__study_id))
    if (studies < 10) moderation_text <- paste0(moderation_text, \'<p class="metaui-note metaui-warning-text">Only \', studies, if (studies == 1) " study enters" else " studies enter",
      " this test, so it has little power and its intervals are rough. Treat the result as a description of these studies.</p>")
    model <- model_sig
    out <- if (numeric) list(constant = FALSE, moderation_text = HTML(moderation_text), b = as.numeric(model$b),
      ci.lb = as.numeric(model$ci.lb), ci.ub = as.numeric(model$ci.ub)) else {
      levels <- metaUI_moderator_levels(model, levels(droplevels(df[[moderator]])))
      list(constant = FALSE, moderation_text = HTML(moderation_text), b = levels$b, levels = levels$level, ci.lb = levels$ci.lb, ci.ub = levels$ci.ub)
    }
    if (numeric) {
      x <- df[[moderator]]
      out$centre <- as.data.frame(stats::predict(model, newmods = mean(x, na.rm = TRUE)))
      grid <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 40)
      out$grid <- cbind(as.data.frame(stats::predict(model, newmods = grid))[c("pred", "ci.lb", "ci.ub")], mod = grid)
    }
    out
  }
  # Saved summaries (default selection, every moderator) seed a small store for the session.
  moderator_key <- function(df, moderator) rlang::hash(list(df[c("metaUI__effect_id", "metaUI__study_id", "metaUI__effect_size", "metaUI__variance", moderator)], moderator))
  moderator_store <- list2env(if (is.null(metaUI_opening$moderators)) list() else metaUI_opening$moderators)
  moderator_fit <- reactive({
    df <- df_filtered()
    key <- moderator_key(df, input$moderator)
    if (exists(key, envir = moderator_store, inherits = FALSE)) return(get(key, envir = moderator_store))
    value <- moderator_summary(df, input$moderator)
    assign(key, value, envir = moderator_store)
    value
  })


  output$moderation_table <- shiny::renderText({
    model <- moderator_model()
    if (isTRUE(model$constant)) return("")
    df <- metaUI_moderator_rows(df_filtered(), input$moderator)
    x <- df[[input$moderator]]
    if (is.numeric(x)) {
      # Three significant digits: a per-unit slope such as -0.0027 must not read as -0.00
      coefficient <- function(value) metaUI_minus(trimws(formatC(signif(value, 3), format = "fg", digits = 3, flag = "#")))
      # Its interval shares the decimals of the slope, so that the three numbers line up.
      slope_text <- function(b, lb, ub) {
        places <- if (is.finite(b) && b != 0) min(8, max(0, 2 - floor(log10(abs(signif(b, 3)))))) else 3
        f <- function(v) metaUI_minus(formatC(v, format = "f", digits = places))
        paste0(coefficient(b), " [", f(lb), ", ", f(ub), "]")
      }
      # Mean and SD share their decimals, as in the Sample tab.
      places <- if (abs(mean(x, na.rm = TRUE)) >= 100) 1 else 2
      stat <- function(value) metaUI_minus(trimws(formatC(value, format = "f", digits = places)))
      # The range as the Sample tab gives it: no trailing zeros, and "to" so that minus signs stay clear.
      end <- function(value) { v <- stat(value); if (grepl("[.]", v)) sub("[.]?0+$", "", v) else v }
      range_text <- paste0(end(min(x, na.rm = TRUE)), " to ", end(max(x, na.rm = TRUE)))
      # These coefficients have no r equivalent.
      fisher <- metaUI_fit_suffix(df)
      row <- function(label, value) paste0("<tr><td>", label, \'</td><td class="num">\', value, "</td></tr>")
      return(paste0(\'<table class="table metaui-custom metaui-stats"><tbody>\',
        # The intercept would be the effect at a moderator value of 0, often far outside the data; the mean is inside it.
        { centre <- model$centre
          shown <- metaUI_show(c(centre$pred, centre$ci.lb, centre$ci.ub), df)
          digits <- metaUI_digits(shown[2], shown[3]); value <- function(v) metaUI_minus(formatC(v, format = "f", digits = digits))
          row(paste0("Effect at the mean ", htmltools::htmlEscape(sub("^metaUI__filter_", "", input$moderator)), " (", stat(mean(x, na.rm = TRUE)), ") [95% CI]"),
            paste0(value(shown[1]), " [", value(shown[2]), ", ", value(shown[3]), "]")) }, row(paste0("Slope, per unit of ", htmltools::htmlEscape(sub("^metaUI__filter_", "", input$moderator)), fisher, " [95% CI]"), slope_text(model$b[2], model$ci.lb[2], model$ci.ub[2])), row("<i>k</i>", sum(!is.na(x))),
        row("Moderator mean (SD)", paste0(stat(mean(x, na.rm = TRUE)), " (", stat(stats::sd(x, na.rm = TRUE)), ")")),
        row("Moderator range", range_text), "</tbody></table>"))
    }
    levels <- model$levels
    counts <- table(x)
    summary <- estimatesfiltered()
    # The line is the primary estimate of the Summary, so that the same number appears on both tabs.
    reference <- intersect(c(primary_index(summary), which(models_to_run$code %in% c(metaUI_code_multilevel, metaUI_code_random))), which(summary$status == "ok"))
    overall <- summary$es[reference]
    shown <- metaUI_show(df$metaUI__effect_size, df)
    metaUI_moderator_html(levels, metaUI_show(as.numeric(model$b), df), metaUI_show(model$ci.lb, df), metaUI_show(model$ci.ub, df),
      as.integer(counts[levels]), x, shown,
      paste0(df$metaUI__study_id, ": ", round(shown, 2)), metaUI_eff_size_type_label,
      ref = if (length(overall)) overall[1] else NULL, ref_label = if (length(overall)) summary$Model[reference[1]], se = df$metaUI__se, show_effects = isTRUE(input$moderator_effects), keep_order = metaUI_author_order(x), ratio = ratio_scale)
  })

  output$moderation_text <- shiny::renderText({
    model <- moderator_model()
    model$moderation_text
  })

  </FILTER>

  # Heterogeneity -----------------------------------------------------------

  # Heterogeneity comes from the primary model when it has study and within-study components;
  # otherwise from the multilevel model, which is named.
  heterogeneity_source <- reactive({
    df <- df_filtered()
    results <- estimatesreactive()
    req(results)
    primary <- primary_index(results$table)
    # The default summary model: the multilevel model, or the random-effects model that replaces it.
    summary_row <- which(models_to_run$code %in% c(metaUI_code_multilevel, metaUI_code_random) & !models_to_run$aggregated)[1]
    code <- if (is.na(summary_row)) metaUI_code_multilevel else models_to_run$code[summary_row]
    name <- if (is.na(summary_row)) "Random-Effects Multilevel Model" else models_to_run$name[summary_row]
    if (length(primary) && identical(results$fit_data, df) && results$table$status[primary[1]] == "ok" && !is.null(results$components[[primary[1]]]))
      return(list(model = primary_model, primary = TRUE, fit = results$components[[primary[1]]]))
    list(model = name, primary = !is.na(summary_row) && identical(primary_model, name),
      fit = tryCatch(metaUI_reuse_fit(results, models_to_run, code, df, function() if (identical(code, metaUI_code_random))
        metaUI_univariate(df$metaUI__effect_size, df$metaUI__se, method = "REML") else metaUI_multilevel_fit(df)), error = function(e) e))
  })
  # A selection too small for the multilevel model (one effect, say) gets a message, not an error.
  heterogeneity_fit <- reactive({
    source <- heterogeneity_source()
    if (inherits(source$fit, "error")) shiny::validate(shiny::need(FALSE, if (nrow(df_filtered()) < 2L)
      "Heterogeneity needs at least two effects; widen the selection." else
      paste("Heterogeneity could not be estimated for this selection:", metaUI_reader_reason(conditionMessage(source$fit), ""))))
    source
  })

  output$heterogeneity_reading <- renderUI({
    df <- df_filtered()
    source <- heterogeneity_fit()
    het <- metaUI_heterogeneity(source$fit, df)
    req(!is.na(het$Q_p))
    # A non-significant Q is not evidence of no variation: with few studies the test has little power.
    tau <- if (is.finite(het$total_variance)) paste0(" The typical spread of true effects is &tau; = ", formatC(sqrt(het$total_variance), format = "f", digits = 2),
      if (df$metaUI__es_type[1] == "ZCOR") " on the Fisher <i>z</i> scale" else "", " (the square root of the total &sigma;<sup>2</sup> below).") else ""
    reading <- if (het$Q_p < .05) paste0("Effects vary <b>more</b> than sampling error alone would predict (<i>Q</i> test, <i>p</i> ", fmt_p(het$Q_p), ").") else
      paste0("The <i>Q</i> test does not detect more variation than sampling error would produce (<i>p</i> ", fmt_p(het$Q_p), "); with few studies it has little power to do so.")
    tagList(p(class = "metaui-reading", HTML(paste0(reading, tau))),
      p(class = "metaui-note", if (!is.null(primary_model) && !source$primary) paste0("From the ", source$model, "; the primary model, ", primary_model, ", does not report these variance components.")
        else paste0("From the ", source$model, ".")))
  })

  output$heterogeneity <- renderTable({
    df <- df_filtered()

    het <- metaUI_heterogeneity(heterogeneity_fit()$fit, df)
    # A component the data cannot separate (one study, say) is said to be so, not shown as NA.
    het[c("study_variance", "effect_variance", "total_variance")] <-
      lapply(het[c("study_variance", "effect_variance", "total_variance")], function(x) if (is.na(x)) "not identified" else round_(x, digits = 4))
    het$Q <- round(het$Q, 2)
    het$Q_p <- fmt_p(het$Q_p, include_equal = FALSE)

    # rma.mv variance components are sigma^2 , not tau^2
    het$Q <- formatC(het$Q, format = "f", digits = 2)
    scale <- metaUI_fit_suffix(df)
    names(het) <- c(paste0("&sigma;<sup>2</sup> between studies", scale), paste0("&sigma;<sup>2</sup> within studies", scale), paste0("Total &sigma;<sup>2</sup>", scale), "<i>Q</i>", "<i>p</i> (<i>Q</i>)", "Variance components")
    metaUI_statistics_rows(het)
  }, colnames = FALSE, sanitize.text.function = function(x) x, align = "lr")

  # FOREST PLOT FOR ALL INCLUDED STUDIES ------------------------------------
  output$forest_panel <- renderUI({
    df <- df_filtered()
    if (!nrow(df)) return(p(class = "metaui-reason", role = "status", "No effects selected."))
    div(class = "metaui-export-actions", downloadButton("forest_pdf", "Download PDF (all studies)"),
      downloadButton("forest_png", "Download PNG (this page)"),
      downloadButton("forest_csv", "Download plot data"))
  })
  forest_rows <- reactive({
    df <- df_filtered()
    validate(need(nrow(df) > 0, "No effects selected."))
    metaUI_forest_rows(df, estimatesfiltered(), models_to_run, primary_model)
  })
  # Every page of a download shares the axis of the whole selection.
  forest_plot_for <- function(rows, top_axis = FALSE, family = plot_colours$family, capacity = NULL) metaUI_forest_plot(rows, df_filtered()$metaUI__display_scale[1],
    colours = plot_colours, top_axis = top_axis, family = family, axis_title = metaUI_eff_size_type_label, capacity = capacity,
    limits = range(c(forest_rows()$LCL, forest_rows()$UCL, forest_rows()$es), na.rm = TRUE))
  forest_height <- function(rows) 90 + 20 * rows
  # Downloads follow the order on screen: the PNG is the page shown, the PDF every study, a few dozen effects to a page.
  forest_screen_order <- function() metaUI_forest_order(forest_rows(), forest_studies(), forest_view$sort)
  forest_view <- reactiveValues(page = 1L, sort = "name")
  observeEvent(input$forest_page, {
    page <- suppressWarnings(as.integer(input$forest_page))
    if (length(page) == 1L && !is.na(page)) forest_view$page <- page
  })
  observeEvent(input$forest_sort, {
    forest_view$sort <- if (isTRUE(input$forest_sort %in% c("estimate", "estimate_low"))) input$forest_sort else "name"
    forest_view$page <- 1L
  })
  observeEvent(df_filtered(), { forest_view$page <- 1L; forest_view$found <- NULL })
  # Study names are sent on demand, so long lists cost nothing until searched.
  observeEvent(forest_rows(), updateSelectizeInput(session, "forest_find", server = TRUE, selected = character(),
    choices = sort(unique(as.character(forest_rows()$study[forest_rows()$kind == "Selected effect"])))))
  observeEvent(input$forest_find, {
    req(nzchar(input$forest_find))
    position <- match(input$forest_find, metaUI_forest_order(forest_rows(), forest_studies(), forest_view$sort))
    req(!is.na(position))
    forest_view$found <- input$forest_find
    forest_view$page <- ceiling(position / 50L)
  })
  output$forest_found <- renderUI({
    name <- forest_view$found
    req(name)
    order <- metaUI_forest_order(forest_rows(), forest_studies(), forest_view$sort)
    position <- match(name, order)
    req(!is.na(position))
    pages <- ceiling(length(order) / 50L)
    p(class = "metaui-note metaui-forest-found", paste0(name, if (pages > 1L) paste0(" is on page ", ceiling(position / 50L), " of ", pages, ", marked") else " is marked", " in the table below."))
  })
  forest_studies <- reactive(metaUI_forest_studies(df_agg()))
  forest_details <- reactive(metaUI_effect_details(df_filtered()))
  output$foreststudies <- renderText({
    metaUI_forest_html(forest_rows(), metaUI_eff_size_type_label, studies = forest_studies(), details = forest_details(), page = forest_view$page, sort = forest_view$sort, found = forest_view$found, ratio = ratio_scale)
  })
  output$forest_pdf <- downloadHandler(filename = function() download_name("forest-plot.pdf"), content = function(file) {
    pages <- metaUI_forest_chunks(forest_rows(), forest_screen_order(), <<opts$max_forest_plot_rows>>)
    capacity <- max(vapply(pages, nrow, integer(1)))
    # Cairo embeds the letters of any study name; the plain PDF device lacks some (\\u0131, say).
    # A Mac without XQuartz reports cairo but cannot load it (a warning, and no device opens);
    # the plain device then stands in, and the log says that some letters may be missing.
    before <- grDevices::dev.cur()
    cairo <- isTRUE(capabilities("cairo")) && isTRUE(tryCatch({
      grDevices::cairo_pdf(file, width = 10, height = forest_height(capacity) / 96, onefile = TRUE)
      grDevices::dev.cur() != before
    }, warning = function(w) FALSE, error = function(e) FALSE))
    if (!cairo) {
      if (grDevices::dev.cur() != before) grDevices::dev.off()
      message("Forest plot PDF: cairo could not be loaded, so the plain PDF device is used; some letters in study names may be missing.")
      grDevices::pdf(file, width = 10, height = forest_height(capacity) / 96)
    }
    on.exit(grDevices::dev.off())
    for (rows in pages) print(forest_plot_for(rows, family = "", capacity = capacity))
  })
  output$forest_png <- downloadHandler(filename = function() download_name("forest-plot.png"), content = function(file) {
    order <- forest_screen_order()
    page <- max(1L, min(forest_view$page, ceiling(length(order) / 50L)))
    rows <- metaUI_forest_subset(forest_rows(), order[seq((page - 1L) * 50L + 1L, min(page * 50L, length(order)))])
    ggplot2::ggsave(file, forest_plot_for(rows, top_axis = forest_height(nrow(rows)) > 700), device = "png", width = 10,
      height = forest_height(nrow(rows)) / 96, dpi = 120, limitsize = FALSE)
  })
  output$forest_csv <- downloadHandler(filename = function() download_name("forest-plot-data.csv"), content = function(file) {
    utils::write.csv(forest_rows(), file, row.names = FALSE)
  })

  # FUNNEL PLOT -------------------------------------------------------------

  meta_agg <- reactive({
    df_agg <- df_agg()

    fit <- tryCatch(metaUI_univariate(df_agg$metaUI__effect_size, df_agg$metaUI__se), error = function(e) e)
    # Build the message only on failure: need() evaluates its message eagerly
    if (inherits(fit, "error")) {
      shiny::validate(shiny::need(FALSE, paste(
        "The random-effects model for the funnel plot and Egger test could not be estimated for this selection:", conditionMessage(fit)
      )))
    }
    fit
  })

  output$funnel_key <- renderUI({
    p(class = "metaui-note metaui-key", HTML(paste0("Shaded regions: <i>p</i> &lt; .05 against ", if (ratio_scale) "a ratio of 1" else "zero", ". Vertical dashed line: the random-effects estimate from the study-level effects shown here (",
      metaUI_minus(round_(metaUI_show(as.numeric(meta_agg()$b), metaUI__df), metaUI_digits(meta_agg()$ci.lb, meta_agg()$ci.ub))), ")",
      if (!is.null(primary_model)) paste0(", which can differ from the primary estimate on the Summary, from the ", htmltools::htmlEscape(primary_model)) else "", ". Sloping dashed lines: where 95% of studies with that standard error would fall if they all estimated this effect; they widen as studies get smaller.",
      if (metaUI__df$metaUI__es_type[1] == "ZCOR") " Positions follow the Fisher <i>z</i> scale, on which standard errors do not depend on the effect, so the <i>r</i> ticks are unevenly spaced." else
      if (ratio_scale) " Positions follow the log scale, on which the standard errors are measured, so the ratio ticks are unevenly spaced." else "")))
  })

  output$funnel <- renderPlot({
    plot_colours <- plot_palette() # light or dark, as the browser asks; downloads stay light
    meta_agg <- meta_agg()

    metaUI_funnel(meta_agg, xlab = metaUI_eff_size_type_label, colours = plot_colours, width = session$clientData$output_funnel_width, back = metaUI__df$metaUI__es_type[1])
  }, height = 400, alt = "Funnel plot: each study\\u2019s effect against its standard error, with the pooled estimate and the lines within which 95% of studies would fall without small-study effects. The text below gives Egger\\u2019s test.") # fixed: after an error the collapsed container would otherwise set the next plot height

  output$bias_power_note <- renderUI({
    studies <- length(unique(df_filtered()$metaUI__study_id))
    if (studies < 10L) p(class = "metaui-note metaui-warning-text", paste0("Only ", studies, if (studies == 1L) " study" else " studies",
      " in this selection: these tests have very little power, and Egger\\u2019s test is usually not recommended below 10 studies."))
  })

  output$eggers_reading <- renderUI({
    fit <- meta_agg()
    eggers <- metaUI_egger(as.numeric(fit$yi), sqrt(fit$vi))
    req(!is.null(eggers$estimate))
    p(class = "metaui-reading", HTML(paste0("Funnel plot asymmetry is <b>", if (eggers$p.value < .05) "" else "not ",
      "significant</b> at the 5% level: <i>t</i>\\u00a0=\\u00a0", metaUI_minus(round_(eggers$statistic, 3)), ", <i>p</i>\\u00a0", sub(" ", "\\u00a0", fmt_p(eggers$p.value)), ".")))
  })

  output$eggers <- renderTable({
    meta_agg <- meta_agg()

    eggers <- metaUI_egger(as.numeric(meta_agg$yi), sqrt(meta_agg$vi))
    shiny::validate(shiny::need(!is.null(eggers$estimate),
      "Egger test requires at least three studies."))
    # t and p are in the sentence above.
    metaUI_statistics_rows(data.frame(
      "Intercept" = metaUI_minus(round_(eggers$estimate[1], 3)),
      check.names = FALSE
    ))
  }, colnames = FALSE, sanitize.text.function = function(x) x, align = "lr")


  # PCURVE ------------------------------------------------------------------

  # Phone-width p-curves show their notes and test results as text below the plot.
  pcurve_notes <- reactiveVal(NULL)
  output$pcurve_reading <- renderUI({
    notes <- pcurve_notes()
    req(notes)
    p(class = "metaui-reading", HTML(if (identical(notes$present, "yes")) "The <i>p</i>-curve <b>indicates evidential value</b>: it is right-skewed, as expected when significant results reflect real effects rather than selective reporting alone."
      else if (identical(notes$absent, "yes")) "The <i>p</i>-curve <b>indicates that evidential value is absent or inadequate</b>: it is flatter than expected at 33% power."
      else "The <i>p</i>-curve is <b>inconclusive</b>: it neither shows clear right skew nor rejects 33% power."))
  })
  output$pcurve_notes <- renderUI({
    notes <- pcurve_notes()
    if (!length(notes)) return(NULL)
    stats <- notes$stats
    tagList(
      tags$table(class = "table metaui-custom metaui-pcurve-stats", tags$thead(tags$tr(tags$th("Curve"), tags$th("Test"), tags$th("Result"))),
        tags$tbody(lapply(seq_len(nrow(stats)), function(i) tags$tr(tags$td(HTML(stats$line[i])), tags$td(stats$test[i]), tags$td(HTML(stats$result[i])))))),
      p(class = "metaui-note", HTML(gsub("<br */?>", " ", paste(notes$text, collapse = " ")))),
      tags$details(class = "metaui-help", tags$summary("How to read these tests"), tags$p("Right-skew tests ask whether significant results cluster at very small p-values, as real effects produce. The full test uses every significant result; the half test only those below .025, which are harder to reach by selective reporting; the binomial test compares the share below .025 with the half expected without an effect. The 33% power tests ask whether the curve is flatter than studies with 33% power would produce, which would suggest evidential value is absent or inadequate.")))
  })
  output$pcurve <- renderPlot({
    plot_colours <- plot_palette() # light or dark, as the browser asks; downloads stay light
    pcurve_notes(NULL)
    df <- df_filtered()
    selected <- tryCatch(metaUI_pcurve_data(df), error = function(e) e)
    validate(need(!inherits(selected, "error"), if (inherits(selected, "error")) conditionMessage(selected) else ""))
    message("P-curve direction-contrary exclusions: ", attr(selected, "direction_exclusions"))
    # pcurve() draws as it computes; its drawing goes nowhere, the plot below uses its values.
    grDevices::pdf(NULL)
    result <- tryCatch(pcurve(selected, effect.estimation = FALSE, compact = TRUE, accent = plot_colours$accent), error = function(e) e, finally = grDevices::dev.off())
    validate(need(!inherits(result, "error"), if (inherits(result, "error")) paste("Not estimated:", metaUI_reader_message(conditionMessage(result))) else ""))
    pcurve_notes(list(stats = result$stats, text = c(result$notes, paste0(attr(selected, "direction_exclusions"), " further results were excluded because they ran against the expected direction.")),
      present = result$EvidencePresent, absent = result$EvidenceAbsent))
    metaUI_pcurve_plot(result$PlotData, plot_colours)
  }, width = function() max(420, session$clientData$output_pcurve_width),
    height = function() if (isTRUE(session$clientData$output_pcurve_width < 600)) 380 else 400,
    alt = "P-curve: the share of significant p-values in each range, beside the shares expected with no effect and with 33% power. The text above gives the reading.")


  # ZCURVE ------------------------------------------------------------------


  zcurve_fit <- reactive({
    df <- df_filtered()
    selected <- df[!duplicated(df$metaUI__study_id), , drop = FALSE]
    warnings <- new.env(parent = emptyenv())
    warnings$messages <- character()
    result <- tryCatch(withCallingHandlers(
      zcurve::zcurve(abs(selected$metaUI__effect_size / selected$metaUI__se), bootstrap = FALSE),
      warning = function(w) { warnings$messages <- c(warnings$messages, conditionMessage(w)); invokeRestart("muffleWarning") }),
      error = function(e) e)
    list(result = result, warnings = unique(warnings$messages))
  })

  output$zcurve_warnings <- renderText({
    value <- zcurve_fit()
    paste(if (inherits(value$result, "error")) paste("Not estimated:", metaUI_reader_message(conditionMessage(value$result))) else "",
          if (length(value$warnings)) paste(if (length(value$warnings) == 1L) "Warning:" else "Warnings:", gsub("EDR", "the expected discovery rate", paste(vapply(value$warnings, function(w)
            # zcurve gives this warning below 300 significant results; say so.
            if (startsWith(w, "The z-curve method is meant for large samples")) paste0("Only ", value$result$N_sig, " of the tests are significant. Z-curve is meant for at least 300, so with fewer its expected discovery rate can be biased.") else w,
            character(1)), collapse = "; "), fixed = TRUE)) else "")
  })

  output$zcurve_reading <- renderUI({
    value <- zcurve_fit()
    req(!inherits(value$result, "error"))
    lines <- attr(metaUI_zcurve_plot(value$result, plot_colours), "statistics")
    # The first number is the rate; a confidence interval may follow it.
    numbers <- function(i) as.numeric(regmatches(lines[i], gregexpr("[0-9]*[.][0-9]+", lines[i]))[[1]])
    percent <- function(x) paste0(round(100 * x), "%")
    observed <- numbers(4); expected <- numbers(6)[1]; replicable <- numbers(8)[1]
    verdict <- if (length(observed) < 3L) "" else if (expected < observed[2]) "This is below the observed rate\\u2019s 95% CI, which suggests selective reporting."
      else if (expected <= observed[3]) "This is within the observed rate\\u2019s 95% CI, so it gives no clear sign of selective reporting."
      else "This is above the observed rate\\u2019s 95% CI, so it gives no sign of selective reporting."
    # Below 300 significant results the expected rate can be biased, so the verdict is held back.
    if (nzchar(verdict) && length(value$warnings)) verdict <- paste0(sub("\\\\.$", "", verdict), ", but with only ", value$result$N_sig, " significant results this comparison is unreliable.")
    tagList(p(class = "metaui-reading", HTML(paste0("<b>", percent(observed[1]), "</b> of these tests are significant (observed discovery rate); without selective reporting, the fit expects <b>", percent(expected),
      "</b> (expected discovery rate). ", verdict, " Of the significant results, an estimated <b>", percent(replicable), "</b> would be significant again in exact replications (expected replicability rate)."))),
      p(class = "metaui-note", "The curve is fitted to the significant results only (z above 1.96); the bars below that are shown for comparison. The expected discovery rate is shown without an interval, so treat its comparison with the observed rate as a rough guide.",
        # P-curve, where it applies, reports a power estimate for the same significant results.
        " P-curve\\u2019s power estimate above also concerns the significant results. It assumes they all have the same power, which overstates it when power varies; z-curve allows for this, so the two can differ."))
  })

  output$zcurve_statistics <- renderUI({
    value <- zcurve_fit()
    req(!inherits(value$result, "error"))
    lines <- attr(metaUI_zcurve_plot(value$result, plot_colours), "statistics")
    # The package pairs each label with the value on the next line.
    labels <- c(sub(":$", "", lines[c(3, 5, 7)]))
    # Rates as percentages, as in the sentence above.
    values <- gsub("  ", ", ", lines[c(4, 6, 8)])
    values <- vapply(values, function(line) {
      found <- gregexpr("[0-9]*[.][0-9]+", line)
      regmatches(line, found) <- list(paste0(round(100 * as.numeric(regmatches(line, found)[[1]])), "%"))
      line
    }, character(1), USE.NAMES = FALSE)
    tags$table(class = "table metaui-custom metaui-stats metaui-zstats", tags$tbody(
      tags$tr(tags$td(HTML("Range of <i>z</i>-scores")), tags$td(class = "num", sub("^Range: ", "", lines[1]),
        { top <- max(ggplot2::layer_scales(metaUI_zcurve_plot(value$result, plot_colours))$x$get_limits()); if (max(value$result$data) > top) paste0(" (plot ends at ", format(top), ")") else "" })),
      tags$tr(tags$td("Tests"), tags$td(class = "num", lines[2])),
      lapply(seq_along(labels), function(i) tags$tr(tags$td(labels[i]), tags$td(class = "num", values[i])))))
  })

  output$zcurve <- renderPlot({
    plot_colours <- plot_palette() # light or dark, as the browser asks; downloads stay light
    value <- zcurve_fit()
    # The reason is given under the heading.
    validate(need(!inherits(value$result, "error"), ""))
    metaUI_zcurve_plot(value$result, plot_colours, narrow = isTRUE(session$clientData$output_zcurve_width < 560))
  }, width = function() max(420, session$clientData$output_zcurve_width), height = 400,
    alt = "Z-curve: a histogram of the studies\\u2019 z-scores with the fitted curve. The text above gives its estimates.")

  # DISTRIBUTION OF EFFECTS -------------------------------------------------
  output$distribution_note <- renderUI({
    centre <- mean(metaUI__df$metaUI__effect_size)
    # The mean stays in the formula even when it rounds to zero, so that readers see it is subtracted.
    numerator <- paste0("(effect ", if (centre < 0) "+" else metaUI_minus("-"), " ", formatC(abs(centre), format = "f", digits = 2), ")")
    p(class = "metaui-note", HTML(paste0("The Exclude extreme effects filter uses <i>z</i> = (effect ", metaUI_minus("-"), " mean of all effects in the full dataset) / their SD = ", numerator, " / ",
      round_(sd(metaUI__df$metaUI__effect_size), 2), if (nzchar(metaUI_scale(metaUI__df$metaUI__es_type[1])$fit)) paste0(", on the ", metaUI_scale(metaUI__df$metaUI__es_type[1])$fit, " scale") else "", ".")))
  })

  # The dots on the plot, as a list that needs no pointer.
  output$outlier_list <- renderUI({
    df <- df_filtered()
    effects <- df$metaUI__effect_size
    qs <- stats::quantile(effects, c(.25, .75))
    flagged <- df[effects < qs[[1]] - 1.5 * diff(qs) | effects > qs[[2]] + 1.5 * diff(qs), , drop = FALSE]
    if (!nrow(flagged)) return(p(class = "metaui-note", "No effects lie beyond 1.5 IQR."))
    flagged <- flagged[order(flagged$metaUI__effect_size), , drop = FALSE]
    # The stored z, which the z-score filter uses, so that the list explains that filter.
    z <- flagged$metaUI__es_z
    shown <- metaUI_show(flagged$metaUI__effect_size, df)
    # Sample size and sampling interval tell a small, noisy study from a precise one far from the rest.
    lower <- metaUI_show(flagged$metaUI__effect_size - stats::qnorm(.975) * flagged$metaUI__se, df)
    upper <- metaUI_show(flagged$metaUI__effect_size + stats::qnorm(.975) * flagged$metaUI__se, df)
    n <- flagged$metaUI__N
    has_n <- any(is.finite(n))
    two <- function(x) metaUI_minus(formatC(x, format = "f", digits = 2))
    tags$details(class = "metaui-help", tags$summary(paste0("List the ", nrow(flagged), if (nrow(flagged) == 1L) " effect" else " effects", " beyond 1.5 IQR")),
      div(class = "metaui-scroll", tags$table(class = "table metaui-custom metaui-stats metaui-outliers",
        tags$thead(tags$tr(tags$th("Study"), if (has_n) tags$th(class = "num", HTML("<i>N</i>")), tags$th(class = "num", "Effect"), tags$th(class = "num", "95% CI"), tags$th(class = "num", HTML("<i>z</i>")))),
        tags$tbody(lapply(seq_len(nrow(flagged)), function(i) tags$tr(tags$td(as.character(flagged$metaUI__study_id[i])),
          if (has_n) tags$td(class = "num", if (is.finite(n[i])) format(round(n[i], 1), big.mark = ",") else ""),
          tags$td(class = "num", two(shown[i])), tags$td(class = "num", paste0("[", two(lower[i]), ", ", two(upper[i]), "]")),
          tags$td(class = "num", two(z[i]))))))),
      p(class = "metaui-note", "95% CI: the effect\\u2019s normal sampling interval, without shrinkage. A wide interval marks an imprecise estimate; a narrow one far from the rest is more likely to matter."))
  })

  output$violin <- plotly::renderPlotly({
    plot_colours <- plot_palette() # light or dark, as the browser asks; downloads stay light
    df <- df_filtered()
    effects <- df$metaUI__effect_size

    # Outliers follow the boxplot rule: quartiles +/- 1.5 * IQR
    qs <- quantile(effects, c(.25, .5, .75))
    bounds <- c(qs[[1]] - 1.5 * diff(qs[c(1, 3)]), qs[[3]] + 1.5 * diff(qs[c(1, 3)]))
    outliers <- df[effects < bounds[1] | effects > bounds[2], , drop = FALSE]
    # Which effects stand out is settled on the fitting scale; the plot shows them as readers read them.
    z <- outliers$metaUI__es_z # the z the filter uses
    # Ratios are binned and drawn on the log scale, with the axis and values given as ratios.
    if (!ratio_scale) {
      effects <- metaUI_show(effects, df); qs[] <- metaUI_show(qs, df)
      outliers$metaUI__effect_size <- metaUI_show(outliers$metaUI__effect_size, df)
    }
    as_read <- function(x) if (ratio_scale) exp(x) else x

    breaks <- pretty(effects, n = 24)
    counts <- graphics::hist(effects, breaks = breaks, plot = FALSE)$counts
    span <- range(breaks) + c(-1, 1) * diff(range(breaks)) * .02
    # Ticks carry the decimals their spacing needs, so small ranges keep distinct labels.
    ticks <- if (ratio_scale) log(metaUI_ratio_ticks(span, 6)) else pretty(span, 6); ticks <- ticks[ticks >= span[1] & ticks <= span[2]]
    tick_digits <- max(0L, ceiling(-log10(diff(ticks)[1] * (1 + 1e-9))))
    tick_text <- if (ratio_scale) metaUI_tick_labels(exp(ticks)) else metaUI_minus(formatC(ticks, format = "f", digits = tick_digits))
    # The axis ends on a labelled tick above the tallest bar.
    top <- max(pretty(c(0, max(counts)), 5))
    ink <- plot_colours$ink
    axis_font <- list(size = 13, color = plot_colours$slate)
    strip <- top * 1.13

    p <- plotly::plot_ly(x = (breaks[-1] + breaks[-length(breaks)]) / 2, y = counts, width = diff(breaks), type = "bar",
        marker = list(color = plot_colours$bars, line = list(color = plot_colours$background, width = 1)),
        customdata = as_read((breaks[-1] + breaks[-length(breaks)]) / 2), hovertemplate = "%{y} effects around %{customdata:.2f}<extra></extra>", showlegend = FALSE) %>%
      plotly::add_trace(x = outliers$metaUI__effect_size, y = rep(strip, nrow(outliers)), type = "scatter", mode = "markers", inherit = FALSE, showlegend = FALSE,
        marker = list(color = plot_colours$slate, size = 6), text = paste0(outliers$metaUI__study_id, ": ", round(as_read(outliers$metaUI__effect_size), 2), " (z = ", round(z, 2), ")"),
        hovertemplate = "%{text}<extra></extra>") %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(bargap = 0, font = list(family = metaui_plotly_font, color = ink),
        paper_bgcolor = plot_colours$background, plot_bgcolor = plot_colours$background,
        margin = list(l = 44, r = 12, t = 8, b = 48),
        shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = top * 1.05, line = list(color = plot_colours$slate, width = 1), layer = "above"),
          list(type = "line", x0 = qs[[1]], x1 = qs[[3]], y0 = strip, y1 = strip, line = list(color = plot_colours$slate, width = 5)),
          list(type = "line", x0 = qs[[2]], x1 = qs[[2]], y0 = strip - top * .03, y1 = strip + top * .03, line = list(color = ink, width = 2))),
        annotations = list(
          list(x = qs[[2]], y = strip, yshift = 9, text = paste0("middle 50%, median ", metaUI_minus(round_(as_read(qs[[2]]), 2))), showarrow = FALSE, xanchor = "center", yanchor = "bottom", font = list(size = 13, color = plot_colours$slate)),
          if (any(outliers$metaUI__effect_size < qs[[1]])) list(x = min(outliers$metaUI__effect_size), y = strip, yshift = -9, text = "beyond 1.5 IQR", showarrow = FALSE, xanchor = "left", yanchor = "top", font = list(size = 13, color = plot_colours$slate)),
          if (any(outliers$metaUI__effect_size > qs[[3]])) list(x = max(outliers$metaUI__effect_size), y = strip, yshift = -9, text = "beyond 1.5 IQR", showarrow = FALSE, xanchor = "right", yanchor = "top", font = list(size = 13, color = plot_colours$slate))),
        xaxis = list(title = list(text = metaUI_label_html(metaUI_eff_size_type_label), font = list(size = 13.5)), range = span, tickmode = "array", tickvals = ticks, ticktext = tick_text, tickfont = axis_font, zeroline = FALSE, showgrid = FALSE, fixedrange = TRUE, showline = TRUE, linecolor = plot_colours$slate, linewidth = 1, ticks = "outside", ticklen = 3, tickcolor = plot_colours$slate),
        yaxis = list(title = list(text = "Effects", font = list(size = 13.5)), tickfont = axis_font, gridcolor = plot_colours$grid, zeroline = FALSE, fixedrange = TRUE, range = c(0, top * 1.3), rangemode = "normal", tick0 = 0, tickvals = pretty(c(0, top), 5)[pretty(c(0, top), 5) <= top]))
    p
  })

  # DOWNLOAD ----------------------------------------------------------------

  data_list <- reactive({
    req(is.null(state_values$pending_upload_filters))
    upload_info <- attr(df_filtered(), "metaUI_upload_info")
    filter_selections <- attr(df_filtered(), "metaUI_applied_filters")

    list(
      instructions = metaUI_workbook_instructions(metaUI__df),
      dataset = metaUI_workbook_dataset(attr(df_filtered(), "metaUI_source_data"), metaUI__df),
      summary = if (nrow(df_filtered())) as.data.frame(estimatesfiltered()) else data.frame(status = "unsupported", reason = "No eligible rows selected"),
      filters = filter_selections,
      computations = if (length(metaUI_computation_specs(metaUI__df)) && nrow(df_filtered())) computations_comparison() else data.frame(computation = "As supplied"),
      bayesian_sensitivity = if (!is.null(bayesian_options) && nrow(df_filtered())) bayesian_sensitivity() else data.frame(status = "not enabled"),
      equivalence = if (nrow(df_filtered())) interval_assessment() else data.frame(assessment = "Not assessed", reason = "No eligible rows selected",
        bound = if (is.null(bound <- tryCatch(chosen_bound(), error = function(e) NULL))) NA_real_ else bound),
      provenance = data.frame(field = c("uploaded", "file", "z_rule", "primary_model"),
        value = c(as.character(!is.null(upload_info)),
          if (is.null(upload_info)) "built dataset" else upload_info$file,
          if (is.null(upload_info)) "prepare_data descriptive standardisation" else upload_info$z_rule,
          if (is.null(primary_model)) "none chosen" else primary_model))
    )
  })


  workbook_name <- function() paste0(Sys.Date(), "-", gsub("^_+|_+$", "", gsub("[^A-Za-z0-9-]+", "_", dataset_name)), "-metaUI-data.xlsx")

  output$executeDownload <- downloadHandler(
    filename = function() {
      workbook_name()
    },
    content = function(file) {
      writexl::write_xlsx(data_list(), file)
    }
  )

  # The visible action triggers this hidden link through JavaScript.
  outputOptions(output, "executeDownload", suspendWhenHidden = FALSE)

  observeEvent(input$downloadData, {
    if (!is.null(state_values$pending_upload_filters)) {
      showModal(modalDialog(title = "Restoring filters", "Wait until the uploaded selection has been analyzed."))
      return()
    }
    if (state_values$ever_analyzed == TRUE) {
      shinyjs::runjs("$(\'#executeDownload\')[0].click();")
    } else {
      showModal(modalDialog(title = "Not analyzed yet", HTML("The download includes model results; click <i>Update results</i> first.")))
    }

}
)

  # REPRODUCTION CODE -------------------------------------------------------
  # Script for the applied selection; it reads the workbook of the same analysis.
  reproduction_code <- function(format) {
    req(is.null(state_values$pending_upload_filters))
    df <- df_filtered()
    metaUI_reproduction_code(format, applied = attr(df, "metaUI_applied_filters"), filters = filters,
      source = metaUI_workbook_dataset(attr(df, "metaUI_source_data"), metaUI__df), built = metaUI__df, models = models_to_run,
      estimates = if (nrow(df)) estimatesfiltered() else NULL,
      settings = list(dataset_name = dataset_name, workbook = workbook_name(), primary_model = primary_model,
        correlation = correlation_dependent, aggregation = aggregation_method[1],
        bound = tryCatch(chosen_bound(), error = function(e) NULL), known_models = known_interval_models,
        upload = attr(df, "metaUI_upload_info"), selected_rows = nrow(df),
        exported = format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
      app_dir = metaUI_app_dir, env = environment(metaUI_fit_models))
  }
  code_name <- function(extension) sub("metaUI-data[.]xlsx$", paste0("metaUI-selection.", extension), workbook_name())
  # Other downloads share the date and data name of the workbook, so that files from two apps or days stay apart.
  download_name <- function(what) sub("metaUI-data[.]xlsx$", paste0("metaUI-", what), workbook_name())
  output$executeCodeDownload <- downloadHandler(filename = function() code_name("R"),
    content = function(file) writeLines(reproduction_code("R"), file, useBytes = TRUE))
  output$executeQuartoDownload <- downloadHandler(filename = function() code_name("qmd"),
    content = function(file) writeLines(reproduction_code("qmd"), file, useBytes = TRUE))
  outputOptions(output, "executeCodeDownload", suspendWhenHidden = FALSE)
  outputOptions(output, "executeQuartoDownload", suspendWhenHidden = FALSE)
  # Same gate as the workbook, which the code needs.
  request_code <- function(target) {
    if (!is.null(state_values$pending_upload_filters)) {
      showModal(modalDialog(title = "Restoring filters", "Wait until the uploaded selection has been analyzed."))
    } else if (isTRUE(state_values$ever_analyzed)) {
      shinyjs::runjs(paste0("$(\'#", target, "\')[0].click();"))
    } else {
      showModal(modalDialog(title = "Not analyzed yet", HTML("The code reproduces model results; click <i>Update results</i> first.")))
    }
  }
  observeEvent(input$downloadCode, request_code("executeCodeDownload"))
  observeEvent(input$downloadQuarto, request_code("executeQuartoDownload"))

  # UPLOAD ----------------------------------------------------------------

  observeEvent(input$executeUpload, {
    if (is.null(input$uploadData)) {
      showModal(modalDialog(title = "No file selected", "Choose a workbook first."))
      return()
    }
    upload <- tryCatch({
      # The reader sees the file problem, not a server path inside a package error.
      sheets <- tryCatch(readxl::excel_sheets(input$uploadData$datapath),
        error = function(e) stop("This file is not an Excel workbook (.xlsx). Upload a workbook downloaded from this app.", call. = FALSE))
      if (!all(c("dataset", "filters") %in% sheets)) stop("The workbook needs the dataset and filters sheets of a workbook downloaded from this app.", call. = FALSE)
      df <- readxl::read_xlsx(input$uploadData$datapath, "dataset")
      df <- metaUI_validate_upload(df, metaUI__df)
      new_levels <- list()
      <FILTER>
      for (i in filters) {
        label <- sub("^metaUI__filter_", "", i$col)
        if (i$type == "numeric") {
          value <- suppressWarnings(as.numeric(df[[i$col]]))
          text <- !is.na(df[[i$col]]) & trimws(as.character(df[[i$col]])) != "" & is.na(value)
          if (any(text)) stop("The filter column \\"", label, "\\" must hold numbers or be empty (not in ", metaUI_upload_rows(text), ").", call. = FALSE)
          df[[i$col]] <- value
        }
        if (i$type == "selection") {
          value <- as.character(df[[i$col]])
          value[is.na(value) | trimws(value) == ""] <- "(Missing)"
          # New categories (added studies, say) join the filter after the authors\' categories.
          built_levels <- levels(metaUI__df[[i$col]])
          added <- setdiff(unique(value), built_levels)
          added <- c(sort(setdiff(added, "(Missing)"), method = "radix"), intersect(added, "(Missing)"))
          if (length(added)) new_levels[[i$id]] <- added
          df[[i$col]] <- factor(value, levels = c(built_levels, added))
        }
      }
      </FILTER>
      attr(df, "metaUI_runtime_rows") <- nrow(df)
      selections <- readxl::read_xlsx(input$uploadData$datapath, "filters")
      if (!all(c("id", "selection") %in% names(selections))) stop("Invalid filter sheet.")
      filter_values <- split(selections, selections$id)
      numeric_ids <- "outliers_z_scores"
      <FILTER>
      for (i in filters) {
        if (!i$id %in% names(filter_values) && i$type == "numeric") stop("Missing saved filter: ", i$id)
        if (!i$id %in% names(filter_values)) filter_values[[i$id]] <- data.frame(selection = character())
        if (i$type == "numeric") numeric_ids <- c(numeric_ids, i$id)
        else {
        # New categories are selected, so that added studies enter the analysis.
        filter_values[[i$id]] <- data.frame(selection = c(filter_values[[i$id]]$selection[!is.na(filter_values[[i$id]]$selection)], new_levels[[i$id]]))
        if (any(!filter_values[[i$id]]$selection %in% levels(df[[i$col]]))) stop("The filters sheet names a category that is not in the data.", call. = FALSE)
      }
      }
      </FILTER>
      # A range left at its full extent means "everything": it widens to take in added values.
      full <- metaUI_full_selection(metaUI__df, filters)
      columns <- c(outliers_z_scores = "metaUI__es_z", stats::setNames(vapply(filters, function(i) i$col, character(1)), vapply(filters, function(i) i$id, character(1))))
      for (id in numeric_ids) {
        values <- suppressWarnings(as.numeric(filter_values[[id]]$selection))
        if (length(values) != 2L || any(!is.finite(values)) || values[1] > values[2]) stop("The filters sheet has an invalid range for ", id, ".", call. = FALSE)
        observed <- df[[columns[[id]]]][is.finite(df[[columns[[id]]]])]
        if (length(full[[id]]) == 2L && all(abs(values - full[[id]]) <= 1e-8 * pmax(1, abs(full[[id]]))) && length(observed))
          filter_values[[id]] <- data.frame(selection = as.character(c(min(values[1], observed), max(values[2], observed))))
      }
      if (length(metaUI_computation_specs(metaUI__df))) {
        value <- filter_values$computation$selection
        if (is.null(value)) value <- "primary"
        if (length(value) != 1L || is.na(value) || !value %in% metaUI_computations(metaUI__df)) stop("Unknown saved computation.")
        filter_values$computation <- data.frame(selection = value)
      }
      list(data = df, filters = filter_values, new_levels = new_levels, bound = metaUI_saved_bound(input$uploadData$datapath, sheets))
    }, error = function(e) e)
    if (inherits(upload, "error")) {
      showModal(modalDialog(title = "Invalid upload", conditionMessage(upload)))
      return()
    }
    attr(fit_cached, "clear")()
    state_values$uploaded_data <- upload$data
    shinyjs::removeClass(selector = ".metaui-no-missing", class = "metaui-no-missing")
    derivation <- attr(upload$data, "metaUI_upload_derivations")
    # The upload number distinguishes files with identical metadata.
    state_values$upload_count <- if (is.null(state_values$upload_count)) 1L else state_values$upload_count + 1L
    upload_number <- state_values$upload_count
    labels <- sub("^metaUI__filter_", "", vapply(filters, function(i) i$col, character(1)))[match(names(upload$new_levels), vapply(filters, function(i) i$id, character(1)))]
    state_values$upload_info <- list(file = basename(input$uploadData$name), rows = nrow(upload$data), upload_number = upload_number,
      z_rule = derivation$rule, z_disagreements = derivation$supplied_z_disagreements,
      checkbox_note = "Missing-value choices restored (older files: missing values included).",
      changes = paste0(if (derivation$recomputed_rows > 0) paste0("Derived columns recomputed from the edited ones in ", derivation$recomputed_rows, if (derivation$recomputed_rows == 1) " row. " else " rows. ") else "",
        if (derivation$added_ids > 0) paste0(derivation$added_ids, if (derivation$added_ids == 1) " effect ID added. " else " effect IDs added. ") else "",
        if (length(upload$new_levels)) paste0("New categories, selected: ", paste(paste0(labels, ": ", vapply(upload$new_levels, paste, character(1), collapse = ", ")), collapse = "; "), ". ") else ""))
    # The bound does not refit, so it can be set before the restored analysis.
    updateNumericInput(session, "sesoi", value = upload$bound)
    restore_filters(upload$filters, upload$data, "upload")
  })

  changed_choices <- new.env()
  restore_filters <- function(filter_values, restored_data, source) {
    state_values$restore_source <- source
    <FILTER>
    for (i in filters) {
      if (i$type == "numeric") {
        flag <- filter_values[[paste0(i$id, "_include_NA")]]$selection
        include <- if (is.null(flag)) TRUE else identical(toupper(as.character(flag[1])), "TRUE")
        updateCheckboxInput(inputId = paste0(i$id, "_include_NA"), value = include)
        selection <- as.numeric(filter_values[[i$id]]$selection[1:2])
        limits <- range(c(restored_data[[i$col]], selection), na.rm = TRUE)
        step <- if (diff(selection) > 0) diff(selection) / 1000 else 1e-8
        updateSliderInput(inputId = i$id,
          min = selection[1] - step * ceiling((selection[1] - min(limits)) / step),
          max = selection[2] + step * ceiling((max(limits) - selection[2]) / step),
          step = step, value = selection)
      } else {
        # Uploaded data can add categories; the choices follow the data, and return to the authors\' afterwards.
        levels_now <- levels(restored_data[[i$col]])
        new_choices <- !identical(levels_now, levels(metaUI__df[[i$col]])) || isTRUE(changed_choices[[i$id]])
        changed_choices[[i$id]] <- !identical(levels_now, levels(metaUI__df[[i$col]]))
        if (length(levels(metaUI__df[[i$col]])) >= <<opts$selection_list_threshold>>) {
          if (new_choices) shinyWidgets::updatePickerInput(session, inputId = i$id, choices = levels_now, selected = filter_values[[i$id]]$selection)
          else shinyWidgets::updatePickerInput(session, inputId = i$id, selected = filter_values[[i$id]]$selection)
        } else {
          if (new_choices) updateCheckboxGroupInput(inputId = i$id, choices = levels_now, selected = filter_values[[i$id]]$selection)
          else updateCheckboxGroupInput(inputId = i$id, selected = filter_values[[i$id]]$selection)
        }
      }
    }
    </FILTER>
    z_selection <- as.numeric(filter_values[["outliers_z_scores"]]$selection[1:2])
    z_limits <- range(c(restored_data$metaUI__es_z, z_selection))
    z_step <- if (diff(z_selection) > 0) diff(z_selection) / 1000 else 1e-8
    updateSliderInput(inputId = "outliers_z_scores",
      min = z_selection[1] - z_step * ceiling((z_selection[1] - min(z_limits)) / z_step),
      max = z_selection[2] + z_step * ceiling((max(z_limits) - z_selection[2]) / z_step),
      step = z_step, value = z_selection)
    if (length(metaUI_computation_specs(metaUI__df))) updateSelectInput(session, "computation",
      selected = filter_values$computation$selection)
    state_values$restore_started <- as.numeric(Sys.time())
    state_values$pending_upload_filters <- filter_values
  }


  # Wait for browser input bindings to acknowledge every restored selection.
  observe({
    saved <- state_values$pending_upload_filters
    req(!is.null(saved))
    invalidateLater(250, session)
    if (as.numeric(Sys.time()) - state_values$restore_started > 5) {
      state_values$pending_upload_filters <- NULL
      state_values$link_notice <- if (state_values$restore_source == "upload") "Uploaded data not analyzed: check the filters and click Update results. Downloads still contain the previous analysis." else "Linked selection not restored: check the filters and click Update results."
      return()
    }
    same_numbers <- function(x, y) {
      x <- as.numeric(x); y <- as.numeric(y)
      length(x) == length(y) && all(is.finite(x)) && all(abs(x - y) <= 1e-8 * pmax(1, abs(y)))
    }
    matches <- same_numbers(input$outliers_z_scores, saved[["outliers_z_scores"]]$selection[1:2])
    <FILTER>
    for (i in filters) {
      if (i$type == "numeric") {
        matches <- matches && same_numbers(input[[i$id]], saved[[i$id]]$selection[1:2])
        flag <- saved[[paste0(i$id, "_include_NA")]]$selection
        include <- if (is.null(flag)) TRUE else identical(toupper(as.character(flag[1])), "TRUE")
        matches <- matches && identical(input[[paste0(i$id, "_include_NA")]], include)
      } else {
        matches <- matches && setequal(input[[i$id]], as.character(saved[[i$id]]$selection))
      }
    }
    </FILTER>
    if (length(metaUI_computation_specs(metaUI__df))) matches <- matches && identical(input$computation,
      as.character(saved$computation$selection))
    req(matches)
    state_values$pending_upload_filters <- NULL
    shinyjs::runjs("$(\'#go\')[0].click();")
  })


  output$link_notice <- renderText(if (isTRUE(state_values$link_failed) || isTRUE(state_values$link_stale)) NULL else state_values$link_notice)
  output$link_banner <- renderUI(if (isTRUE(state_values$link_failed) || isTRUE(state_values$link_stale)) div(class = "metaui-link-banner", role = "alert", state_values$link_notice))
  session$onFlushed(function() {
    isolate({
    query <- session$clientData$url_search
    if (is.null(query) || !nzchar(query)) return()
    # URL changes after Analyze are publication of state, not a new restore.
    if (state_values$ever_analyzed || !is.null(state_values$uploaded_data)) return()
    restored <- tryCatch(metaUI_parse_selection(query, metaUI_dataset_key, metaUI__df, filters), error = function(e) e)
    if (inherits(restored, "error")) {
      state_values$link_notice <- "This link is incomplete or damaged, so its selection could not be restored. The results show every effect; set the filters yourself or ask for a new link."
      state_values$link_failed <- TRUE
      return()
    }
    if (is.null(restored)) return()
    updateNumericInput(session, "sesoi", value = if (is.null(restored$sesoi)) NA_real_ else restored$sesoi)
    restore_filters(restored$filters, metaUI__df, "link")
    state_values$link_notice <- if (isTRUE(restored$stale)) paste0("The data have been updated since this link was made. Its selection was applied as far as it still fits",
      if (length(restored$adjusted)) paste0(" (", paste(restored$adjusted, collapse = "; "), ")") else "", ", so results can differ from those first shared.")
      else "Selection restored from link."
    state_values$link_stale <- isTRUE(restored$stale)
    })
  }, once = TRUE)
  selection_query <- reactive({
    req(isTruthy(input$go))
    if (!is.null(attr(df_filtered(), "metaUI_upload_info"))) return(NULL)
    bound <- tryCatch(chosen_bound(), error = function(e) NULL)
    metaUI_selection_query(attr(df_filtered(), "metaUI_applied_filters"), metaUI_dataset_key, bound, metaUI__df, filters)
  })
  observeEvent(list(input$go, input$sesoi), {
    req(isTruthy(input$go))
    # The opening analysis is the default selection, so the address stays as the reader typed it.
    if (opening_run() && identical(as.integer(input$go), 1L) && !isTruthy(input$sesoi)) return()
    query <- tryCatch(selection_query(), error = function(e) NULL)
    updateQueryString(if (is.null(query)) "?" else query, mode = "replace", session = session)
  }, ignoreInit = TRUE)
  output$share_selection <- renderUI({
    req(isTruthy(input$go))
    if (!is.null(attr(df_filtered(), "metaUI_upload_info"))) return(p(class = "metaui-note",
      "Links cannot include uploaded data; share the workbook instead."))
    query <- tryCatch(selection_query(), error = function(e) e)
    if (inherits(query, "error")) return(div(class = "metaui-share", p(class = "metaui-note", conditionMessage(query))))
    div(class = "metaui-share", tags$button(type = "button", class = "btn btn-default metaui-copy-link", `data-href` = query,
      title = "Copies a link that reopens the app with this selection", icon("link"), "Copy link"),
      tags$span(class = "metaui-copy-status", role = "status"))
  })

}

')

if (!any_filters) {
  glue_string <- glue_string %>%
    stringr::str_remove_all(regex("\\<FILTER\\>.*?\\</FILTER\\>", dotall = TRUE))
} else {
  glue_string <- glue_string %>%
    stringr::str_remove_all("\\<FILTER\\>") %>%
    stringr::str_remove_all("\\</FILTER\\>")
}

glue::glue(.open = "<<", .close = ">>",
           glue_string)

}
