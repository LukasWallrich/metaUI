# Wrap in function so that it can be saved more easily

labels_and_options <- function(dataset_name, correlation = .6, primary_model = NULL, bayesian = FALSE, correlations = FALSE, custom_models = FALSE, effects = NA, studies = NA, strip = "", citation = "", intro = "") {
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
      not_analysed <- HTML(paste0("<p class=\'metaui-empty-title\'>{effects} effects from {studies} studies</p>", {paste(deparse(strip), collapse = "\n")}, "<p class=\'metaui-fitting\'>Each dot is one effect. Fitting the models\u2026</p>", {paste(deparse(if (nzchar(citation)) paste0("<p class=\'metaui-note\'>", citation, "</p>") else ""), collapse = "\n")}))

      fitting_note <- HTML("<p class=\'metaui-fitting\'>Fitting the models\u2026</p>")

      summary_overview_main <- HTML("<h3>Selected sample</h3>")
      summary_table_main <- HTML("<h3>Effect size estimates</h3>")
       # Confidence level needs to be changed in all entries in model.R if you want to adjust it
      summary_table_notes <- HTML(glue::glue("<p>CI = <<ci_width>> confidence interval; its actual coverage depends on each model\\\'s assumptions. <i>k</i> is the number of effects or studies the model reports.{if (bayesian) " Bayesian rows show central credible intervals." else ""}</p>", .open = "<<", .close = ">>"))
      model_help <- HTML("<h4>Default models</h4><dl>
        <dt>Random-Effects Multilevel Model</dt><dd>metafor::rma.mv with REML, random effects for studies and for effects within studies, independent sampling errors and t-based intervals. Uses every selected effect.</dd>
        <dt>Robust Variance Estimation</dt><dd>robumeta::robu with correlated-effects weights (rho = .8) and no small-sample correction, so intervals can be too narrow with few studies.</dd>
        <dt>Trim-and-fill</dt><dd>Duval-Tweedie L0 common-effect trimming followed by metafor ML/Hartung-Knapp on study-level effects (random effects, ML tau-squared, Hartung-Knapp intervals). <i>k</i> includes imputed studies.</dd>
        <dt>P-uniform star; Hedges-Vevea Selection Model</dt><dd>Selection models on study-level effects. They need a declared expected direction and are reported as unsupported without one.</dd>
        <dt>Precision Effect Test (PET); PEESE</dt><dd>Weighted regressions of study-level effects on their standard error (PET) or variance (PEESE); the intercept is reported.</dd>
        </dl>{if (custom_models) "<p>Rows not described here were added by the authors; their code is in models.R in the app source.</p>" else ""}")

      sample_overview_main <- HTML("<h3>Sample breakdown</h3>")
      sample_table <- HTML("<h3>Effects and studies</h3>")
      study_details_hint <- HTML("<p class=\'metaui-note\'>Select a study name for its reference and, where the authors could include it, its abstract.</p>")
      sample_moderation_main <- HTML("<h3>Simple tests of moderation</h3>")
      sample_moderation_notes <- HTML("<p class=\'metaui-note\'>One moderator at a time; correlations between moderators are ignored, so these tests are exploratory. Models are fitted by maximum likelihood. <i>k</i> = number of effects.</p>")

      firstvalues <- HTML("<p class=\'metaui-note\'>P-curve and z-curve use the first effect per study, an exploratory selection rule applied before direction screening. P-curve excludes and counts effects that are zero or opposite to the expected direction. Both use normal Wald statistics, not the source p-values.</p>")

      qrppb_main <- HTML("<h3>Publication bias and selective reporting</h3><p class=\'metaui-note\'>Exploratory small-study and selection diagnostics. A non-significant result does not show that there is no bias: these tests often have little power.</p>")
      funnel_main <- HTML("<h4>Funnel plot of study-level effects</h4>")

      eggers_main <- HTML("<h4>Egger\'s test of funnel plot asymmetry</h4>")

      pcurve_main <- HTML("<h4>P-curve of effects</h4>")

      zcurve_main <- HTML("<h4>Z-curve of effects</h4>")

      diagnostics_main <- HTML("<h3>Distribution of effect sizes</h3><p class=\'metaui-note\'>Dots mark effects more than 1.5 interquartile ranges (IQR) beyond the middle 50%. Hover over or tap a dot for its study and <i>z</i>-score; the list below the plot gives them all.</p>")
      diagnostics_het <- HTML("<h3>Heterogeneity (REML multilevel model)</h3>")

      forest_note <- HTML("<p class=\'metaui-note\'>Effects: observed, with normal 95% sampling intervals and no shrinkage. Combined rows: the study-level effects used by the study-level models (see Methods); with correlated effects of unequal precision they can lie away from the simple average. Summaries: the primary model, where the authors declared one, with multilevel and robust variance estimation (RVE). Effects keep their numbers from the full dataset, so a selection can leave gaps.{if (correlations) " Correlations are shown as r; the plot data also include fitting-scale values." else ""}</p>")
      zscore_help <- HTML("<p class=\'metaui-help-text\'>z = (effect \\u2212 mean of all effects in the full dataset) / their SD, on the fitting scale. This is a simple standardization, not a model-based outlier test; Outlier diagnostics gives the values used.</p>")
      # Result tabs show not_analysed until the first analysis has finished. A tab with
      # precomputed content (opening = TRUE) shows it from the first paint instead.
      # Only the Summary tab carries the strip of effects, so the page holds it once.
      results_panel <- function(..., opening = FALSE, placeholder = fitting_note) if (opening) tagList(...) else tagList(
        conditionalPanel("!output.metaui_ready", div(class = "metaui-empty", role = "status", placeholder)),
        conditionalPanel("output.metaui_ready && output.no_effects", div(class = \'metaui-empty\', role = \'status\', p(class = \'metaui-empty-title\', \'No effects match this selection\'), p(\'Change the filters, or reset them to select every effect again.\'))),
        conditionalPanel("output.metaui_ready && !output.no_effects", ...))
      data_help <- HTML("<p class=\'metaui-help-text\'>The workbook holds all rows (not only the selection), the last applied filters and the last results.{if (bayesian) " With Bayesian models, it waits for two extra prior-sensitivity fits." else ""}<br/>Upload is for reanalysis: edit a downloaded workbook and upload it to analyze your version. Uploads must be workbooks from this app, with the same columns and effect scale; rows may be edited. Saved filters and the equivalence bound are restored and analyzed.</p><p class=\'metaui-help-text\'>The R script and Quarto file reproduce the applied selection and its model rows from the workbook of the same analysis, without metaUI. They do not reproduce the plots or diagnostic tabs.</p>")


  ')
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
      metaUI_slider_input("{filter_col %>% stringr::str_replace_all(" ", "_")}",
      tagList("{stringr::str_remove(filter_col, "metaUI__filter_")}",
      {help}),
          spec = list(min = {spec$min}, max = {spec$max}, step = {spec$step}, ticks = {spec$ticks})
      )
                 ')

    } else if (is.factor(data[[filter_col]])) {

      # Serialise as R code so levels containing quotes or backslashes stay valid
      levels_ <- levels(data[[filter_col]]) %>% na.omit() %>% as.character()
      choices <- paste(deparse(levels_), collapse = "\n")

      out <- glue::glue('{if (length(levels_) < opts$selection_list_threshold) "checkboxGroupInput(" else "shinyWidgets::pickerInput(multiple = TRUE, options = list(`actions-box` = TRUE, dropupAuto = FALSE, `selected-text-format` = \\"count\\", `count-selected-text` = \\"{0} of {1} selected\\", `none-selected-text` = \\"None selected\\"), "} "{filter_col %>% stringr::str_replace_all(" ", "_")}",
      tagList("{stringr::str_remove(filter_col, rm_prefix)}",
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
          metaUI_missing_toggle("{filter_col %>% stringr::str_replace_all(" ", "_")}_include_NA", {sum(is.na(data[[filter_col]]))})
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

  filter_ids <-  filter_ids %>% stringr::str_replace_all(" ", "_")

  numeric <- vapply(colnames(data)[stringr::str_detect(colnames(data), "metaUI__filter_")], function(col) is.numeric(data[[col]]), logical(1))
  items <- purrr::pmap_chr(list(filter_ids, filter_names, numeric), \(fid, fn, is_numeric) {
    if (is_numeric) glue::glue('
            div(class = "metaui-breakdown-item", h4("{fn}"), div(class = "metaui-breakdown-numeric",
              tableOutput("summary_{fid}_table"),
              shinycssloaders::withSpinner(plotOutput("summary_{fid}_plot", height = 170))))
                ')
    else glue::glue('
            div(class = "metaui-breakdown-item", h4("{fn}"), htmlOutput("summary_{fid}_table"))
                ')
  }) %>% glue::glue_collapse(sep = ",\n")
  paste0('div(class = "metaui-breakdowns",\n', items, "),\n")


}

generate_moderator_selection <- function(data) {
  filter_cols <- colnames(data) %>%
    stringr::str_subset("metaUI__filter_") %>%
    setNames(., stringr::str_remove(., "metaUI__filter_"))

  filter_vector <- purrr::imap_chr(filter_cols, \(fc, fn) {
    paste('"', fn, '" = "', fc, '"', sep = '')
  })  %>% glue::glue_collapse(sep = ", ")

  # Create a selection drop-down with filter_cols as values
  glue::glue('
    selectInput("moderator", "Moderator",
      choices = c({filter_vector}),
      selected = "{filter_cols[1]}"
    )')
}

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
      shiny::htmlOutput("moderation_text"),
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

  out <- glue::glue('

  fluidPage(lang = "en",
    theme = shinythemes::shinytheme("{opts$shiny_theme}"),
    tags$head(tags$link(rel = "stylesheet", href = "metaui.css"), tags$script(src = "metaui.js")),
    {if (!is.null(opts$design)) \'tags$head(tags$link(rel = "stylesheet", href = "app-design.css")),\' else ""}
    shinyjs::useShinyjs(),
    tags$a(class = "metaui-skip", href = "#metaui_results", "Skip to results"),
    # Application title
    div(class = "metaui-header",
    titlePanel(
    windowTitle = {paste(deparse(paste0(dataset_name, " - metaUI")), collapse = "\n")},
    title = {get_favicon_tag(dataset_name)}),
    if (nzchar(welcome_text)) div(class = "metaui-intro", welcome_text)),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$h2(class = "metaui-panel-title", "Select effects"),
        # States which selection and data the visible results use; on phones it stays above the closed filters.
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
        actionButton("downloadData", "Download .xlsx", icon = icon("download")),
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
            htmltools::tagAppendAttributes(numericInput("sesoi", "Smallest effect size of interest (\\u00b1)",
              value = NA_real_, min = 0), placeholder = "Enter a bound", .cssSelector = "input"),
            p(class = "metaui-note", "A symmetric bound, on the scale of the estimates. There is no universal default; justify it substantively. Changing it reassesses the existing intervals without refitting. The last column gives the smallest bound that contains each interval, rounded up. Assessments use the unrounded limits, so an interval shown as ending at 0.30 can still reach past \\u00b10.30."),
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
            {if (any_filters) "sample_overview_main," else ""}
            {generate_sample_description_ui(data, any_filters)}
            div(id = "metaui_sample_table", sample_table),
            if (!is.null(metaUI_study_info)) div(class = "metaui-export-actions metaui-reference-actions",
              tags$span(class = "metaui-export-label", "References for this selection:"),
              downloadButton("references_txt", "Text (.txt)"), downloadButton("references_bib", "BibTeX (.bib)"), downloadButton("references_xlsx", "Excel (.xlsx)")),
            p(class = "metaui-note metaui-key metaui-forest-controls", HTML(metaUI_switch("One row per", "metaui-sample-view", c(effects = "effect", studies = "study"), "effects"))),
            if (!is.null(metaUI_study_info)) study_details_hint,
            DT::dataTableOutput("sample_table")
            )
          ),
          {if (!is.null(opts$bayesian)) \'tabPanel("Bayesian sensitivity", results_panel(p(class = "metaui-note", "Study-level normal-normal model: flat prior on the mean; half-normal prior on heterogeneity at half, baseline and double its scale. Effects are combined within studies by the declared correlation or first effect per study. tau is on the fitting scale. Intervals are 95% central credible intervals and depend on the prior."), tableOutput("bayesian_sensitivity"))),\' else ""}
          {if (length(metaUI_computation_specs(data))) \'tabPanel("Effect computations", results_panel(p(class = "metaui-note", "The selected effects under each author-defined computation; multilevel and RVE models only. The z-score filter uses the primary computation."), div(class = "metaui-scroll", tableOutput("computation_comparison")))),\' else ""}
          {generate_mod_tab(data, any_filters)}
          tabPanel("Forest plot", results_panel(tags$h3("Effects by study"), uiOutput("forest_panel"), forest_note)),
          tabPanel(
            "Publication bias", results_panel(qrppb_main, uiOutput("bias_power_note"), funnel_main, uiOutput("funnel_key"), div(class = "metaui-plot", plotOutput("funnel", width = "100%", height = 400) %>% shinycssloaders::withSpinner()),
            eggers_main, uiOutput("eggers_reading"), div(class = "metaui-scroll", tableOutput("eggers")),
            firstvalues,
            {if (identical(as.character(data$metaUI__es_type[1]), "SMD")) \'pcurve_main, uiOutput("pcurve_reading"), div(class = "metaui-scroll metaui-plot", plotOutput("pcurve", height = "auto") %>% shinycssloaders::withSpinner()), uiOutput("pcurve_notes"),\' else \'p(class = "metaui-note", "P-curve is available here only for standardized mean differences."),\'}
            zcurve_main, uiOutput("zcurve_reading"), div(class = "metaui-scroll metaui-plot", plotOutput("zcurve", height = "auto") %>% shinycssloaders::withSpinner()),
            uiOutput("zcurve_statistics"),
            div(class = "metaui-note metaui-warning-text", textOutput("zcurve_warnings")))
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
    filter_ids <- colnames(metaUI__df) %>% stringr::str_subset("metaUI__filter_") %>% stringr::str_replace_all(" ", "_")
    filters_types <- purrr::map_chr(filter_cols, \\(x) {
      if (is.numeric(metaUI__df[[x]])) "numeric" else "selection"
    })
   filters <- purrr::pmap(list(filter_cols, filter_ids, filters_types), ~list(col = ..1, id = ..2, type =  ..3))
       purrr::pmap(list(filter_cols, filter_ids, filters_types), ~ paste0("list(col = \'", ..1, "\', id = \'", ..2, "\', type = \'", ..3, "\')"))  %>%
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
    if (filters_changed()) paste0("Results show the previous selection: ", counts$retained_rows, " of ", counts$available_rows,
      " effects.")
    else paste0(counts$retained_rows, " of ", counts$available_rows, " effects, ", studies, if (studies == 1L) " study" else " studies")
  })

  output$selection_status <- renderText({
    if (!is.null(state_values$pending_upload_filters)) return("Restoring saved filters.")
    validate(need(isTruthy(input$go), "No analysis yet."))
    df <- df_filtered()
    counts <- attr(df, "metaUI_filter_report")
    # Input IDs replace spaces with underscores; show the column labels instead
    filter_cols <- grep("^metaUI__filter_", names(metaUI__df), value = TRUE)
    labels <- c(stats::setNames(sub("^metaUI__filter_", "", filter_cols), gsub(" ", "_", filter_cols)),
      outliers_z_scores = "z-score range")
    filter_names <- unname(labels[names(counts$successive_filter_exclusions)])
    excluded <- unlist(counts$successive_filter_exclusions)
    # Only filters that removed effects are listed.
    details <- paste(paste(filter_names[excluded > 0], excluded[excluded > 0], sep = ": "), collapse = "; ")
    upload <- attr(df, "metaUI_upload_info")
    paste(if (is.null(upload)) "Built dataset (as published with this app)." else paste0("Uploaded data: ", upload$file,
      " (", upload$rows, " rows), not the authors\' dataset. ", upload$z_rule,
      "; supplied z disagreements: ", upload$z_disagreements, ". ", upload$checkbox_note),
      if (length(metaUI_computation_specs(metaUI__df))) paste0("Computation: ",
        names(metaUI_computations(metaUI__df))[match(attr(df, "metaUI_computation"), metaUI_computations(metaUI__df))], ".") else "",
      "Selected", counts$retained_rows, "of", counts$available_rows,
      "rows.", if (counts$total_excluded == 0) "No effects excluded." else paste("Excluded", counts$total_excluded, paste0("by successive filters (", details, ").")))
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
      else if (is.na(pending)) "New selection. Update results to apply it." else paste0("New selection: ", pending, if (pending == 1L) " effect" else " effects", ". Update results to apply it.")
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
  comparison_models <- models_to_run[models_to_run$code %in% c(metaUI_code_multilevel, metaUI_code_rve), , drop = FALSE]
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
    value
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
    if (is.null(bound)) return(stats::setNames(data.frame(table$Model, threshold), c("Model", "Smallest bound that contains the interval (\\u00b1)")))
    stats::setNames(data.frame(table$Model, table$assessment, threshold), c("Model", "Assessment", "Smallest bound that contains the interval (\\u00b1)"))
  }, align = "?")
  # The answer leads, directly under the bound it answers.
  output$equivalence_verdict <- renderUI({
    table <- interval_assessment()
    bound <- tryCatch(chosen_bound(), error = function(e) NULL)
    req(!is.null(bound), "assessment" %in% names(table))
    p(class = "metaui-reading", HTML(paste0("<b>", sum(grepl("within bounds", table$assessment, fixed = TRUE)), " of ", nrow(table), "</b> intervals lie within \\u00b1", format(bound), ".")))
  })
  output$equivalence_note <- renderUI({
    table <- interval_assessment()
    tagList(
      if ("reason" %in% names(table)) p(class = "metaui-reason", table$reason),
      tags$details(class = "metaui-help", tags$summary("How to read this assessment"), tags$p("For unchanged default models, strict containment of the reported 95% CI corresponds to two one-sided tests at nominal alpha = .025 each, given the model assumptions; this is stricter than the conventional two one-sided tests (TOST) at alpha = .05. Custom intervals have unknown coverage. For Bayesian rows, containment implies at least 95% posterior probability within the bounds; it depends on the prior and is not a TOST. The multilevel model uses residual degrees of freedom; RVE has no small-sample correction, so its intervals can be too narrow. Bias-adjusted models rest on their own assumptions. This assesses the average effect; individual true effects can still exceed the bound.")))
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
      labels <- c(stats::setNames(sub("^metaUI__filter_", "", filter_cols), gsub(" ", "_", filter_cols)), outliers_z_scores = "z-score range")
      last <- names(counts)[max(which(unlist(counts) > 0), 0)]
      return(div(class = "metaui-empty", role = "status", tags$p(class = "metaui-empty-title", "No effects match this selection"),
        tags$p(if (length(last)) paste0("The ", unname(labels[last]), " filter removed the last remaining effects. "), "Change it, or ", tags$button(type = "button", class = "metaui-reset-link", "reset the filters"), " to select every effect again.")))
    }
    table <- estimatesfiltered()
    req(table)
    if (is.null(primary_model)) return(div(class = "metaui-primary metaui-primary-none",
      tags$p("The authors have not marked a primary model. The rows below are alternative estimators for the same selection; they answer different questions and the app does not choose between them.")))
    index <- primary_index(table)
    if (!length(index)) return(div(class = "metaui-primary metaui-primary-missing", role = "alert",
      tags$p(tags$b("Declared primary model not found: "), primary_model, ". Check the name in labels_and_options.R against models.R.")))
    row <- table[index[1], ]
    digits <- metaUI_digits(table$LCL[table$status == "ok"], table$UCL[table$status == "ok"])
    fmt <- function(x) metaUI_minus(formatC(x, format = "f", digits = digits))
    interval <- gsub(" ", "\\u00a0", if (grepl("^Confidence", row$interval_type)) ci_label else "95% interval", fixed = TRUE)
    unit <- metaUI_count_unit(row, length(unique(df_filtered()$metaUI__study_id)))
    div(class = "metaui-primary", role = "region", `aria-label` = "Primary analysis",
      if (row$status == "ok") tags$p(class = "metaui-primary-value", HTML(paste0("Estimated effect: ", metaUI_label_html(if (metaUI__df$metaUI__es_type[1] == "ZCOR") "r" else metaUI_eff_size_type_label), " = <b>", fmt(row$es), "</b>, ",
        if (is.na(row$LCL) || is.na(row$UCL)) "no interval reported" else paste0(interval, "\\u00a0[", fmt(row$LCL), ", ", fmt(row$UCL), "]"))))
      else tags$p(class = "metaui-primary-gap", tags$b(if (identical(row$status, "failed")) "Could not be estimated for this selection: " else "Not estimated for this selection: "), metaUI_reader_reason(row$reason, row$warnings), " The app does not substitute another model."),
      # The headline already gives the sign and the interval; this adds what it cannot.
      { reading <- if (row$status == "ok") trimws(paste0(
          { others <- table[-index[1], , drop = FALSE]; others <- others[others$status == "ok", , drop = FALSE]
            if (nrow(others) > 1L) paste0("Across the ", nrow(others), " other estimators, estimates range from ", fmt(min(others$es)), " to ", fmt(max(others$es)), ".") else "" },
          # An introduction from the authors states the direction in their own words.
          if (nzchar(welcome_text)) "" else switch(metaUI__df$metaUI__direction[1], positive = " Positive values are in the direction the authors expected.",
            negative = " Negative values are in the direction the authors expected.", ""))) else ""
        if (nzchar(reading)) tags$p(class = "metaui-primary-reading", reading) },
      # The tinted first row of the table below names the model and its k; the line here names it only when no estimate is shown.
      { label <- trimws(paste0(if (row$status != "ok") paste0(row$Model, ". Primary analysis, declared by the authors.") else "",
          if (grepl("hedges|cohen|standardi[sz]ed mean|^smd$", metaUI_eff_size_type_label, ignore.case = TRUE)) " Effects are standardized mean differences, in standard deviations." else
          # The headline names r alone; a qualified label (adjusted for attenuation, say) is given here.
          if (metaUI__df$metaUI__es_type[1] == "ZCOR" && !identical(metaUI_eff_size_type_label, "r")) paste0(" Effects are correlations: ", metaUI_eff_size_type_label, ".") else ""))
        if (nzchar(label)) tags$p(class = "metaui-primary-label", label) },
      if (row$status == "ok" && nzchar(row$warnings)) tags$p(class = "metaui-primary-warning", paste("Warnings:", metaUI_reader_warnings(row$warnings))),
      { studies <- length(unique(df_filtered()$metaUI__study_id))
        if (row$status == "ok" && studies < 10L) tags$p(class = "metaui-primary-warning", paste0("With only ", studies, if (studies == 1L) " study" else " studies",
          ", these estimates are fragile: some intervals can be too narrow (robust variance estimation has no small-sample correction here), and bias-adjusted estimates are unstable.")) })
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
      paste0("Effects are fitted and shown on the scale declared by the authors: ", metaUI_label_html(metaUI_eff_size_type_label), ".")
    direction_text <- if (direction == "unspecified") paste("No expected direction was declared. Methods that need one (by default p-uniform*,",
      "the Hedges-Vevea selection model and p-curve) are reported as unsupported rather than guessed.") else
      paste0("Expected direction declared by the authors: ", direction, ". Direction-dependent methods (p-uniform*, the Hedges-Vevea selection model, p-curve) analyze effects in that direction",
        if (direction == "negative") "; their inputs are sign-flipped for fitting and their estimates flipped back for display" else "", ". Other models are two-sided.")
    dependence_text <- paste0(if (aggregation_method[1] == "first") "Study-level models use only the first selected effect from each study." else
      paste0("Study-level models first combine the effects within each study (GLS average, assumed correlation of ", correlation_dependent,
      " between effects from the same study)."), " Effect-level models (by default multilevel and RVE) use every selected effect. Studies are assumed independent.")
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
      studies = length(unique(df_filtered()$metaUI__study_id)))
  })


  <FILTER>
  # Sample overview per filter/moderator -----------------------------------------------------------

       <FILTER>
    <<purrr::map_chr(filters, \\(f) {
      if (f$type == "numeric") {
        glue::glue("
    output$`summary_{f$id}_table` <- renderTable({{ # {{ escapes the glue syntax
    df <- df_filtered()
    summary <- summarise_numeric(df[[\'{f$col}\']], \'{f$col  %>% stringr::str_remove(\'metaUI__filter_\')}\')
    summary$Value <- sub(\'[.]00$\', \'\', summary$Value)
    summary[summary$Statistic != \'Var\', , drop = FALSE]
  }})

  output$`summary_{f$id}_plot` <- renderPlot({{
    df <- df_filtered()
    ggplot2::ggplot(df, ggplot2::aes(x = `{f$col}`)) +
      ggplot2::geom_histogram(na.rm = TRUE, bins = 24, fill = plot_colours$bars, colour = plot_colours$background, linewidth = .3) +
      ggplot2::scale_y_continuous(limits = function(x) range(pretty(c(0, x), 3)), breaks = function(x) unique(floor(pretty(x, 3))), name = \'Effects\', expand = ggplot2::expansion(mult = 0)) +
      metaUI_plot_theme(plot_colours) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank())
  }}, bg = plot_colours$background)
  ")
      } else {
        glue::glue("
          output$`summary_{f$id}_table` <- renderText({{
    df <- df_filtered()
    metaUI_bars_html(summarise_categorical(df[[\'{f$col}\']], \'{f$col  %>% stringr::str_remove(\'metaUI__filter_\')}\'), \'{f$col  %>% stringr::str_remove(\'metaUI__filter_\')}\')
  }})
        ")
      }
    }) %>% paste(collapse = "\n")>>

  </FILTER>

  # Sample table -----------------------------------------------------------

  selected_references <- reactive(metaUI_selected_references(metaUI_study_info, df_filtered()$metaUI__study_id))
  output$references_txt <- downloadHandler(filename = function() "references.txt", content = function(file) {
    references <- selected_references()$Reference
    writeLines(enc2utf8(paste0(references[nzchar(references)], "\n")), file, useBytes = TRUE)
  })
  output$references_bib <- downloadHandler(filename = function() "references.bib", content = function(file) {
    refs <- selected_references()
    missing <- refs$Reference[is.na(refs$bibtex)]
    writeLines(enc2utf8(c(unique(stats::na.omit(refs$bibtex)), if (length(missing)) c("", "% References without a BibTeX entry:", paste("%", missing)))), file, useBytes = TRUE)
  })
  output$references_xlsx <- downloadHandler(filename = function() "references.xlsx", content = function(file) {
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
    if ("Source" %in% names(out) && all(is.na(out$Source) | startsWith(out$Study, as.character(out$Source)))) out$Source <- NULL
    studies <- identical(input$sample_view, "studies")
    if (studies) out <- metaUI_study_rows(out)
    unit <- if (studies) "studies" else "effects"

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
    cells <- metaUI_study_cells(out$Study, out$metaUI__key, metaUI_study_info, metaUI_study_details_html, metaUI_study_reference_text)
    out$Study <- cells$label
    out$metaUI__details <- cells$details
    out$metaUI__key <- NULL

    # Non-integer numeric columns show 2 decimals where a value needs them (whole numbers such as
    # sample sizes and years stay whole), with true minus signs; sorting stays on the numbers.
    decimals <- setdiff(names(out)[vapply(out, function(x) is.numeric(x) && any(x != round(x), na.rm = TRUE), logical(1))], "Effect size")
    three_decimals <- DT::JS("function(data, type) { return type === \'display\' && data !== null && data !== \'\' ? Number(data).toFixed(3).replace(\'-\', \'\\u2212\') : data; }")
    two_decimals <- DT::JS("function(data, type) { if (type !== \'display\' || data === null || data === \'\') return data; var x = Number(data); return (Number.isInteger(x) ? String(x) : x.toFixed(2)).replace(\'-\', \'\\u2212\'); }")
    targets <- function(columns) match(intersect(columns, names(out)), names(out)) - 1

    table <- out %>% DT::datatable(
      rownames = FALSE, style = "default", class = "compact",
      options = list(autoWidth = FALSE, dom = \'<"metaui-dt-top"lfp>rt<"metaui-dt-bottom"ip>\', columnDefs = list(list(targets = ncol(out) - 1, visible = FALSE, searchable = FALSE),
          list(targets = targets(decimals), render = two_decimals, className = "dt-right"),
          list(targets = targets("Effect size"), render = three_decimals, className = "dt-right"),
          list(targets = union(unname(which(vapply(out, is.numeric, logical(1)))) - 1, targets(c("N", "Effects"))), className = "dt-right"),
          list(targets = "_all", defaultContent = "(Missing)")),
        language = list(search = paste("Search", unit), lengthMenu = "Show _MENU_ per page", info = paste("_START_ to _END_ of _TOTAL_", unit), infoFiltered = "(from _MAX_)")),
      escape = setdiff(names(out), c("URL", "Study", "metaUI__details")), # built (and escaped) above
      caption = tags$caption(
        style = "caption-side: bottom; text-align: left; margin: 8px 0;",
        HTML(if (studies) "One row per study. Values that differ between its effects show their range, or \\u201cseveral\\u201d." else paste("Effect size:", metaUI_label_html(metaUI_eff_size_type_label)))
      )
    )
    table
  })

 <FILTER>

  # Moderators  -----------------------------------------------------------


  # Categorical moderators are drawn in their table; numeric ones get a scatter beside theirs.
  output$moderation_panel <- renderUI({
    # The finding above states a failed fit once; the panel then stays empty.
    model <- tryCatch(moderator_model(), error = function(e) NULL)
    if (is.null(model) || isTRUE(model$constant)) NULL
    else if (is.numeric(df_filtered()[[input$moderator]])) div(class = "metaui-moderation-grid",
      shiny::htmlOutput("moderation_table"), shinycssloaders::withSpinner(plotly::plotlyOutput("moderation_plot", height = 340)),
      p(class = "metaui-note metaui-key", "Dots: selected effects, larger when more precise; whole-number moderators are spread slightly so that dots do not stack. Line and band: the multilevel moderator model with its 95% CI."))
    else div(class = "metaui-moderation-wide", shiny::htmlOutput("moderation_table"))
  })

  output$moderation_plot <- plotly::renderPlotly({
    df <- df_filtered()
    req(is.numeric(df[[input$moderator]]))
    df$mod <- df[[input$moderator]]
    # The line and band are the multilevel moderator model tested above; dots follow precision.
    model <- moderator_model()
    grid <- seq(min(df$mod, na.rm = TRUE), max(df$mod, na.rm = TRUE), length.out = 40)
    fitted <- as.data.frame(stats::predict(model, newmods = grid))
    fitted$mod <- grid
    fitted[c("pred", "ci.lb", "ci.ub")] <- lapply(fitted[c("pred", "ci.lb", "ci.ub")], metaUI_show, df)
    df$shown <- metaUI_show(df$metaUI__effect_size, df)
    df$hover <- paste0(df$metaUI__study_id, ": ", round(df$shown, 2))
    p <- ggplot2::ggplot(data = df, ggplot2::aes(y = shown, x = mod)) +
      ggplot2::geom_hline(yintercept = 0, colour = plot_colours$slate, linewidth = .3) +
      ggplot2::geom_ribbon(data = fitted, ggplot2::aes(x = mod, ymin = ci.lb, ymax = ci.ub), inherit.aes = FALSE, fill = plot_colours$accent, alpha = .14) +
      # Whole-number moderators (years, say) stack their dots: spread them a little, for sight only.
      suppressWarnings(ggplot2::geom_point(ggplot2::aes(size = 1 / metaUI__se, text = hover), shape = 21, fill = plot_colours$slate, colour = plot_colours$background, stroke = .3, alpha = .65, na.rm = TRUE,
        position = ggplot2::position_jitter(width = if (all(df$mod == round(df$mod), na.rm = TRUE)) .3 else 0, height = 0, seed = 1))) +
      ggplot2::geom_line(data = fitted, ggplot2::aes(x = mod, y = pred), inherit.aes = FALSE, colour = plot_colours$accent, linewidth = .7) +
      ggplot2::scale_size(range = c(1.8, 3.6), guide = "none") +
      ggplot2::scale_x_continuous(breaks = { span <- range(df$mod, na.rm = TRUE); ticks <- pretty(span, 8); ticks[ticks >= span[1] & ticks <= span[2]] }) +
      ggplot2::scale_y_continuous(breaks = { span <- range(df$shown, fitted$ci.lb, fitted$ci.ub); ticks <- pretty(span, 8); ticks[ticks >= span[1] & ticks <= span[2]] }) +
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

  moderator_fit <- reactive({
    df <- df_filtered()
    # A moderator with one value has nothing to fit, test or draw.
    if (length(unique(stats::na.omit(df[[input$moderator]]))) < 2L)
      return(list(constant = TRUE, moderation_text = HTML(paste0(\'<p class="metaui-moderation-finding">The selected effects do not vary on <b>\', htmltools::htmlEscape(stringr::str_remove(input$moderator, "metaUI__filter_")), \'</b>, so its relationship with the effect sizes cannot be tested. Widen the selection to test it.</p>\'))))

    if (is.numeric(df[[input$moderator]])) {

      model <- metafor::rma.mv(
        yi = metaUI__effect_size,
        V = metaUI__variance,
        random = ~ 1 | metaUI__study_id/metaUI__effect_id,
        tdist = TRUE,
        data = df,
        mods = as.formula(glue::glue("~`{input$moderator}`")),
        method = "ML",
        sparse = TRUE
      )
     moderation_text <-     HTML(glue::glue(
      \'<p class="metaui-moderation-finding"><b>{input$moderator  %>% stringr::str_remove("metaUI__filter_")}</b> is <b>{ifelse(model[["QMp"]] < .05, "a", "not a")} significant{ifelse(is.numeric(df[[input$moderator]]), " linear", "")} moderator</b> at the 5% level: \',
     \'<i>F</i>({model[["QMdf"]][1]}, {model[["QMdf"]][2]}) = {round(model[["QM"]], digits = 2)}, \',
      \'<i>p</i> {fmt_p(model[["QMp"]])}.</p>\'
    ))

    } else {
     model_sig <- metafor::rma.mv(
        yi = metaUI__effect_size,
        V = metaUI__variance,
        random = ~ 1 | metaUI__study_id/metaUI__effect_id,
        tdist = TRUE,
        data = df,
        mods = as.formula(glue::glue("~`{input$moderator}`")),
        method = "ML",
        sparse = TRUE
      )
     model <- metafor::rma.mv(
        yi = metaUI__effect_size,
        V = metaUI__variance,
        random = ~ 1 | metaUI__study_id/metaUI__effect_id,
        tdist = TRUE,
        data = df,
        mods = as.formula(glue::glue("~`{input$moderator}` - 1")),
        method = "ML",
        sparse = TRUE
      )

     moderation_text <-     HTML(glue::glue(
      \'<p class="metaui-moderation-finding"><b>{input$moderator  %>% stringr::str_remove("metaUI__filter_")}</b> is <b>{ifelse(model_sig[["QMp"]] < .05, "a", "not a")} significant{ifelse(is.numeric(df[[input$moderator]]), " linear", "")} moderator</b> at the 5% level: \',
     \'<i>F</i>({model_sig[["QMdf"]][1]}, {model_sig[["QMdf"]][2]}) = {round(model_sig[["QM"]], digits = 2)}, \',
      \'<i>p</i> {fmt_p(model_sig[["QMp"]])}.</p>\'
    ))
    }
    # Effects without a value for the moderator drop out of its model.
    missing <- sum(is.na(df[[input$moderator]]))
    if (missing) moderation_text <- HTML(paste0(moderation_text, \'<p class="metaui-note">\', nrow(df) - missing, " of ", nrow(df),
      " selected effects have a value for ", htmltools::htmlEscape(stringr::str_remove(input$moderator, "metaUI__filter_")), " and enter this model.</p>"))
    model$moderation_text <- moderation_text
  model
  })


  output$moderation_table <- shiny::renderText({
    model <- moderator_model()
    if (isTRUE(model$constant)) return("")
    df <- df_filtered()
    x <- df[[input$moderator]]
    if (is.numeric(x)) {
      # Three significant digits: a per-unit slope such as -0.0027 must not read as -0.00
      coefficient <- function(value) metaUI_minus(trimws(formatC(signif(value, 3), format = "fg", digits = 3, flag = "#")))
      # Mean and SD share their decimals, as in the Sample tab.
      places <- if (abs(mean(x, na.rm = TRUE)) >= 100) 1 else 2
      stat <- function(value) metaUI_minus(trimws(formatC(value, format = "f", digits = places)))
      whole <- all(x == round(x), na.rm = TRUE)
      range_text <- if (whole) paste0(min(x, na.rm = TRUE), "\\u2013", max(x, na.rm = TRUE)) else paste0(stat(min(x, na.rm = TRUE)), " to ", stat(max(x, na.rm = TRUE)))
      # These coefficients have no r equivalent.
      fisher <- if (df$metaUI__es_type[1] == "ZCOR") " (Fisher <i>z</i>)" else ""
      row <- function(label, value) paste0("<tr><td>", label, \'</td><td class="num">\', value, "</td></tr>")
      return(paste0(\'<table class="table metaui-custom metaui-stats"><tbody>\',
        # The intercept would be the effect at a moderator value of 0, often far outside the data; the mean is inside it.
        { centre <- as.data.frame(stats::predict(model, newmods = mean(x, na.rm = TRUE)))
          shown <- metaUI_show(c(centre$pred, centre$ci.lb, centre$ci.ub), df)
          digits <- metaUI_digits(shown[2], shown[3]); value <- function(v) metaUI_minus(formatC(v, format = "f", digits = digits))
          row(paste0("Effect at the mean ", input$moderator %>% stringr::str_remove("metaUI__filter_"), " (", stat(mean(x, na.rm = TRUE)), ") [95% CI]"),
            paste0(value(shown[1]), " [", value(shown[2]), ", ", value(shown[3]), "]")) }, row(paste0("Slope, per unit of ", input$moderator %>% stringr::str_remove("metaUI__filter_"), fisher), coefficient(model$b[2])), row("<i>k</i>", sum(!is.na(x))),
        row("Moderator mean (SD)", paste0(stat(mean(x, na.rm = TRUE)), " (", stat(stats::sd(x, na.rm = TRUE)), ")")),
        row("Moderator range", range_text), "</tbody></table>"))
    }
    levels <- rownames(model$b) %>% stringr::str_remove(input$moderator) %>% stringr::str_remove_all("`") #Needed to support spaces in moderator names
    counts <- table(x)
    summary <- estimatesfiltered()
    overall <- summary$es[models_to_run$code == metaUI_code_multilevel & summary$status == "ok"]
    shown <- metaUI_show(df$metaUI__effect_size, df)
    metaUI_moderator_html(levels, metaUI_show(as.numeric(model$b), df), metaUI_show(model$ci.lb, df), metaUI_show(model$ci.ub, df),
      as.integer(counts[levels]), x, shown,
      paste0(df$metaUI__study_id, ": ", round(shown, 2)), metaUI_eff_size_type_label,
      ref = if (length(overall)) overall[1] else NULL, se = df$metaUI__se, show_effects = isTRUE(input$moderator_effects))
  })

  output$moderation_text <- shiny::renderText({
    model <- moderator_model()
    model$moderation_text
  })

  </FILTER>

  # Heterogeneity -----------------------------------------------------------

  output$heterogeneity_reading <- renderUI({
    df <- df_filtered()
    req(estimatesreactive())
    het <- metaUI_heterogeneity(metaUI_reuse_fit(estimatesreactive(), models_to_run, metaUI_code_multilevel, df, function() metaUI_multilevel_fit(df)), df)
    req(!is.na(het$Q_p))
    p(class = "metaui-reading", HTML(paste0("Effects vary <b>", if (het$Q_p < .05) "more" else "no more", "</b> than sampling error alone would predict (",
      "<i>Q</i> test, <i>p</i> ", fmt_p(het$Q_p), ").")))
  })

  output$heterogeneity <- renderTable({
    df <- df_filtered()

    req(estimatesreactive())
    metapp_total <- metaUI_reuse_fit(estimatesreactive(), models_to_run,
      metaUI_code_multilevel, df, function() metaUI_multilevel_fit(df))

    het <- metaUI_heterogeneity(metapp_total, df)
    het[c("study_variance", "effect_variance", "total_variance")] <-
      lapply(het[c("study_variance", "effect_variance", "total_variance")], round_, digits = 4)
    het$Q <- round(het$Q, 2)
    het$Q_p <- fmt_p(het$Q_p, include_equal = FALSE)

    # rma.mv variance components are sigma^2 , not tau^2
    het$Q <- formatC(het$Q, format = "f", digits = 2)
    scale <- if (df$metaUI__es_type[1] == "ZCOR") " (Fisher <i>z</i>)" else ""
    names(het) <- c(paste0("&sigma;<sup>2</sup> between studies", scale), paste0("&sigma;<sup>2</sup> within studies", scale), paste0("Total &sigma;<sup>2</sup>", scale), "<i>Q</i>", "<i>p</i> (<i>Q</i>)", "Variance components")
    metaUI_statistics_rows(het)
  }, colnames = FALSE, sanitize.text.function = function(x) x, align = "lr")

  # FOREST PLOT FOR ALL INCLUDED STUDIES ------------------------------------
  forest_limit_message <- "PDF and PNG downloads are limited to <<opts$max_forest_plot_rows>> effects. Narrow the selection to get them."
  output$forest_panel <- renderUI({
    df <- df_filtered()
    if (!nrow(df)) return(p(class = "metaui-reason", role = "status", "No effects selected."))
    small <- nrow(df) <= <<opts$max_forest_plot_rows>>
    tagList(
      div(class = "metaui-export-actions", if (small) downloadButton("forest_pdf", "Download PDF"),
      if (small) downloadButton("forest_png", "Download PNG"),
      downloadButton("forest_csv", "Download plot data")),
      # The reason sits by the buttons it explains.
      if (!small) p(class = "metaui-note", forest_limit_message),
      shiny::htmlOutput("foreststudies"))
  })
  forest_rows <- reactive({
    df <- df_filtered()
    validate(need(nrow(df) > 0, "No effects selected."))
    metaUI_forest_rows(df, estimatesfiltered(), models_to_run, primary_model)
  })
  # A drawn page grows with every row, so only the image downloads keep a limit.
  forest_image_rows <- reactive({
    validate(need(nrow(df_filtered()) <= <<opts$max_forest_plot_rows>>, forest_limit_message))
    forest_rows()
  })
  forest_plot_for <- function(label_width = NULL, top_axis = FALSE, family = plot_colours$family) metaUI_forest_plot(forest_image_rows(), df_filtered()$metaUI__display_scale[1], colours = plot_colours, label_width = label_width, top_axis = top_axis, family = family)
  forest_height <- function() 90 + 20 * nrow(forest_image_rows())
  # Plots taller than about a laptop screen repeat the x axis at the top.
  forest_plot <- reactive(forest_plot_for(top_axis = forest_height() > 700))
  forest_view <- reactiveValues(page = 1L, sort = "name")
  observeEvent(input$forest_page, {
    page <- suppressWarnings(as.integer(input$forest_page))
    if (length(page) == 1L && !is.na(page)) forest_view$page <- page
  })
  observeEvent(input$forest_sort, {
    forest_view$sort <- if (identical(input$forest_sort, "estimate")) "estimate" else "name"
    forest_view$page <- 1L
  })
  observeEvent(df_filtered(), forest_view$page <- 1L)
  forest_studies <- reactive(metaUI_forest_studies(df_agg()))
  forest_details <- reactive(metaUI_effect_details(df_filtered()))
  output$foreststudies <- renderText({
    metaUI_forest_html(forest_rows(), metaUI_eff_size_type_label, studies = forest_studies(), details = forest_details(), page = forest_view$page, sort = forest_view$sort)
  })
  output$forest_pdf <- downloadHandler(filename = function() "forest-plot.pdf", content = function(file) {
    ggplot2::ggsave(file, forest_plot_for(top_axis = forest_height() > 700, family = ""), device = "pdf", width = 10,
      height = forest_height() / 96, limitsize = FALSE)
  })
  output$forest_png <- downloadHandler(filename = function() "forest-plot.png", content = function(file) {
    ggplot2::ggsave(file, forest_plot(), device = "png", width = 10,
      height = forest_height() / 96, dpi = 120, limitsize = FALSE)
  })
  output$forest_csv <- downloadHandler(filename = function() "forest-plot-data.csv", content = function(file) {
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
    p(class = "metaui-note metaui-key", HTML(paste0("Shaded regions: <i>p</i> &lt; .05 against zero. Dashed lines: the random-effects estimate from the study-level effects shown here (",
      metaUI_minus(round_(metaUI_show(as.numeric(meta_agg()$b), metaUI__df), metaUI_digits(meta_agg()$ci.lb, meta_agg()$ci.ub))), "), with its 95% limits.",
      if (metaUI__df$metaUI__es_type[1] == "ZCOR") " Positions follow the Fisher <i>z</i> scale, on which standard errors do not depend on the effect, so the <i>r</i> ticks are unevenly spaced." else "")))
  })

  output$funnel <- renderPlot({
    meta_agg <- meta_agg()

    metaUI_funnel(meta_agg, xlab = metaUI_eff_size_type_label, colours = plot_colours, width = session$clientData$output_funnel_width, back = metaUI__df$metaUI__es_type[1] == "ZCOR")
  }, height = 400) # fixed: after an error the collapsed container would otherwise set the next plot height

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
      "significant</b> at the 5% level: <i>t</i> = ", metaUI_minus(round_(eggers$statistic, 3)), ", <i>p</i> ", fmt_p(eggers$p.value), ".")))
  })

  output$eggers <- renderTable({
    meta_agg <- meta_agg()

    eggers <- metaUI_egger(as.numeric(meta_agg$yi), sqrt(meta_agg$vi))
    shiny::validate(shiny::need(!is.null(eggers$estimate),
      "Egger test requires at least three studies."))
    # t and p are in the sentence above.
    metaUI_statistics_rows(data.frame(
      "Intercept" = metaUI_minus(round_(eggers$estimate[1], 3)), "Residual variance (&tau;<sup>2</sup>)" = round_(eggers$tau^2, 3),
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
    pcurve_notes(NULL)
    df <- df_filtered()
    selected <- tryCatch(metaUI_pcurve_data(df), error = function(e) e)
    validate(need(!inherits(selected, "error"), if (inherits(selected, "error")) conditionMessage(selected) else ""))
    message("P-curve direction-contrary exclusions: ", attr(selected, "direction_exclusions"))
    # pcurve() draws as it computes; its drawing goes nowhere, the plot below uses its values.
    grDevices::pdf(NULL)
    result <- tryCatch(pcurve(selected, effect.estimation = FALSE, compact = TRUE, accent = plot_colours$accent), error = function(e) e, finally = grDevices::dev.off())
    validate(need(!inherits(result, "error"), if (inherits(result, "error")) conditionMessage(result) else ""))
    pcurve_notes(list(stats = result$stats, text = c(result$notes, paste0(attr(selected, "direction_exclusions"), " further results were excluded because they ran against the expected direction.")),
      present = result$EvidencePresent, absent = result$EvidenceAbsent))
    metaUI_pcurve_plot(result$PlotData, plot_colours)
  }, width = function() max(420, session$clientData$output_pcurve_width),
    height = function() if (isTRUE(session$clientData$output_pcurve_width < 600)) 380 else 400)


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
    paste(if (inherits(value$result, "error")) paste("Could not be estimated:", conditionMessage(value$result)) else "",
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
    tagList(p(class = "metaui-reading", HTML(paste0("<b>", percent(observed[1]), "</b> of these tests are significant (observed discovery rate); without selective reporting, the fit expects <b>", percent(expected),
      "</b> (expected discovery rate). ", verdict, " Of the significant results, an estimated <b>", percent(replicable), "</b> would be significant again in exact replications (expected replicability rate)."))),
      p(class = "metaui-note", "The curve is fitted to the significant results only (z above 1.96); the bars below that are shown for comparison. The expected discovery rate is shown without an interval, so treat its comparison with the observed rate as a rough guide."))
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
    value <- zcurve_fit()
    validate(need(!inherits(value$result, "error"), "Z-curve could not be estimated for this selection; reason below."))
    metaUI_zcurve_plot(value$result, plot_colours)
  }, width = function() max(420, session$clientData$output_zcurve_width), height = 400)

  # DISTRIBUTION OF EFFECTS -------------------------------------------------
  output$distribution_note <- renderUI({
    centre <- mean(metaUI__df$metaUI__effect_size)
    # A mean that rounds to zero drops out of the formula.
    numerator <- if (round(abs(centre), 2) == 0) "effect" else paste0("(effect ", if (centre < 0) "+" else metaUI_minus("-"), " ", round_(abs(centre), 2), ")")
    p(class = "metaui-note", HTML(paste0("The Exclude extreme effects filter uses <i>z</i> = (effect ", metaUI_minus("-"), " mean of all effects in the full dataset) / their SD = ", numerator, " / ",
      round_(sd(metaUI__df$metaUI__effect_size), 2), if (metaUI__df$metaUI__es_type[1] == "ZCOR") ", on the Fisher <i>z</i> scale" else "", ".")))
  })

  # The dots on the plot, as a list that needs no pointer.
  output$outlier_list <- renderUI({
    df <- df_filtered()
    effects <- df$metaUI__effect_size
    qs <- stats::quantile(effects, c(.25, .75))
    flagged <- df[effects < qs[[1]] - 1.5 * diff(qs) | effects > qs[[2]] + 1.5 * diff(qs), , drop = FALSE]
    if (!nrow(flagged)) return(p(class = "metaui-note", "No effects lie beyond 1.5 IQR."))
    flagged <- flagged[order(flagged$metaUI__effect_size), , drop = FALSE]
    z <- (flagged$metaUI__effect_size - mean(metaUI__df$metaUI__effect_size)) / sd(metaUI__df$metaUI__effect_size)
    shown <- metaUI_show(flagged$metaUI__effect_size, df)
    tags$details(class = "metaui-help", tags$summary(paste0("List the ", nrow(flagged), if (nrow(flagged) == 1L) " effect" else " effects", " beyond 1.5 IQR")),
      div(class = "metaui-scroll", tags$table(class = "table metaui-custom metaui-stats",
        tags$thead(tags$tr(tags$th("Study"), tags$th(class = "num", "Effect"), tags$th(class = "num", HTML("<i>z</i>")))),
        tags$tbody(lapply(seq_len(nrow(flagged)), function(i) tags$tr(tags$td(as.character(flagged$metaUI__study_id[i])),
          tags$td(class = "num", metaUI_minus(formatC(shown[i], format = "f", digits = 2))), tags$td(class = "num", metaUI_minus(formatC(z[i], format = "f", digits = 2)))))))))
  })

  output$violin <- plotly::renderPlotly({
    df <- df_filtered()
    effects <- df$metaUI__effect_size

    # Standardise against the built dataset, like the z-score filter (metaUI__es_z)
    efm <- mean(metaUI__df$metaUI__effect_size)
    efsd <- sd(metaUI__df$metaUI__effect_size)

    # Outliers follow the boxplot rule: quartiles +/- 1.5 * IQR
    qs <- quantile(effects, c(.25, .5, .75))
    bounds <- c(qs[[1]] - 1.5 * diff(qs[c(1, 3)]), qs[[3]] + 1.5 * diff(qs[c(1, 3)]))
    outliers <- df[effects < bounds[1] | effects > bounds[2], , drop = FALSE]
    # Which effects stand out is settled on the fitting scale; the plot shows them as readers read them.
    z <- (outliers$metaUI__effect_size - efm) / efsd
    effects <- metaUI_show(effects, df); qs[] <- metaUI_show(qs, df)
    outliers$metaUI__effect_size <- metaUI_show(outliers$metaUI__effect_size, df)

    breaks <- pretty(effects, n = 24)
    counts <- graphics::hist(effects, breaks = breaks, plot = FALSE)$counts
    span <- range(breaks) + c(-1, 1) * diff(range(breaks)) * .02
    # Ticks carry the decimals their spacing needs, so small ranges keep distinct labels.
    ticks <- pretty(span, 6); ticks <- ticks[ticks >= span[1] & ticks <= span[2]]
    tick_digits <- max(0L, ceiling(-log10(diff(ticks)[1] * (1 + 1e-9))))
    # The axis ends on a labelled tick above the tallest bar.
    top <- max(pretty(c(0, max(counts)), 5))
    ink <- plot_colours$ink
    axis_font <- list(size = 13, color = plot_colours$slate)
    strip <- top * 1.13

    p <- plotly::plot_ly(x = (breaks[-1] + breaks[-length(breaks)]) / 2, y = counts, width = diff(breaks), type = "bar",
        marker = list(color = plot_colours$bars, line = list(color = plot_colours$background, width = 1)),
        hovertemplate = "%{y} effects around %{x:.2f}<extra></extra>", showlegend = FALSE) %>%
      plotly::add_trace(x = outliers$metaUI__effect_size, y = rep(strip, nrow(outliers)), type = "scatter", mode = "markers", inherit = FALSE, showlegend = FALSE,
        marker = list(color = plot_colours$slate, size = 6), text = paste0(outliers$metaUI__study_id, ": ", round(outliers$metaUI__effect_size, 2), " (z = ", round(z, 2), ")"),
        hovertemplate = "%{text}<extra></extra>") %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(bargap = 0, font = list(family = metaui_plotly_font, color = ink),
        paper_bgcolor = plot_colours$background, plot_bgcolor = plot_colours$background,
        margin = list(l = 44, r = 12, t = 8, b = 48),
        shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = top * 1.05, line = list(color = plot_colours$slate, width = 1), layer = "above"),
          list(type = "line", x0 = qs[[1]], x1 = qs[[3]], y0 = strip, y1 = strip, line = list(color = plot_colours$slate, width = 5)),
          list(type = "line", x0 = qs[[2]], x1 = qs[[2]], y0 = strip - top * .03, y1 = strip + top * .03, line = list(color = ink, width = 2))),
        annotations = list(
          list(x = qs[[2]], y = strip, yshift = 9, text = paste0("middle 50%, median ", metaUI_minus(round_(qs[[2]], 2))), showarrow = FALSE, xanchor = "center", yanchor = "bottom", font = list(size = 13, color = plot_colours$slate)),
          if (any(outliers$metaUI__effect_size < qs[[1]])) list(x = min(outliers$metaUI__effect_size), y = strip, yshift = -9, text = "beyond 1.5 IQR", showarrow = FALSE, xanchor = "left", yanchor = "top", font = list(size = 13, color = plot_colours$slate)),
          if (any(outliers$metaUI__effect_size > qs[[3]])) list(x = max(outliers$metaUI__effect_size), y = strip, yshift = -9, text = "beyond 1.5 IQR", showarrow = FALSE, xanchor = "right", yanchor = "top", font = list(size = 13, color = plot_colours$slate))),
        xaxis = list(title = list(text = metaUI_label_html(metaUI_eff_size_type_label), font = list(size = 13.5)), range = span, tickmode = "array", tickvals = ticks, ticktext = metaUI_minus(formatC(ticks, format = "f", digits = tick_digits)), tickfont = axis_font, zeroline = FALSE, showgrid = FALSE, fixedrange = TRUE, showline = TRUE, linecolor = plot_colours$slate, linewidth = 1, ticks = "outside", ticklen = 3, tickcolor = plot_colours$slate),
        yaxis = list(title = list(text = "Effects", font = list(size = 13.5)), tickfont = axis_font, gridcolor = plot_colours$grid, zeroline = FALSE, fixedrange = TRUE, range = c(0, top * 1.3), rangemode = "normal", tick0 = 0, tickvals = pretty(c(0, top), 5)[pretty(c(0, top), 5) <= top]))
    p
  })

  # DOWNLOAD ----------------------------------------------------------------

  data_list <- reactive({
    req(is.null(state_values$pending_upload_filters))
    upload_info <- attr(df_filtered(), "metaUI_upload_info")
    filter_selections <- attr(df_filtered(), "metaUI_applied_filters")

    list(
      dataset = attr(df_filtered(), "metaUI_source_data"),
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
          if (is.null(primary_model)) "none declared" else primary_model))
    )
  })


  workbook_name <- function() paste0(Sys.Date(), "-", gsub(" ", "_", dataset_name), "-metaUIdata.xlsx")

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
      source = attr(df, "metaUI_source_data"), built = metaUI__df, models = models_to_run,
      estimates = if (nrow(df)) estimatesfiltered() else NULL,
      settings = list(dataset_name = dataset_name, workbook = workbook_name(), primary_model = primary_model,
        correlation = correlation_dependent, aggregation = aggregation_method[1],
        bound = tryCatch(chosen_bound(), error = function(e) NULL), known_models = known_interval_models,
        upload = attr(df, "metaUI_upload_info"), selected_rows = nrow(df),
        exported = format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
      app_dir = metaUI_app_dir, env = environment(metaUI_fit_models))
  }
  code_name <- function(extension) sub("metaUIdata[.]xlsx$", paste0("metaUI-selection.", extension), workbook_name())
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
      sheets <- readxl::excel_sheets(input$uploadData$datapath)
      if (!all(c("dataset", "filters") %in% sheets)) stop("Upload needs dataset and filters sheets.")
      df <- readxl::read_xlsx(input$uploadData$datapath, "dataset")
      df <- metaUI_validate_upload(df, metaUI__df)
      <FILTER>
      for (i in filters) {
        if (i$type == "numeric" && is.logical(df[[i$col]]) && all(is.na(df[[i$col]]))) df[[i$col]] <- as.numeric(df[[i$col]])
        if (i$type == "numeric" && !is.numeric(df[[i$col]])) stop("Uploaded numeric filter has changed type.")
        if (i$type == "selection") {
          if (anyNA(df[[i$col]]) && !"(Missing)" %in% levels(metaUI__df[[i$col]])) stop("Missing categories require a fresh app built with keep_missing_level = TRUE.")
          df[[i$col]][is.na(df[[i$col]])] <- "(Missing)"
          if (any(!is.na(df[[i$col]]) & !df[[i$col]] %in% levels(metaUI__df[[i$col]]))) stop("New categories require a fresh app.")
          df[[i$col]] <- factor(df[[i$col]], levels = levels(metaUI__df[[i$col]]))
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
        filter_values[[i$id]] <- data.frame(selection = filter_values[[i$id]]$selection[!is.na(filter_values[[i$id]]$selection)])
        if (any(!filter_values[[i$id]]$selection %in% levels(metaUI__df[[i$col]]))) stop("Unknown saved category.")
      }
      }
      </FILTER>
      for (id in numeric_ids) {
        values <- suppressWarnings(as.numeric(filter_values[[id]]$selection))
        if (length(values) != 2L || any(!is.finite(values)) || values[1] > values[2]) stop("Invalid saved numeric range: ", id)
      }
      if (length(metaUI_computation_specs(metaUI__df))) {
        value <- filter_values$computation$selection
        if (is.null(value)) value <- "primary"
        if (length(value) != 1L || is.na(value) || !value %in% metaUI_computations(metaUI__df)) stop("Unknown saved computation.")
        filter_values$computation <- data.frame(selection = value)
      }
      list(data = df, filters = filter_values, bound = metaUI_saved_bound(input$uploadData$datapath, sheets))
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
    state_values$upload_info <- list(file = basename(input$uploadData$name), rows = nrow(upload$data), upload_number = upload_number,
      z_rule = derivation$rule, z_disagreements = derivation$supplied_z_disagreements,
      checkbox_note = "Missing-value choices restored (older files: missing values included).")
    # The bound does not refit, so it can be set before the restored analysis.
    updateNumericInput(session, "sesoi", value = upload$bound)
    restore_filters(upload$filters, upload$data, "upload")
  })

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
        if (length(levels(metaUI__df[[i$col]])) >= <<opts$selection_list_threshold>>) {
          shinyWidgets::updatePickerInput(session, inputId = i$id, selected = filter_values[[i$id]]$selection)
        } else {
          updateCheckboxGroupInput(inputId = i$id, selected = filter_values[[i$id]]$selection)
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


  output$link_notice <- renderText(state_values$link_notice)
  session$onFlushed(function() {
    isolate({
    query <- session$clientData$url_search
    if (is.null(query) || !nzchar(query)) return()
    # URL changes after Analyze are publication of state, not a new restore.
    if (state_values$ever_analyzed || !is.null(state_values$uploaded_data)) return()
    restored <- tryCatch(metaUI_parse_selection(query, metaUI_dataset_key, metaUI__df, filters), error = function(e) e)
    if (inherits(restored, "error")) {
      state_values$link_notice <- paste("Link not applied:", conditionMessage(restored), "The app shows every effect instead.")
      return()
    }
    if (is.null(restored)) return()
    updateNumericInput(session, "sesoi", value = if (is.null(restored$sesoi)) NA_real_ else restored$sesoi)
    restore_filters(restored$filters, metaUI__df, "link")
    state_values$link_notice <- "Selection restored from link."
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
