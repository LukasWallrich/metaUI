# Wrap in function so that it can be saved more easily

labels_and_options <- function(dataset_name, correlation = .6) {
  glue::glue('

      # TK - defaults? Advanced options?
      aggregation_method <- c("aggregate", "first")
      correlation_dependent <- {correlation}

      ci_width <- "95 %"

      # Fixed texts
      welcome_title <- HTML("Explore this meta-analysis")
      welcome_text <- HTML("<p>Choose which effects to include with the filters on the left, then click <b>Analyze data</b>. Every tab shows results for the selection you last analysed; if you change a filter afterwards, the app marks the results as out of date until you analyse again.</p><p>The estimators answer different questions and are not interchangeable. The <b>About</b> tab records the data source and the authors\' notes on their primary analysis.</p>")
      dataset_name <- {paste(deparse(as.character(dataset_name)), collapse = "\n")}
      go <- ""
      not_analysed <- HTML("<p><b>No results yet.</b> Choose the effects to include in the left panel, then click <b>Analyze data</b>. Results, plots and diagnostics appear here once the selection has been analysed.</p>")

      summary_overview_main <- HTML("<h3>Selected sample</h3>")
      summary_table_main <- HTML("<h3>Effect size estimates</h3>")
       # Confidence level needs to be changed in all entries in model.R if you want to adjust it
      summary_table_notes <- HTML(glue::glue("<i>Notes:</i> CI = <<ci_width>> confidence interval for frequentist rows; Bayesian rows show central credible intervals. k = model-specific count (effects for multilevel; studies for RVE and study-level fits; trim-and-fill includes imputed effects). The downloaded summary sheet keeps every column, including fitting-scale values, fit times and cache reuse.", .open = "<<", .close = ">>"))
      model_help <- HTML("<details class=\'metaui-help\'><summary>What do the default models do?</summary><dl>
        <dt>Random-Effects Multilevel Model</dt><dd>metafor::rma.mv with REML, random effects for studies and for effects within studies, independent sampling errors and t-based intervals. Uses every selected effect.</dd>
        <dt>Robust Variance Estimation</dt><dd>robumeta::robu with correlated-effects weights (rho = .8) and no small-sample correction, so intervals can be too narrow with few studies.</dd>
        <dt>Trim-and-fill</dt><dd>meta::trimfill on study-level effects (random effects, ML tau-squared, Hartung-Knapp intervals). k includes imputed effects.</dd>
        <dt>P-uniform star; Hedges-Vevea Selection Model</dt><dd>Selection models on study-level effects. They need a declared expected direction and are reported as unsupported without one.</dd>
        <dt>Precision Effect Test (PET); PEESE</dt><dd>Weighted regressions of study-level effects on their standard error (PET) or variance (PEESE); the intercept is reported.</dd>
        </dl><p>Rows added by the authors are described on the About tab.</p></details>")

      sample_overview_main <- HTML("<h3>Sample breakdown</h3>")
      sample_table <- HTML("<h3>List of effect sizes</h3>")
      sample_moderation_main <- HTML("<h3>Simple tests of moderation (ML)</h3>")
      sample_moderation_notes <- HTML("<p class=\'metaui-note\'><i>Notes:</i> This does <i>not</i> consider correlations between moderators, and is thus only intended for exploration, k = model-specific count (effects for multilevel; studies for RVE/aggregated fits; trim-and-fill includes imputed effects).</p>")

      firstvalues <- HTML("<p class=\'metaui-note\'><i>Notes:</i> First effects per study are selected before direction screening for p-curve; opposite/zero effects are excluded with counts. P- and z-curve use normal Wald statistics, not supplied source p-values. Authors must justify this exploratory selection rule.</p>")

      qrppb_main <- HTML("<h3>Publication bias and questionable research practices</h3><p class=\'metaui-note\'>Exploratory small-study and selection diagnostics for the analysed selection.</p>")
      funnel_main <- HTML("<h4>Funnel plot of study-level effects</h4>")

      eggers_main <- HTML("<h4>Egger\'s test of funnel plot asymmetry</h4>")

      pcurve_main <- HTML("<h4>P-curve of effects</h4>")

      zcurve_main <- HTML("<h4>Z-curve of effects (EM, no bootstrapping)</h4>")

      diagnostics_main <- HTML("<h3>Distribution of effect sizes</h3>")
      diagnostics_het <- HTML("<h3>Heterogeneity (REML multilevel model)</h3>")

      scroll <- HTML("<p class=\'metaui-note\'>Each row is one selected effect with a normal 95% sampling interval, grouped by study. Diamonds compare the available multilevel and robust variance estimation (RVE) summaries using their reported intervals. Correlations are shown as r; downloads include fitting-scale values too. These are observed effects, without shrinkage. Wide plots scroll sideways on small screens.</p>")
      zscore_help <- HTML("<p class=\'metaui-help-text\'>z = (effect - mean of all built effects) / their SD, on the fitting scale. Descriptive only; use it to exclude extreme effects.</p>")
      # Result tabs show not_analysed until Analyze data is first clicked
      results_panel <- function(...) tagList(
        conditionalPanel("!(input.go > 0)", div(class = "metaui-empty", role = "status", not_analysed)),
        conditionalPanel("input.go > 0", ...))
      data_help <- HTML("<p class=\'metaui-help-text\'><b>Download</b> saves an .xlsx with the full current dataset (all rows, not only the selection), the filters last applied and the summary of the last analysis.<br/>Bayesian-enabled downloads may wait for two extra prior-sensitivity fits.<br/><b>Upload</b> accepts a file downloaded from this app, possibly with edited rows. It must keep the same columns and effect scale; its saved filters are restored and analysed automatically.</p>")


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
        `aria-label` = paste("Help for", name), icon("info-circle"))
      panel <- tags$div(id = help_id, class = "metaui-filter-help-panel", hidden = NA,
        filter_popups[[name]])

      paste0("HTML(", paste(deparse(as.character(tag)), collapse = "\n"), ")")
    } else ""

    if (is.numeric(data[[filter_col]])) {
      # Round slider ends to (same) appropriate number of significant digits
      l <- log10(max(abs(max(data[[filter_col]], na.rm = TRUE)), abs(min(data[[filter_col]], na.rm = TRUE))))
      # Integer significant digits: two beyond the largest magnitude, at least three.
      # Bounds round outward (floor/ceiling), so the default range retains every row.
      sig_dig <- if (is.finite(l)) max(3, floor(l) + 2) else 3
      values <- data[[filter_col]][!is.na(data[[filter_col]])]
      step <- if (length(values) && all(values == round(values))) ", step = 1" else ""
      out <- glue::glue('
      sliderInput("{filter_col %>% stringr::str_replace_all(" ", "_")}",
      tagList("{stringr::str_remove(filter_col, "metaUI__filter_")}",
      {help}),
          min = {signif_floor(min(data[[filter_col]], na.rm = TRUE), sig_dig)},
          max = {signif_ceiling(max(data[[filter_col]], na.rm = TRUE), sig_dig)},
          value = c({signif_floor(min(data[[filter_col]], na.rm = TRUE), sig_dig)},
          {signif_ceiling(max(data[[filter_col]], na.rm = TRUE), sig_dig)}),
          sep = ""{step}
      )
                 ')

    } else if (is.factor(data[[filter_col]])) {

      choices <- levels(data[[filter_col]]) %>% na.omit()

      out <- glue::glue('{if (length(choices) < opts$selection_list_threshold) "checkboxGroupInput(" else "shinyWidgets::pickerInput(multiple = TRUE, options = list(`actions-box` = TRUE), "} "{filter_col %>% stringr::str_replace_all(" ", "_")}",
      tagList("{stringr::str_remove(filter_col, rm_prefix)}",
      {help}),
          choices = c("{glue::glue_collapse(choices, sep = \'", "\')}"),
        selected = c("{glue::glue_collapse(choices, sep = \'", "\')}")
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
          checkboxInput("{filter_col %>% stringr::str_replace_all(" ", "_")}_include_NA",
          "Include missing values", value = TRUE)
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

  purrr::map2_chr(filter_ids, filter_names, \(fid, fn) {
    glue::glue('
            fluidRow(column(12, h4("{fn}")),
              div(column(4, tableOutput("summary_{fid}_table")),
                column(8, shinycssloaders::withSpinner(plotOutput("summary_{fid}_plot", height = 300)))
              )
            )
                ')
  }) %>% glue::glue_collapse(sep = ",\n") %>% paste0(",\n")


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
    tags$span({paste(deparse(paste0("Dynamic Meta-Analysis of ", dataset_name)), collapse = "\n")}))')

}

generate_mod_tab <- function(data, any_filters) {
  if (!any_filters) return("")
  glue::glue('
    tabPanel(
      "Moderation",
      results_panel(
      sample_moderation_main,
      {generate_moderator_selection(data)},
      fluidRow(
        div(column(4, DT::dataTableOutput("moderation_table"), shiny::htmlOutput("moderation_text")),
            column(8, shinycssloaders::withSpinner(plotly::plotlyOutput("moderation_plot"))))
      ),
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

  fluidPage(
    theme = shinythemes::shinytheme("{opts$shiny_theme}"),
    tags$head(tags$link(rel = "stylesheet", href = "metaui.css"),
      tags$script(src = "metaui.js")),
    shinyjs::useShinyjs(),
    # Application title
    titlePanel(
    windowTitle = {paste(deparse(paste0("Dynamic Meta-Analysis of ", dataset_name)), collapse = "\n")},
    title = {get_favicon_tag(dataset_name)}),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$h2(class = "metaui-panel-title", "Select effects"),
        {if (length(metaUI_computation_specs(data))) paste0(
          \'selectInput("computation", "Effect-size computation", choices = \',
          paste(deparse(metaUI_computations(data)), collapse = "\n"), \', selected = "primary"),\nuiOutput("computation_note"),\') else ""}
        div(id = "filters",
        {generate_ui_filters(data, filter_popups, any_filters, opts = opts)}
        uiOutput("z_score_filter"), zscore_help),
        div(class = "metaui-actions",
          actionButton("go", "Analyze data", class = "btn-primary"),
          actionButton("resetFilters", "Reset filters")),
        div(id = "apply_state_box", class = "metaui-apply-state", role = "status", `aria-live` = "polite",
          textOutput("apply_state")),
        uiOutput("share_selection"), textOutput("link_notice"),
        tags$hr(),
        tags$h2(class = "metaui-panel-title", "Data"),
        actionButton("downloadData", "Download data and results", icon = icon("download")),
        conditionalPanel("false",
          downloadButton("executeDownload", "Execute the download")
        ),
      fileInput("uploadData", "Upload a metaUI .xlsx file",
                multiple = FALSE,
                accept = c(".xlsx")
      ),
      actionButton("executeUpload", "Upload and analyse", icon = icon("upload")),
      data_help
      ),
      mainPanel(
        width = 9,
        # States which selection and data the visible results use
        div(id = "metaui_basis", class = "metaui-basis", role = "status", `aria-live` = "polite",
          textOutput("selection_status"),
          textOutput("calculation_status")),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Summary",
            results_panel(
            go,
            summary_table_main,
            uiOutput("estimate_gaps"),
            div(class = "metaui-scroll", plotOutput("model_comparison", width = "100%", height = "auto") %>% shinycssloaders::withSpinner()),
            div(class = "metaui-scroll", tableOutput("effectestimate")),
            summary_table_notes,
            model_help,
            uiOutput("scale_note"),
            tags$h3("Practical equivalence: interval assessment"),
            numericInput("sesoi", "Smallest effect size of interest (symmetric bound, on the displayed scale)",
              value = NA_real_, min = 0),
            p(class = "metaui-note", "Choose a scientifically justified bound; there is no universal default. Changing it compares the existing intervals without refitting. The threshold column means the interval is contained for any bound strictly larger than that value."),
            uiOutput("equivalence_note"),
            div(class = "metaui-scroll", tableOutput("equivalence")),
            summary_overview_main,
            div(class = "metaui-scroll", tableOutput("sample"))
            )
          ),
          tabPanel(
            "Sample",
            results_panel(
            summary_overview_main,
            div(class = "metaui-scroll", tableOutput("sample_overview")),
            {if (any_filters) "sample_overview_main," else ""}
            {generate_sample_description_ui(data, any_filters)}
            sample_table,
            DT::dataTableOutput("sample_table")
            )
          ),
          {if (!is.null(opts$bayesian)) \'tabPanel("Bayesian sensitivity", results_panel(p("Study-level normal-normal model, flat prior on the mean, half-normal prior on heterogeneity. Aggregation uses the declared within-study correlation or first effect per study. Compare half, baseline and double the heterogeneity prior scale; tau is on the fitting scale. Intervals are 95% central credible intervals and depend on the prior."), tableOutput("bayesian_sensitivity"))),\' else ""}
          {if (length(metaUI_computation_specs(data))) \'tabPanel("Effect computations", results_panel(p("Compare the same selected effects using each author-defined computation. Only the multilevel and RVE models are fitted here. The z-score filter stays anchored to the primary computation."), div(class = "metaui-scroll", tableOutput("computation_comparison")))),\' else ""}
          {generate_mod_tab(data, any_filters)}
          tabPanel("Forest Plot", results_panel(go, scroll, uiOutput("forest_panel"))),
          tabPanel(
            "Publication Bias", results_panel(go, qrppb_main, funnel_main, plotOutput("funnel", width = "100%") %>% shinycssloaders::withSpinner(),
            eggers_main, DT::dataTableOutput("eggers") %>% shinycssloaders::withSpinner(),
            firstvalues,
            pcurve_main, plotOutput("pcurve") %>% shinycssloaders::withSpinner(),
            zcurve_main, plotOutput("zcurve") %>% shinycssloaders::withSpinner(),
            div(class = "metaui-reason", textOutput("zcurve_warnings")))
          ),
          tabPanel(
            "Outlier Diagnostics",
            results_panel(
            diagnostics_main,
            plotly::plotlyOutput("violin", height = 500) %>% shinycssloaders::withSpinner(),
            diagnostics_het,
            div(class = "metaui-scroll", tableOutput("heterogeneity") %>% shinycssloaders::withSpinner()))
          ),
          tabPanel("About", div(class = "metaui-about", HTML({paste(deparse(as.character(about)), collapse = "\n")})))
        )
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

    showModal(modalDialog(
        title = welcome_title,
       welcome_text,
       easyClose = TRUE
      ))

  # Set app states
    state_values <- reactiveValues(
      uploaded_data = NULL,
      upload_info = NULL,
      ever_analyzed = FALSE,
      restore_source = "upload",
      link_notice = NULL
    )

  # Create slider to filter by z-scores
  z_sig_dig <- 2

  insertUI(
    selector = "#z_score_filter",
    where = "beforeEnd",
    ui = sliderInput("outliers_z_scores", "Exclude based on z-scores",
      min = signif_floor(min(metaUI__df$metaUI__es_z), z_sig_dig),
      max = signif_ceiling(max(metaUI__df$metaUI__es_z), z_sig_dig), value = c(
        signif_floor(min(metaUI__df$metaUI__es_z), z_sig_dig),
        signif_ceiling(max(metaUI__df$metaUI__es_z, na.rm = TRUE), z_sig_dig)
      ), sep = ""
    )
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


  capture_filters <- function() {
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

  output$selection_status <- renderText({
    if (!is.null(state_values$pending_upload_filters)) return("Restoring saved filters: waiting for matching browser inputs; review changed bounds before clicking Analyze manually.")
    validate(need(isTruthy(input$go), "No analysis yet. Results will describe the selection at the moment you click Analyze data."))
    df <- df_filtered()
    counts <- attr(df, "metaUI_filter_report")
    filter_names <- sub("^metaUI__filter_", "", names(counts$successive_filter_exclusions))
    filter_names[filter_names == "outliers_z_scores"] <- "z-score range"
    details <- paste(paste(filter_names, unlist(counts$successive_filter_exclusions), sep = ": "), collapse = "; ")
    upload <- attr(df, "metaUI_upload_info")
    paste(if (is.null(upload)) "Built dataset (as published with this app)." else paste0("Uploaded data: ", upload$file,
      " (", upload$rows, " rows); results are not the authors\' dataset. ", upload$z_rule,
      "; supplied z disagreements: ", upload$z_disagreements, ". ", upload$checkbox_note),
      if (length(metaUI_computation_specs(metaUI__df))) paste0("Computation: ",
        names(metaUI_computations(metaUI__df))[match(attr(df, "metaUI_computation"), metaUI_computations(metaUI__df))], ".") else "",
      "Selected", counts$retained_rows, "of", counts$available_rows,
      "rows. Excluded", counts$total_excluded, paste0("by successive filters (", details, ")."))
  })

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

  output$apply_state <- renderText({
    if (!is.null(state_values$pending_upload_filters)) return(paste("Restoring", state_values$restore_source, "filters; they will be analysed automatically."))
    if (!isTruthy(input$go)) return("Not analysed yet.")
    if (filters_changed()) "Filters changed since the last analysis. The results still show the previous selection; click Analyze data to update them."
    else "Results match these filters."
  })

  # Data for forest plot and table ------------------------------------------
  fit_cached <- metaUI_fit_cache(<<opts$fit_cache_entries>>)

  estimatesreactive <- reactive({
    df <- df_filtered()

    # Check if there are any studies left after filtering
    if (nrow(df) == 0) {
      showModal(modalDialog(
        title = "No effect sizes selected!",
        "Your selection criteria do not match any effect sizes. Please adjust them and try again."
      ))

      return(NULL)
    }

   state_values$ever_analyzed <- TRUE

    fit_cached(df, models_to_run, correlation_dependent, aggregation_method[1])
  })

  output$calculation_status <- renderText({
    result <- estimatesreactive()
    req(result)
    paste(if (result$cache_hit) "Reused model estimates from an identical earlier selection in this session (not refitted)." else "Models fitted for this selection.",
          sprintf("Ready in %.3f s (calculation only).", result$calculation_seconds),
          "Reported fit times describe the original fits.")
  })

  estimatesfiltered <- eventReactive(input$go, {
    estimatesreactive()$table
  })

  # Aggregated values meta-analysis ----------------------------------------------


  comparison_cache <- metaUI_fit_cache(6L, limit = 6L)
  comparison_models <- models_to_run[models_to_run$code %in% c(metaUI_code_multilevel, metaUI_code_rve), , drop = FALSE]
  computations_comparison <- eventReactive(input$go, {
    df <- df_filtered()
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
    p(class = "metaui-note", if (length(spec)) spec[[1]]$justification else "Primary computation supplied by the authors.",
      "Click Analyze data to apply a computation change. The z-score filter remains based on the primary computation.")
  })

  bayesian_options <- <<paste(deparse(opts$bayesian), collapse = "\n")>>
  bayesian_sensitivity <- eventReactive(input$go, {
    result <- estimatesreactive()
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
      ~Sources, ~Studies,
      ~Effects, ~`Sample size`,
      if ("metaUI__article_label" %in% names(df)) length(unique(df$metaUI__article_label)) else 0L, length(unique(df$metaUI__study_id)),
      length(df$metaUI__study_id), "not summed" # N totals require a documented independent-sample rule
    )

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

  output$sample <- renderTable(sample_overview())
  output$sample_overview <- renderTable(sample_overview())

  known_interval_models <- <<paste(deparse(opts$known_interval_models), collapse = "\n")>>
  chosen_bound <- reactive({
    value <- input$sesoi
    if (is.null(value) || is.na(value)) return(NULL)
    metaUI_interval_assessment(estimatesfiltered()[0, ], value, metaUI__df$metaUI__display_scale[1])
    value
  })
  interval_assessment <- reactive({
    tryCatch(metaUI_interval_assessment(estimatesfiltered(), chosen_bound(),
      metaUI__df$metaUI__display_scale[1], known_interval_models),
      error = function(e) data.frame(assessment = "Not assessed", reason = conditionMessage(e), bound = input$sesoi))
  })
  output$equivalence <- renderTable(interval_assessment(), digits = 4)
  output$equivalence_note <- renderUI({
    table <- interval_assessment()
    tagList(
      if ("reason" %in% names(table)) p(class = "metaui-reason", table$reason),
      p(class = "metaui-note", "For unchanged default models and fit helpers, strict containment of the reported 95% CI corresponds to two one-sided tests at nominal alpha = .025 each, given the model assumptions. This is stricter than conventional alpha = .05 TOST. Custom intervals have unknown coverage. Bayesian credible intervals are prior-dependent: containment implies at least 95% posterior probability within the bounds, and is not a TOST. The multilevel model uses residual degrees of freedom; RVE has no small-sample correction, so these intervals can be too narrow. Bias-adjusted models describe their own assumptions. This assesses the average effect: individual true effects can still exceed your bound."))
  })

  # MODEL COMPARISON -----------------------------------------------------
  output$model_comparison <- renderPlot({
    estimates_explo_agg <- estimatesfiltered() %>% dplyr::filter(status == "ok")
    validate(need(nrow(estimates_explo_agg) > 0, "No model could be estimated for this selection; see the table below for reasons."))
    estimates_explo_agg$Model <- stringr::str_wrap(estimates_explo_agg$Model, 32)

    bound <- tryCatch(chosen_bound(), error = function(e) NULL)
    ggplot2::ggplot() +
      {if (!is.null(bound)) ggplot2::annotate("rect", xmin = -bound, xmax = bound,
        ymin = -Inf, ymax = Inf, fill = "#dcebf4", alpha = .6)} +
      ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "grey55") +
      ggplot2::geom_errorbar(data = estimates_explo_agg, ggplot2::aes(y = Model, xmin = LCL, xmax = UCL), width = .25, colour = "#22303c") +
      ggplot2::geom_point(data = estimates_explo_agg, ggplot2::aes(x = es, y = Model), size = 2.6, colour = "#1c5a85") +
      ggplot2::labs(x = metaUI_eff_size_type_label, y = NULL) +
      ggplot2::theme_minimal(base_size = 15) +
      ggplot2::scale_y_discrete(limits = rev(unique(estimates_explo_agg$Model))) +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(colour = "#22303c", hjust = 1, lineheight = .9))
  }, height = function() 90 + 52 * max(1, sum(estimatesfiltered()$status == "ok")))

  # Name every model missing from the plot so none disappears silently
  output$estimate_gaps <- renderUI({
    table <- estimatesfiltered()
    missing <- table[table$status != "ok", , drop = FALSE]
    if (!nrow(missing)) return(NULL)
    div(class = "metaui-gaps", tags$b(paste0("Not plotted (", nrow(missing), " of ", nrow(table), " models):")),
      tags$ul(lapply(seq_len(nrow(missing)), function(i) tags$li(tags$b(missing$Model[i]), paste0(" - ",
        missing$status[i], ": ", missing$reason[i])))))
  })

  output$scale_note <- renderUI({
    metric <- metaUI__df$metaUI__es_type[1]
    direction <- metaUI__df$metaUI__direction[1]
    scale_text <- if (metric == "ZCOR") paste("Correlations are fitted as Fisher z. Summary estimates and forest-plot effects are back-transformed to r;",
      "diagnostics, heterogeneity and moderator results stay on the Fisher z scale.") else
      paste0("Effects are fitted and shown on the scale declared by the authors: ", metaUI_eff_size_type_label, ".")
    direction_text <- if (direction == "unspecified") paste("No expected direction was declared, so neither sign is treated as favourable.",
      "Methods that need a direction (in the default set: p-uniform*, the Hedges-Vevea selection model and p-curve) are reported as unsupported rather than guessed.") else
      paste0("The authors declared ", direction, " effects as the expected direction. Direction-dependent methods (p-uniform*, the Hedges-Vevea selection model, p-curve) analyse effects in that direction",
        if (direction == "negative") "; their inputs are sign-flipped for fitting and their estimates flipped back for display" else "", ". Other models are two-sided.")
    dependence_text <- paste0(if (aggregation_method[1] == "first") "Models marked study-level use only the first selected effect from each study." else
      paste0("Models marked study-level first combine the effects within each study (GLS average assuming a correlation of ", correlation_dependent,
      " between effects from the same study)."), " Effect-level models (by default the multilevel model and RVE) use every selected effect. Different studies are assumed to be independent.")
    tags$details(class = "metaui-help", open = NA, tags$summary("Reading these estimates"),
      tags$p(scale_text), tags$p(direction_text), tags$p(dependence_text),
      tags$p("The rows are different estimators applied to the same selection. They answer different questions, are not interchangeable, and the app does not choose between them."))
  })

  # MODEL COMPARISON TABLE -------------------------------------------------------------------
  # Display formatting only: downloads keep the full estimatesfiltered() table.
  output$effectestimate <- renderTable(
    {
      table <- estimatesfiltered()
      fmt <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = 2))
      data.frame(Model = table$Model, Estimate = fmt(table$es),
        `95% interval` = ifelse(is.na(table$LCL) & is.na(table$UCL), "", paste0("[", fmt(table$LCL), ", ", fmt(table$UCL), "]")),
        `Interval type` = table$interval_type,
        k = ifelse(is.na(table$k), "", format(table$k, trim = TRUE)),
        Status = ifelse(table$status == "ok", "estimated", table$status), # keep validation.json vocabulary
        `Reason or warnings` = trimws(paste(table$reason, table$warnings)),
        Fit = ifelse(table$aggregated, "study-level", "effect-level"),
        check.names = FALSE)
    },
    align = "lrrlrlll"
  )


  <FILTER>
  # Sample overview per filter/moderator -----------------------------------------------------------

       <FILTER>
    <<purrr::map_chr(filters, \\(f) {
      if (f$type == "numeric") {
        glue::glue("
    output$`summary_{f$id}_table` <- renderTable({{ # {{ escapes the glue syntax
    df <- df_filtered()
    summarise_numeric(df[[\'{f$col}\']], \'{f$id  %>% stringr::str_remove(\'metaUI__filter_\')}\')
  }})

  output$`summary_{f$id}_plot` <- renderPlot({{
    df <- df_filtered()
    ggplot2::ggplot(df, ggplot2::aes(x = `{f$col}`)) +
      ggplot2::geom_density(na.rm = TRUE) +
      ggplot2::geom_rug(alpha = .1, na.rm = TRUE) +
      ggplot2::theme_light() +
      ggplot2::xlab(\'{f$col %>% stringr::str_remove(\'metaUI__filter_\')}\')
  }})
  ")
      } else {
        glue::glue("
          output$`summary_{f$id}_table` <- renderTable({{
    df <- df_filtered()
    summarise_categorical(df[[\'{f$col}\']], \'{f$col  %>% stringr::str_remove(\'metaUI__filter_\')}\')
  }})

  output$`summary_{f$id}_plot` <- renderPlot({{
    df <- df_filtered()

    counts <- summarise_categorical(df[[\'{f$col}\']], \'{f$col  %>% stringr::str_remove(\'metaUI__filter_\')}\')

    ggplot2::ggplot(counts, ggplot2::aes(x = reorder(.data[[\'{f$col %>% stringr::str_remove(\'metaUI__filter_\')}\']], Count), y = Count)) +
      ggplot2::geom_col(fill = \'#337ab7\') + ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = \'Effects\') + ggplot2::theme_minimal()

  }})
        ")
      }
    }) %>% paste(collapse = "\n")>>

  </FILTER>

  # Sample table -----------------------------------------------------------

  output$sample_table <- DT::renderDataTable({
    df <- df_filtered()

    out <- df %>%
      dplyr::select(dplyr::any_of("metaUI__article_label"),
        Study =
          "metaUI__study_id", N = "metaUI__N",
        "Effect size" = "metaUI__effect_size", "Source p (primary)" = "metaUI__pvalue",
        dplyr::starts_with("metaUI__filter_"), dplyr::any_of("metaUI__url")
      ) %>%
      dplyr::rename_with(~ stringr::str_replace(.x, "metaUI__filter_", "") %>%
        stringr::str_replace("metaUI__article_label", "Source") %>%
        stringr::str_replace("metaUI__url", "URL")) %>%
      dplyr::mutate(p = fmt_p(p, include_equal = FALSE)) %>%
      dplyr::arrange(.data$Study)

    if ("Source" %in% names(out)) {
      out <- out %>% dplyr::mutate(Study = stringr::str_remove(Study, Source) %>%
        stringr::str_remove("^[[:punct:] ]+"))
    }

    if ("URL" %in% names(out)) {
      out <- out %>% dplyr::mutate(URL = glue::glue("<a href={URL}>{URL}</a>"))
    }

    out %>% DT::datatable(
      rownames = FALSE,
      caption = tags$caption(
        style = "caption-side: bottom; text-align: left; margin: 8px 0;",
        glue::glue("The effect size is given as {metaUI_eff_size_type_label}")
      )
    )
  })

 <FILTER>

  # Moderators  -----------------------------------------------------------


  output$moderation_plot <- plotly::renderPlotly({
    df <- df_filtered()
    mod <- df[[input$moderator]]
    df$mod <- df[[input$moderator]]

    if (is.numeric(mod)) {
      p <- ggplot2::ggplot(data = df, ggplot2::aes(y = metaUI__effect_size, x = mod)) +
        ggplot2::geom_point() +
        ggplot2::theme_bw() +
        ggplot2::xlab(input$moderator %>% stringr::str_remove("metaUI__filter_")) +
        ggplot2::ylab(metaUI_eff_size_type_label) +
        ggplot2::geom_smooth(data = df, ggplot2::aes(y = metaUI__effect_size, x = mod, color = NULL), formula = y ~ x, method = "lm") +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
    } else {
      p <- ggplot2::ggplot(data = df, ggplot2::aes(y = metaUI__effect_size, x = forcats::fct_rev(mod))) +
        ggplot2::geom_violin(fill = NA) +
        ggplot2::theme_bw() +
        ggplot2::geom_jitter(width = .1, alpha = .25) +
        ggplot2::xlab(input$moderator %>% stringr::str_remove("metaUI__filter_")) +
        ggplot2::ylab(metaUI_eff_size_type_label) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        ggplot2::coord_flip()
    }
    plotly::ggplotly(p) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  })


  moderator_model <- reactive({
    df <- df_filtered()

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
      \'<br><br>The {ifelse(is.numeric(df[[input$moderator]]), "linear ", "")}relationship between  <b> {input$moderator  %>% stringr::str_remove("metaUI__filter_")} </b>\',
      \'and the observed effect sizes <b>is {ifelse(model[["QMp"]] < .05, "", "not ")}significant </b> at the 5% level. \',
     \' Test of moderators: <i>F</i>({model[["QMdf"]][1]}, {model[["QMdf"]][2]}) = {round(model[["QM"]], digits = 2)}, \',
      \'<i>p</i> {fmt_p(model[["QMp"]])}.\'
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
      \'<br><br>The {ifelse(is.numeric(df[[input$moderator]]), "linear ", "")}relationship between  <b> {input$moderator  %>% stringr::str_remove("metaUI__filter_")} </b>\',
      \'and the observed effect sizes <b>is {ifelse(model_sig[["QMp"]] < .05, "", "not ")} significant</b> at the 5% level. \',
     \' Test of moderators: <i>F</i>({model_sig[["QMdf"]][1]}, {model_sig[["QMdf"]][2]}) = {round(model_sig[["QM"]], digits = 2)}, \',
      \'<i>p</i> {fmt_p(model_sig[["QMp"]])}.\'
    ))
    }
    model$moderation_text <- moderation_text
  model
  })


  output$moderation_table <- DT::renderDataTable(escape = FALSE, {
    model <- moderator_model()
    df <- df_filtered()
    if (is.numeric(df[[input$moderator]])) {
      modtable <- psych::describe(df[input$moderator] %>% as.data.frame(), fast = TRUE) # [c(2:5, 8, 9)]
      modtable[, 2:6] <- round_(as.data.frame(modtable)[, 2:6], digits = 2)
      modtable <- modtable %>% dplyr::rename(k = n)
      modtable$Intercept <- round_(model$b[1], digits = 2)
      modtable$`&beta;` <- round_(model$b[2], digits = 2)
      modtable$vars <- NULL
      modtable$range <- NULL
      modtable$se <- NULL

      modtable <- modtable %>% dplyr::mutate(Moderator = input$moderator %>% stringr::str_remove("metaUI__filter_"),
                                            dplyr::across(dplyr::everything(), as.character))  %>%
                               dplyr::select("Moderator", Intercept, "&beta;", "k", dplyr::everything()) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "Statistic", values_to = "Value")

    } else {
      modtable <- data.frame(
        "Moderator Levels" = rownames(model$b) %>% stringr::str_remove(input$moderator) %>%
        stringr::str_remove_all("`"), #Needed to support spaces in moderator names
        "Effect size" = round(model$b, digits = 3),
        "95% CI" = fmt_ci(model$ci.lb, model$ci.ub, digits = 3), check.names = FALSE
      )

      modtable <- df %>% dplyr::count(!!rlang::sym(input$moderator)) %>% rename(k = n) %>%
      left_join(modtable, ., by = c("Moderator Levels" = input$moderator))

    }

    modtable %>% DT::datatable(options = list(dom = "t"), escape = FALSE)
  })

  output$moderation_text <- shiny::renderText({
    model <- moderator_model()
    model$moderation_text
  })

  </FILTER>

  # Heterogeneity -----------------------------------------------------------

  output$heterogeneity <- renderTable({
    df <- df_filtered()

    req(estimatesreactive())
    metapp_total <- metaUI_reuse_fit(estimatesreactive(), models_to_run,
      metaUI_code_multilevel, df, function() metaUI_multilevel_fit(df))

    het <- metaUI_heterogeneity(metapp_total, df)
    het$Q <- round(het$Q, 2)
    het$Q_p <- fmt_p(het$Q_p, include_equal = FALSE)

    names(het) <- c("Study-level tau^2", "Effect-level tau^2 (within studies)", "Total tau^2", "Q", "p (Q)", "Variance components")
    het
  }, digits = 3)

  # FOREST PLOT FOR ALL INCLUDED STUDIES ------------------------------------
  forest_limit_message <- "Forest plots can only be displayed with <<opts$max_forest_plot_rows>> effect sizes or fewer. Use the filters to narrow the selection, or download the data to create a larger plot in another tool."
  output$forest_panel <- renderUI({
    df <- df_filtered()
    if (!nrow(df)) return(p(class = "metaui-reason", role = "status", "No eligible rows selected for the forest plot."))
    if (nrow(df) > <<opts$max_forest_plot_rows>>) {
      return(p(class = "metaui-reason", role = "status", forest_limit_message))
    }
    tagList(
      downloadButton("forest_pdf", "Download PDF"),
      downloadButton("forest_png", "Download PNG"),
      downloadButton("forest_csv", "Download plot data"),
      div(class = "metaui-scroll", shinycssloaders::withSpinner(
        plotOutput("foreststudies", height = "auto"))))
  })
  forest_rows <- reactive({
    df <- df_filtered()
    validate(need(nrow(df) > 0, "No eligible rows selected for the forest plot."),
      need(nrow(df) <= <<opts$max_forest_plot_rows>>, forest_limit_message))
    metaUI_forest_rows(df, estimatesfiltered())
  })
  forest_plot <- reactive(metaUI_forest_plot(forest_rows(), df_filtered()$metaUI__display_scale[1]))
  forest_height <- function() 150 + 25 * nrow(forest_rows())
  output$foreststudies <- renderPlot(print(forest_plot()), height = forest_height, width = 900)
  output$forest_pdf <- downloadHandler(filename = function() "forest-plot.pdf", content = function(file) {
    ggplot2::ggsave(file, forest_plot(), device = "pdf", width = 10,
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

    meta::metagen(
      TE = metaUI__effect_size,
      seTE = metaUI__se,
      data = df_agg,
      studlab = df_agg$metaUI__study_id,
      common = FALSE,
      random = TRUE,
      method.tau = "ML", # as recommended by  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4950030/
      method.random.ci = "HK",
      prediction = TRUE,
      sm = df_agg$metaUI__es_type[1]
    )
  })

  output$funnel <- renderPlot({
    meta_agg <- meta_agg()

    metafor::funnel(meta_agg, xlab = metaUI_eff_size_type_label, studlab = FALSE, contour = .95, col.contour = "light grey")
  })

  output$eggers <- DT::renderDataTable({
    meta_agg <- meta_agg()

    eggers <- meta::metabias(meta_agg, k.min = 3, method.bias = "Egger")
    eggers_table <- data.frame(
      "Intercept" = eggers$estimate, "Residual variance (tau<sup>2</sup>)" = eggers$tau^2,
      "t" = eggers$statistic,
      "p" = ifelse(round(eggers$p.value, 3) == 0, "< .001", round(eggers$p.value, 3)),
      check.names = FALSE
    )
    eggers_table[1, ] %>% DT::datatable(options = list(dom = "t"), escape = FALSE) %>%
     DT::formatRound(columns = 1:3, digits=3)

  })


  # PCURVE ------------------------------------------------------------------

  output$pcurve <- renderPlot({
    df <- df_filtered()
    selected <- tryCatch(metaUI_pcurve_data(df), error = function(e) e)
    validate(need(!inherits(selected, "error"), if (inherits(selected, "error")) conditionMessage(selected) else ""))
    message("P-curve direction-contrary exclusions: ", attr(selected, "direction_exclusions"))
    result <- tryCatch(pcurve(selected, effect.estimation = FALSE), error = function(e) e)
    validate(need(!inherits(result, "error"), if (inherits(result, "error")) conditionMessage(result) else ""))
    graphics::mtext(paste("Direction-contrary exclusions:", attr(selected, "direction_exclusions")), side = 3, line = 0)
  })


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
    paste(if (inherits(value$result, "error")) paste("Not estimated:", conditionMessage(value$result)) else "",
          if (length(value$warnings)) paste("Warnings:", paste(value$warnings, collapse = "; ")) else "")
  })

  output$zcurve <- renderPlot({
    value <- zcurve_fit()
    validate(need(!inherits(value$result, "error"), "Z-curve not estimated for this selection; see the estimator reason below."))
    zcurve::plot.zcurve(value$result, annotation = TRUE, main = "")
  })

  # VIOLIN PLOTLY -------------------------------------------------------------
  output$violin <- plotly::renderPlotly({
    df <- df_filtered()

    efm <- mean(df$metaUI__effect_size)
    efsd <- sd(df$metaUI__effect_size)

    # Outliers in boxplot are quartiles + 1.5 * IQR - so cutsoffs calculated here to show points with labels
    qs <- quantile(df$metaUI__effect_size, c(.25, .75))
    bounds <- qs
    bounds[1] <- bounds[1] - 1.5 * diff(range(qs))
    bounds[2] <- bounds[2] + 1.5 * diff(range(qs))

    outliers <- df %>% dplyr::filter(metaUI__effect_size < bounds[1] | metaUI__effect_size > bounds[2])

    violinplot <- ggplot2::ggplot(data = df, ggplot2::aes(x = 1, y = metaUI__effect_size)) +
      ggplot2::xlab("") +
      ggplot2::geom_violin(fill = grDevices::rgb(100 / 255, 180 / 255, 1, .5)) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(name = metaUI_eff_size_type_label) +
      ggplot2::geom_jitter(data = outliers, shape = 16, position = ggplot2::position_jitter(width = .1, height = 0), mapping = ggplot2::aes(group = metaUI__study_id)) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      ) +
      ggplot2::geom_boxplot(width = .25, outlier.shape = NA) +
      ggplot2::theme(text = ggplot2::element_text(size = 10))

    ay <- list(
      tickfont = list(size = 11.7),
      titlefont = list(size = 14.6),
      overlaying = "y",
      nticks = 5,
      side = "right",
      title = "Standardized effect size (z-score)"
    )

    plotly_plot <- plotly::ggplotly(violinplot, tooltip = c("y", "group")) %>%
      plotly::config(modeBarButtons = list(list("toImage")), displaylogo = FALSE) %>%
      plotly::add_lines(
        x = ~1, y = ~ (metaUI__effect_size - efm) / efsd, colors = NULL, yaxis = "y2",
        data = df, showlegend = FALSE, inherit = FALSE
      ) %>%
      plotly::layout(
        yaxis2 = ay,
        margin = list(
          r = 45
        )
      )

    # Hide boxplot outliers so that they are not shown multiple times
    plotly_plot$x$data <- lapply(plotly_plot$x$data, FUN = function(x) {
      if (x$type == "box") {
        x$marker <- list(opacity = 0)
      }
      return(x)
    })

    plotly_plot
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
      equivalence = if (nrow(df_filtered())) interval_assessment() else data.frame(assessment = "Not assessed", reason = "No eligible rows selected"),
      provenance = data.frame(field = c("uploaded", "file", "z_rule"),
        value = c(as.character(!is.null(upload_info)),
          if (is.null(upload_info)) "built dataset" else upload_info$file,
          if (is.null(upload_info)) "prepare_data descriptive standardisation" else upload_info$z_rule))
    )
  })


  output$executeDownload <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "-", gsub(" ", "_", dataset_name), "-metaUIdata.xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(data_list(), file)
    }
  )

  # The visible action triggers this hidden link through JavaScript.
  outputOptions(output, "executeDownload", suspendWhenHidden = FALSE)

  observeEvent(input$downloadData, {
    if (!is.null(state_values$pending_upload_filters)) {
      showModal(modalDialog(title = "Restoring filters", "Wait for the uploaded selection to finish before downloading."))
      return()
    }
    if (state_values$ever_analyzed == TRUE) {
      shinyjs::runjs("$(\'#executeDownload\')[0].click();")
    } else {
      showModal(modalDialog(title = "Download not yet possible", HTML("Click on <i>Analyze data</i> first. As the download will also include model estimates, these need to be created first.")))
    }

}
)

  # UPLOAD ----------------------------------------------------------------

  observeEvent(input$executeUpload, {
    req(input$uploadData)
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
      list(data = df, filters = filter_values)
    }, error = function(e) e)
    if (inherits(upload, "error")) {
      showModal(modalDialog(title = "Invalid upload", conditionMessage(upload)))
      return()
    }
    attr(fit_cached, "clear")()
    state_values$uploaded_data <- upload$data
    derivation <- attr(upload$data, "metaUI_upload_derivations")
    state_values$upload_info <- list(file = basename(input$uploadData$name), rows = nrow(upload$data),
      z_rule = derivation$rule, z_disagreements = derivation$supplied_z_disagreements,
      checkbox_note = "Missing-value choices restored; older files without them default to including missing values.")
    filter_values <- upload$filters
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
      state_values$link_notice <- if (state_values$restore_source == "upload") "Uploaded data not yet analysed; review filters and click Analyze. Downloads retain the previous analysed data." else "Could not restore; review the filters and click Analyze."
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
      state_values$link_notice <- paste("Link not applied:", conditionMessage(restored), "Review filters and click Analyze manually.")
      return()
    }
    if (is.null(restored)) return()
    updateNumericInput(session, "sesoi", value = if (is.null(restored$sesoi)) NA_real_ else restored$sesoi)
    restore_filters(restored$filters, metaUI__df, "link")
    state_values$link_notice <- "Selection restored from a link to the built dataset."
    })
  }, once = TRUE)
  selection_query <- reactive({
    req(isTruthy(input$go))
    if (!is.null(attr(df_filtered(), "metaUI_upload_info"))) return(NULL)
    bound <- tryCatch(chosen_bound(), error = function(e) NULL)
    metaUI_selection_query(attr(df_filtered(), "metaUI_applied_filters"), metaUI_dataset_key, bound)
  })
  observeEvent(list(input$go, input$sesoi), {
    req(isTruthy(input$go))
    query <- tryCatch(selection_query(), error = function(e) NULL)
    updateQueryString(if (is.null(query)) "?" else query, mode = "replace", session = session)
  }, ignoreInit = TRUE)
  output$share_selection <- renderUI({
    req(isTruthy(input$go))
    if (!is.null(attr(df_filtered(), "metaUI_upload_info"))) return(p(class = "metaui-note",
      "Uploaded data are private to this session. Share the workbook; a filter URL cannot recreate those data."))
    query <- tryCatch(selection_query(), error = function(e) e)
    if (inherits(query, "error")) return(p(class = "metaui-note", conditionMessage(query)))
    p(class = "metaui-note", tags$a(href = query, "Link to this applied selection"),
      "Copy this link or the address bar to share results for the built dataset.")
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
