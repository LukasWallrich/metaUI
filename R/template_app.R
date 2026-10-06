# Wrap in function so that it can be saved more easily

labels_and_options <- function(dataset_name, correlation = .6) {
  glue::glue('

      # TK - defaults? Advanced options?
      aggregation_method <- c("aggregate", "first")
      correlation_dependent <- {correlation}

      ci_width <- "95 %"

      # Fixed texts
      welcome_title <- HTML("Welcome to our dynamic meta-analysis app!")
      welcome_text <- HTML("<br /><p style=\'color:blue;\'>To get started, choose a set of studies and click on <b>Analyze data</b>.</p><br/>")
      dataset_name <- {paste(deparse(as.character(dataset_name)), collapse = "\n")}
      go <- ""
      #HTML("<br /><p style=\'color:blue;\'>Choose your set of studies and click on <b>Analyze data</b> to see the results.</p><br/>")

      summary_overview_main <- HTML("<br/><br/><h3>Sample Overview</h3>") # <b></b>
      summary_table_main <- HTML("<br/><br/><h3>Effect Size Estimates</h3>") # <b></b>
       # Confidence level needs to be changed in all entries in model.R if you want to adjust it
      summary_table_notes <- HTML(glue::glue("<i>Notes:</i> Correlation summaries are back-transformed to r. Other diagnostics, moderators, and raw forest plots use the fitting scale; variance components remain on that scale. Study-level GLS aggregation assumes within-study correlation {correlation}.  LCL = Lower <<ci_width>> Confidence Limit, UCL = Upper <<ci_width>> Confidence Limit, k = model-specific count (effects for multilevel; studies for RVE/aggregated fits; trim-and-fill includes imputed effects).", .open = "<<", .close = ">>"))

      sample_overview_main <- HTML("<br/><br/><h3>Sample Breakdown</h3>") # <b></b>
      sample_table <- HTML("<br/><br/><h3>List of Effect Sizes</h3>") # <b></b>
      sample_moderation_main <- HTML("<br/><br/><h3>Simple tests of moderation (ML)</h3>") # <b></b>
      sample_moderation_notes <- HTML("<br /><i>Notes:</i> This does <i>not</i> consider correlations between moderators, and is thus only intended for exploration, k = model-specific count (effects for multilevel; studies for RVE/aggregated fits; trim-and-fill includes imputed effects).")

      firstvalues <- HTML("<br/><br/><i>Notes:</i> First effects per study are selected before direction screening for p-curve; opposite/zero effects are excluded with counts. P- and z-curve use normal Wald statistics, not supplied source p-values. Authors must justify this exploratory selection rule.")

      qrppb_main <- HTML("<h3>Publication Bias and Questionable Research Practices</h3>")
      funnel_main <- HTML("<h3>Funnel Plot of Effects</h3>")

      eggers_main <- HTML("<h3>Egger\'s Test of Funnel Plot Asymmetry</h3>")

      pcurve_main <- HTML("<h3>P-Curve of Effects")

      zcurve_main <- HTML("<h3>Z-Curve of Effects (EM via EM, no bootstrapping)")

      diagnostics_main <- HTML("<h3>Distribution of Effect Sizes")
      diagnostics_het <- HTML("<h3>Heterogeneity (REML)<h5>")

      scroll <- HTML("Scroll down to see forest plot.")


  ')
}



# Wrapped in function so that it is only created once custom variables are set

generate_ui_filters <- function(data, filter_popups, any_filters, opts = opts) {
  if (!any_filters) return("")
  filter_cols <- colnames(data) %>% stringr::str_subset("metaUI__filter_")

  purrr::map_chr(filter_cols, function(filter_col) {
    add_popup <- stringr::str_remove(filter_col, "metaUI__filter_") %in% names(filter_popups)
    if (add_popup) id <- paste0("i", which(colnames(data) == filter_col))
    rm_prefix <- "metaUI__filter_"

    if (is.numeric(data[[filter_col]])) {
      # Round slider ends to (same) appropriate number of significant digits
      l <- log10(max(abs(max(data[[filter_col]], na.rm = TRUE)), abs(min(data[[filter_col]], na.rm = TRUE))))
      sig_dig <- dplyr::case_when(
        l < 2 ~ min(max(abs(l), 2), 4),
        l >= 4 ~ 4,
        TRUE ~ l
      )
      sig_dig <- log10(max(abs(max(data[[filter_col]], na.rm = TRUE)), abs(min(data[[filter_col]], na.rm = TRUE)))) + 1
      out <- glue::glue('
      sliderInput("{filter_col %>% stringr::str_replace_all(" ", "_")}",
      p("{stringr::str_remove(filter_col, "metaUI__filter_")}",
      {if(add_popup)  {{
          glue::glue("
          shinyBS::popify(shinyBS::bsButton(\'{id}\', label = \'\', icon = icon(\'info\'), style = \'color: #fff; background-color: #337ab7; border-color: #2e6da4\', size = \'extra-small\'),
                        \'{stringr::str_remove(filter_col, rm_prefix)}\',
                        \'{escape_quotes(filter_popups[stringr::str_remove(filter_col, rm_prefix)])}\')
                    ")
          }} else  ""}),
          min = {signif_floor(min(data[[filter_col]], na.rm = TRUE), sig_dig)},
          max = {signif_ceiling(max(data[[filter_col]], na.rm = TRUE), sig_dig)},
          value = c({signif_floor(min(data[[filter_col]], na.rm = TRUE), sig_dig)},
          {signif_ceiling(max(data[[filter_col]], na.rm = TRUE), sig_dig)}),
          sep = ""
      )
                 ')

    } else if (is.factor(data[[filter_col]])) {

      choices <- levels(data[[filter_col]]) %>% na.omit()

      out <- glue::glue('{if (length(choices) < opts$selection_list_threshold) "checkboxGroupInput(" else "shinyWidgets::pickerInput(multiple = TRUE, options = list(`actions-box` = TRUE), "} "{filter_col %>% stringr::str_replace_all(" ", "_")}",
      p("{stringr::str_remove(filter_col, rm_prefix)}",
      {if(add_popup)  {{
          glue::glue("
          shinyBS::popify(shinyBS::bsButton(\'{id}\', label = \'\', icon = icon(\'info\'), style = \'color: #fff; background-color: #337ab7; border-color: #2e6da4\', size = \'extra-small\'),
                        \'{stringr::str_remove(filter_col, rm_prefix)}\',
                        \'{escape_quotes(filter_popups[stringr::str_remove(filter_col, rm_prefix)])}\')
                    ")
          }} else  ""}),
          choices = c("{glue::glue_collapse(choices, sep = \'", "\')}"),
        selected = c("{glue::glue_collapse(choices, sep = \'", "\')}")
        )')
    } else {
      stop("Filter/moderator variables must be numeric or factors. This check failed first for ", filter_col)
    }

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
            fluidRow(h4("{fn}"),
              div(column(4, tableOutput("summary_{fid}_table")),
                column(7, shinycssloaders::withSpinner(plotOutput("summary_{fid}_plot")))
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
      sample_moderation_main,
      {generate_moderator_selection(data)},
      fluidRow(
        div(column(4, DT::dataTableOutput("moderation_table"), shiny::htmlOutput("moderation_text")),
            column(7, shinycssloaders::withSpinner(plotly::plotlyOutput("moderation_plot"))))
      ),
      sample_moderation_notes
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
    # Application title
    titlePanel(
    windowTitle = {paste(deparse(paste0("Dynamic Meta-Analysis of ", dataset_name)), collapse = "\n")},
    title = {get_favicon_tag(dataset_name)}),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(id = "filters",
        {generate_ui_filters(data, filter_popups, any_filters, opts = opts)}
        uiOutput("z_score_filter")),
        actionButton("go", "Analyze data"),
        actionButton("resetFilters", "Reset filters"),
        textOutput("selection_status"),
        tags$hr(),
        shinyjs::useShinyjs(),
        actionButton("downloadData", "Download dataset", icon = icon("download")),
        conditionalPanel("false",
          downloadButton("executeDownload", "Execute the download")
        ),
             # Input: Select a file ----
        tags$hr(),
      fileInput("uploadData", "Upload metaUI xlsx file",
                multiple = FALSE,
                accept = c(".xlsx")
      ),
      actionButton("executeUpload", "Upload dataset", icon = icon("upload")),
      ),
      # Show a plot of the generated distribution
      mainPanel(
        width = 9,
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Summary",
            go,
            summary_overview_main,
            tableOutput("sample") %>% shinycssloaders::withSpinner(),
            summary_table_main,
            plotOutput("model_comparison", width = "100%") %>% shinycssloaders::withSpinner(),
            div(),
            tableOutput("effectestimate"),
            textOutput("calculation_status"),
            summary_table_notes
          ),
          tabPanel(
            "Sample",
            {if (any_filters) "sample_overview_main," else ""}
            {generate_sample_description_ui(data, any_filters)}
            h3(sample_table),
            DT::dataTableOutput("sample_table"),
          ),
          {generate_mod_tab(data, any_filters)}
          tabPanel("Forest Plot", go, scroll, plotOutput("foreststudies") %>% shinycssloaders::withSpinner(), cellArgs = list(style = "vertical-align: top")),
          tabPanel(
            "QRP/PB", go, qrppb_main, funnel_main, plotOutput("funnel", width = "100%") %>% shinycssloaders::withSpinner(),
            eggers_main, DT::dataTableOutput("eggers") %>% shinycssloaders::withSpinner(),
            firstvalues,
            pcurve_main, plotOutput("pcurve") %>% shinycssloaders::withSpinner(),
            zcurve_main, plotOutput("zcurve") %>% shinycssloaders::withSpinner(),
            textOutput("zcurve_warnings")
          ),
          tabPanel(
            "Outlier Diagnostics",
            diagnostics_main,
            plotly::plotlyOutput("violin", height = 500) %>% shinycssloaders::withSpinner(),
            diagnostics_het,
            tableOutput("heterogeneity") %>% shinycssloaders::withSpinner()
          ),
          tabPanel("About", HTML({paste(deparse(as.character(about)), collapse = "\n")}))
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
      ever_analyzed = FALSE
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
        signif_ceiling(max(metaUI__df$metaUI__es_z, na.rm = TRUE))
      ), sep = ""
    )
  )
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
    filter_selections
  }

  df_reactive <- reactive({
    applied_filters <- capture_filters()
    df <- if (is.null(state_values$uploaded_data)) metaUI__df else state_values$uploaded_data


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
    attr(df, "metaUI_applied_filters") <- applied_filters
    attr(df, "metaUI_filter_report") <- list(available_rows = available_rows,
      retained_rows = nrow(df), total_excluded = available_rows - nrow(df), successive_filter_exclusions = counts)
    df
  })

  output$selection_status <- renderText({
    if (!is.null(state_values$pending_upload_filters)) return("Restoring saved filters: waiting for matching browser inputs; review changed bounds before clicking Analyze manually.")
    df <- df_filtered()
    counts <- attr(df, "metaUI_filter_report")
    details <- paste(paste(names(counts$successive_filter_exclusions), unlist(counts$successive_filter_exclusions), sep = ": "), collapse = "; ")
    upload <- state_values$upload_info
    paste(if (is.null(upload)) "Built dataset." else paste0("Uploaded data: ", upload$file,
      " (", upload$rows, " rows); results are not the authors\' dataset. ", upload$z_rule,
      "; supplied z disagreements: ", upload$z_disagreements, ". ", upload$checkbox_note),
      "Selected", counts$retained_rows, "of", counts$available_rows,
      "rows. Excluded", counts$total_excluded, "by successive filters:", details)
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
    paste(if (result$cache_hit) "Reused this session\'s identical selection." else "Calculated this selection.",
          sprintf("Ready in %.3f s (calculation only).", result$calculation_seconds),
          "Reported fit times describe the original fits.")
  })

  estimatesfiltered <- eventReactive(input$go, {
    estimatesreactive()$table
  })

  # Aggregated values meta-analysis ----------------------------------------------


  df_agg <- reactive({
    estimatesreactive()$df_agg
  })

  output$sample <- renderTable({
    df <- df_filtered()

    overview <- tibble::tribble(
      ~Sources, ~Studies,
      ~Effects, ~`Sample size`,
      if ("metaUI__article_label" %in% names(df)) length(unique(df$metaUI__article_label)) else 0L, length(unique(df$metaUI__study_id)),
      length(df$metaUI__study_id), NA_real_ # N totals require a documented independent-sample rule
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

  # MODEL COMPARISON -----------------------------------------------------
  output$model_comparison <- renderPlot({
    estimates_explo_agg <- estimatesfiltered() %>% dplyr::filter(status == "ok")

    ggplot2::ggplot() +
      ggplot2::geom_point(data = estimates_explo_agg, ggplot2::aes(x = es, y = Model), stat = "identity") +
      ggplot2::geom_vline(xintercept = 0, linetype = 2) +
      ggplot2::xlab(metaUI_eff_size_type_label) +
      ggplot2::geom_errorbar(data = estimates_explo_agg, ggplot2::aes(y = Model, xmin = LCL, xmax = UCL), stat = "identity") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_discrete(limits = rev(unique(estimates_explo_agg$Model))) +
      ggplot2::theme(text = ggplot2::element_text(size = 20))
  })



  # MODEL COMPARISON TABLE -------------------------------------------------------------------
  output$effectestimate <- renderTable(
    {
      estimatesfiltered()  %>%
        dplyr::select(Model, es, LCL, UCL, k, status, reason, warnings, aggregated, cache_hit, filtered_rows)
    },
    digits = 2
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
        "Effect size" = "metaUI__effect_size", p = "metaUI__pvalue",
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

    het
  })

  # FOREST PLOT FOR ALL INCLUDED STUDIES ------------------------------------
  output$foreststudies <- renderPlot(
    {
      # TK - reconsider which package to use for forest plots
      df <- df_filtered()

      validate(
         need(nrow(df) > 0, "No eligible rows selected for the forest plot."),
         need(nrow(df) <= <<opts$max_forest_plot_rows>>, "Forest plots can only be displayed with <<opts$max_forest_plot_rows>> effect sizes or fewer. Use the filters to narrow the selection if possible. If you really want a forest plot with more effect sizes, you will need to download the data and create it in a different tool where you have customization options that keep it legible.")
      )

      rve <- metaUI_reuse_fit(estimatesreactive(), models_to_run,
        metaUI_code_rve, df, function() metaUI_rve_fit(df))

      robumeta::forest.robu(rve,
        es.lab = "metaUI__es_label", study.lab = "metaUI__study_id",
        "Effect size" = metaUI__effect_size
      )
    },
    # TK - create a function that adjusts the height of the plot based on the number of studies
    height = function () if (nrow(df_filtered()) > <<opts$max_forest_plot_rows>>) 200 else 400 + 25 * nrow(df_filtered()),
    width = 900
    )

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
    filter_selections <- attr(df_filtered(), "metaUI_applied_filters")

    list(
      dataset = if (is.null(state_values$uploaded_data)) metaUI__df else state_values$uploaded_data,
      summary = if (nrow(df_filtered())) as.data.frame(estimatesfiltered()) else data.frame(status = "unsupported", reason = "No eligible rows selected"),
      filters = filter_selections,
      provenance = data.frame(field = c("uploaded", "file", "z_rule"),
        value = c(as.character(!is.null(state_values$upload_info)),
          if (is.null(state_values$upload_info)) "built dataset" else state_values$upload_info$file,
          if (is.null(state_values$upload_info)) "prepare_data descriptive standardisation" else state_values$upload_info$z_rule))
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
    <FILTER>
    for (i in filters) {
      if (i$type == "numeric") {
        flag <- filter_values[[paste0(i$id, "_include_NA")]]$selection
        include <- if (is.null(flag)) TRUE else identical(toupper(as.character(flag[1])), "TRUE")
        updateCheckboxInput(inputId = paste0(i$id, "_include_NA"), value = include)
        selection <- as.numeric(filter_values[[i$id]]$selection[1:2])
        limits <- range(c(upload$data[[i$col]], selection), na.rm = TRUE)
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
    z_limits <- range(c(upload$data$metaUI__es_z, z_selection))
    z_step <- if (diff(z_selection) > 0) diff(z_selection) / 1000 else 1e-8
    updateSliderInput(inputId = "outliers_z_scores",
      min = z_selection[1] - z_step * ceiling((z_selection[1] - min(z_limits)) / z_step),
      max = z_selection[2] + z_step * ceiling((max(z_limits) - z_selection[2]) / z_step),
      step = z_step, value = z_selection)
    state_values$pending_upload_filters <- filter_values
  })

  # Wait for browser input bindings to acknowledge every restored selection.
  observe({
    saved <- state_values$pending_upload_filters
    req(!is.null(saved))
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
    req(matches)
    state_values$pending_upload_filters <- NULL
    shinyjs::runjs("$(\'#go\')[0].click();")
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
