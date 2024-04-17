# Wrap in function so that it can be saved more easily

labels_and_options <- function(dataset_name) {
  glue::glue('

      # TK - defaults? Advanced options?
      aggregation_method <- c("aggregate", "first")
      correlation_dependent <- .6

      ci_width <- "95 %"

      # Fixed texts
      welcome_title <- HTML("Welcome to our dynamic meta-analysis app!")
      welcome_text <- HTML("<br /><p style=\'color:blue;\'>To get started, choose a set of studies and click on <b>Analyze data</b>.</p><br/>")
      dataset_name <- "{dataset_name}"
      go <- ""
      #HTML("<br /><p style=\'color:blue;\'>Choose your set of studies and click on <b>Analyze data</b> to see the results.</p><br/>")

      summary_overview_main <- HTML("<br/><br/><h3>Sample Overview</h3>") # <b></b>
      summary_table_main <- HTML("<br/><br/><h3>Effect Size Estimates</h3>") # <b></b>
       # Confidence level needs to be changed in all entries in model.R if you want to adjust it
      summary_table_notes <- HTML(glue::glue("<i>Notes:</i> LCL = Lower <<ci_width>> Confidence Limit, UCL = Upper <<ci_width>> Confidence Limit, k = number of effects.", .open = "<<", .close = ">>"))

      sample_overview_main <- HTML("<br/><br/><h3>Sample Breakdown</h3>") # <b></b>
      sample_table <- HTML("<br/><br/><h3>List of Effect Sizes</h3>") # <b></b>
      sample_moderation_main <- HTML("<br/><br/><h3>Simple tests of moderation</h3>") # <b></b>
      sample_moderation_notes <- HTML("<br /><i>Notes:</i> This does <i>not</i> consider correlations between moderators, and is thus only intended for exploration, k = number of effects.")

      firstvalues <- HTML("<br/><br/><i>Notes:</i> First reported <i>p</i>-values are selected for p- and z-curve analyses.")

      qrppb_main <- HTML("<h3>Publication Bias and Questionable Research Practices</h3>")
      funnel_main <- HTML("<h3>Funnel Plot of Effects</h3>")

      eggers_main <- HTML("<h3>Egger\'s Test of Funnel Plot Asymmetry</h3>")

      pcurve_main <- HTML("<h3>P-Curve of Effects")

      zcurve_main <- HTML("<h3>Z-Curve of Effects (EM via EM, no bootstrapping)")

      diagnostics_main <- HTML("<h3>Distribution of Effect Sizes")
      diagnostics_het <- HTML("<h3>Heterogeneity (REML)<h5>")

      scroll <- HTML("Scroll down to see forest plot.")

     # Favicon is included as base64 code
      favicon <- "{readLines(system.file("template_code", "favicon.base64", package = "metaUI"))}"

  ')
}



# Wrapped in function so that it is only created once custom variables are set


generate_ui_filters <- function(data, filter_popups, any_filters) {
  if (!any_filters) return("")
  filter_cols <- colnames(data) %>% stringr::str_subset("metaUI__filter_")

  purrr::map_chr(filter_cols, function(filter_col) {
    add_popup <- stringr::str_remove(filter_col, "metaUI__filter_") %in% names(filter_popups)
    if (add_popup) id <- paste0("i", sample(1:10e6, 1))
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
      out <- glue::glue('checkboxGroupInput("{filter_col %>% stringr::str_replace_all(" ", "_")}",
      p("{stringr::str_remove(filter_col, rm_prefix)}",
      {if(add_popup)  {{
          glue::glue("
          shinyBS::popify(shinyBS::bsButton(\'{id}\', label = \'\', icon = icon(\'info\'), style = \'color: #fff; background-color: #337ab7; border-color: #2e6da4\', size = \'extra-small\'),
                        \'{stringr::str_remove(filter_col, rm_prefix)}\',
                        \'{escape_quotes(filter_popups[stringr::str_remove(filter_col, rm_prefix)])}\')
                    ")
          }} else  ""}),
          choices = c("{glue::glue_collapse(levels(data[[filter_col]]) %>% na.omit(), sep = \'", "\')}"),
        selected = c("{glue::glue_collapse(levels(data[[filter_col]]) %>% na.omit(), sep = \'", "\')}")
        )')
    } else {
      stop("Filter/moderator variables must be numeric or factors. This check failed first for ", filter_col)
    }

    if (is.numeric(data[[filter_col]])) {
    # Add option to exclude/include NA values if there are any
    if (any(is.na(data[[filter_col]]))) {
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
    stringr::str_subset("metaUI__filter_") %>% stringr::str_replace_all(" ", "_")
  filter_names <- stringr::str_remove(filter_ids, "metaUI__filter_")

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
  favicon <- readLines(system.file("template_code", "favicon.base64", package = "metaUI"))
  glue::glue('tagList(
                 tags$head(tags$link(rel=\"icon\",
                                href=\"data:image/x-icon;base64,{favicon}\",
                                type=\"image/x-icon\")),
                  tags$span(\"Dynamic Meta-Analysis of {dataset_name}\"))
      ')
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

generate_ui <- function(data, dataset_name, about, filter_popups) {

  # Check whether data contains any filters
  filter_cols <- colnames(data) %>% stringr::str_subset("metaUI__filter_")
  if (length(filter_cols) == 0) any_filters <- FALSE else any_filters <- TRUE

  out <- glue::glue('

  fluidPage(
    theme = shinythemes::shinytheme("yeti"),
    # Application title
    titlePanel(
    windowTitle = glue::glue("Dynamic Meta-Analysis of {dataset_name}"),
    title = {get_favicon_tag(dataset_name)}),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(id = "filters",
        {generate_ui_filters(data, filter_popups, any_filters)}
        uiOutput("z_score_filter")),
        actionButton("go", "Analyze data"),
        actionButton("resetFilters", "Reset filters"),
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
            zcurve_main, plotOutput("zcurve") %>% shinycssloaders::withSpinner()
          ),
          tabPanel(
            "Outlier Diagnostics",
            diagnostics_main,
            plotly::plotlyOutput("violin", height = 500) %>% shinycssloaders::withSpinner(),
            diagnostics_het,
            tableOutput("heterogeneity") %>% shinycssloaders::withSpinner()
          ),
          tabPanel("About", HTML("{about}"))
        )
      )
    )
  )
  ')
  out
}


generate_server <- function(metaUI__df) {

  # Check whether data contains any filters
  filter_cols <- colnames(metaUI__df) %>% stringr::str_subset("metaUI__filter_")
  if (length(filter_cols) == 0) any_filters <- FALSE else any_filters <- TRUE


glue_string <- ('
    function(input, output) {

    showModal(modalDialog(
        title = welcome_title,
       welcome_text,
       easyClose = TRUE
      ))

  # Set app states
    state_values <- reactiveValues(
      to_upload = FALSE,
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

  file_input <- reactive({
    if (state_values$to_upload == TRUE) {
      return(input$uploadData)
    } else {
      return(NULL)
    }
  })

  # Apply reactive filtering of dataset when clicking on the button
  df_filtered <- eventReactive(input$go, {
    df_reactive()
  })

  # Reset filters
       observeEvent(input$resetFilters, {
        shinyjs::reset("filters")
      })


  df_reactive <- reactive({
    if (!is.null(file_input())) {
      df <- readxl::read_xlsx(input$uploadData$datapath, "dataset")
      filter_values <- readxl::read_xlsx(input$uploadData$datapath, "filters") %>% split(.$id)
      <FILTER>
      for (i in filters) {
        if (i$type == "numeric") {
          updateSliderInput(inputId = i$id, value = c(as.numeric(filter_values[[i$id]]$selection[1]), as.numeric(filter_values[[i$id]]$selection[2])))
        } else {
          updateCheckboxGroupInput(inputId = i$id, selected = filter_values[[i$id]]$selection)
        }
      }
       </FILTER>

          updateSliderInput(inputId = "outliers_z_scores", value = c(as.numeric(filter_values[["outliers_z_scores"]]$selection[1]), as.numeric(filter_values[["outliers_z_scores"]]$selection[2])))

      state_values$to_upload <- FALSE
    } else {
      df <- metaUI__df
    }


    <FILTER>
    # Filter by specified metadata filters
    <<purrr::map_chr(filters, \\(f) {
      if (f$type == "numeric") {
        #Using isTRUE because isTRUE(NULL) == TRUE and input$nonexistent == NULL
        glue::glue("df <- df[(df[[\'{f$col}\']] >= input[[\'{f$id}\']][1] & df[[\'{f$col}\']] <= input[[\'{f$id}\']][2]) |
        (is.na(df[[\'{f$col}\']]) & isTRUE(input[[\'{f$id}_include_NA\']])), ]")
      } else {
        glue::glue("df <- df[df[[\'{f$col}\']] %in% input[[\'{f$id}\']], ]")
      }
    }) %>% paste(collapse = "\n")>>
    </FILTER>


    # Filter by zscore
    df <- df[df$metaUI__es_z >= input$outliers_z_scores[1] & df$metaUI__es_z <= input$outliers_z_scores[2], ]
    df
  })

  # Data for forest plot and table ------------------------------------------

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

    if (aggregation_method[1] == "aggregate") {
      # TK - do we want this, or actually just average, despite the problems with that?
      # Aggregate dependent effects based on https://www.jepusto.com/sometimes-aggregating-effect-sizes-is-fine/
      # note that this requires an assumption regarding the degree of correlation
      agg_effects <- function(yi, vi, r = correlation_dependent) {
        corr_mat <- r + diag(1 - r, nrow = length(vi))
        sd_mat <- tcrossprod(sqrt(vi))
        V_inv_mat <- chol2inv(chol(sd_mat * corr_mat))
        V <- 1 / sum(V_inv_mat)
        data.frame(es = V * sum(yi * V_inv_mat), var = V)
      }

      df_agg <-
        df %>%
        dplyr::group_by(metaUI__study_id) %>%
        dplyr::summarise(
          es = list(agg_effects(yi = metaUI__effect_size, vi = metaUI__variance)),
          metaUI__N = max(metaUI__N),
          metaUI__es_type = dplyr::first(metaUI__es_type), # Could also aggregate filters - if length(unique(FILTER)) == 1
          .groups = "drop"
        ) %>%
        tidyr::unnest(cols = "es") %>%
        dplyr::rename(metaUI__effect_size = es, metaUI__variance = var) %>%
        dplyr::mutate(metaUI__se = sqrt(metaUI__variance))
    } else if (aggregation_method[1] == "first") {
      df_agg <-
        df %>%
        dplyr::group_by(metaUI__study_id) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()
    } else {
      stop("Aggregation method not recognized")
    }

    # Run all specified models
    models <- purrr::pmap(models_to_run, \\(...){
      mod_spec <- tibble::tibble(...)
      if (mod_spec$aggregated == TRUE) {
        df <- df_agg
      }
      mod <- try(eval(parse(text = mod_spec$code)))
      if ("try-error" %in% class(mod)) {{
        warning("Model ", mod_spec$name, " could not be estimated. Error was ", mod)
        mod <- NULL
        mod_res <- tibble::tibble(
            Model = mod_spec$name,
            es = NA_real_,
            LCL = NA_real_,
            UCL = NA_real_,
            k = NA_real_
          )
      }} else {{
        mod_res <- tibble::tibble(
          Model = mod_spec$name,
          es = eval(parse(text = mod_spec$es)) %>% as.numeric(),
          LCL = eval(parse(text = mod_spec$LCL)) %>% as.numeric(),
          UCL = eval(parse(text = mod_spec$UCL)) %>% as.numeric(),
          k = eval(parse(text = mod_spec$k)) %>% as.numeric()
        )
      }}
      list(mod = mod, mod_res = mod_res)
    }) %>% purrr::transpose()

    # Generate table

    estimates_explo_agg <- models$mod_res %>% dplyr::bind_rows() %>%
      dplyr::mutate(Model = factor(Model, levels = Model))

    print(estimates_explo_agg)

    list(df_agg = df_agg, table = estimates_explo_agg)
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
      length(unique(df$metaUI__article_label)), length(unique(df$metaUI__study_id)),
      length(df$metaUI__study_id), sum(aggregate(metaUI__N ~ metaUI__study_id, data = df, FUN = "min")$metaUI__N) %>% round()
    )

    if (overview$Sources == 0) {
      overview$Sources <- "not specified"
    }

    message(paste("The current dataset contains", overview$Sources, "sources,", overview$Studies,
      "independent studies and", overview$Effects, "effects.",
      sep = " "
    ))

    if (overview$Sources == "not specified") {
      overview$Sources <- NULL
    }

    overview
  })

  # MODEL COMPARISON -----------------------------------------------------
  output$model_comparison <- renderPlot({
    estimates_explo_agg <- estimatesfiltered()

    ggplot2::ggplot() +
      ggplot2::geom_point(data = estimates_explo_agg, ggplot2::aes(x = es, y = Model), stat = "identity") +
      ggplot2::geom_vline(xintercept = 0, linetype = 2) +
      ggplot2::xlab(metaUI_eff_size_type_label) +
      ggplot2::geom_errorbar(data = estimates_explo_agg, ggplot2::aes(y = Model, xmin = LCL, xmax = UCL), stat = "identity") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_discrete(limits = rev(levels(estimates_explo_agg$Model))) +
      ggplot2::theme(text = ggplot2::element_text(size = 20))
  })



  # MODEL COMPARISON TABLE -------------------------------------------------------------------
  output$effectestimate <- renderTable(
    {
      estimatesfiltered()  %>%
        dplyr::mutate(k = as.integer(k)) # Remove decimal points from k
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
      ggplot2::geom_density() +
      ggplot2::geom_rug(alpha = .1) +
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

    counts$Count %>%
      setNames(counts[[\'{f$col  %>% stringr::str_remove(\'metaUI__filter_\')}\']]) %>%
      waffle::waffle(rows = ceiling(sqrt(sum(.) / 2)), size = max(2, 2 / (sum(.) / 100)))
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
        ggplot2::xlab(input$moderator) +
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
        random = ~ 1 | metaUI__study_id/metaUI__effect_size,
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
        random = ~ 1 | metaUI__study_id/metaUI__effect_size,
        tdist = TRUE,
        data = df,
        mods = as.formula(glue::glue("~`{input$moderator}`")),
        method = "ML",
        sparse = TRUE
      )
     model <- metafor::rma.mv(
        yi = metaUI__effect_size,
        V = metaUI__variance,
        random = ~ 1 | metaUI__study_id/metaUI__effect_size,
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

    metapp_total <- metafor::rma.mv(
      yi = metaUI__effect_size,
      V = metaUI__variance,
      random = ~ 1 | metaUI__study_id/metaUI__effect_size,
      tdist = TRUE, # knapp-hartung adjustment
      data = df,
      method = "ML" # REML failed to converge in tests,
      sparse = TRUE
    )

    het <- data.frame(
      "Sigma2_Level1" = metapp_total$sigma2[1],
      "Sigma2_Level2" = metapp_total$sigma2[2],
      "Tau" = metapp_total$tau2,
      "Q" = round(metapp_total$QE, digits = 2),
      "Q_p" = fmt_p(metapp_total$QEp, include_equal = FALSE)
      )

    print(het)
  })

  # FOREST PLOT FOR ALL INCLUDED STUDIES ------------------------------------
  output$foreststudies <- renderPlot(
    {
      # TK - reconsider which package to use for forest plots
      df <- df_filtered()

      rve <- robumeta::robu(metaUI__effect_size ~ 1, data = df, studynum = metaUI__study_id, var.eff.size = metaUI__variance, small = FALSE)

      robumeta::forest.robu(rve,
        es.lab = "metaUI__es_label", study.lab = "metaUI__study_id",
        "Effect size" = metaUI__effect_size
      )
    },
    # TK - create a function that adjusts the height of the plot based on the number of studies
    height = 7500,
    width = 900 # , height = 7500, width = 900
  )

  # FUNNEL PLOT -------------------------------------------------------------

  meta_agg <- reactive({
    df_agg <- df_agg()

    meta::metagen(
      TE = metaUI__effect_size,
      seTE = metaUI__se,
      data = df_agg,
      studlab = df_agg$metaUI__study_id,
      comb.fixed = FALSE,
      comb.random = TRUE,
      method.tau = "ML", # as recommended by  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4950030/
      hakn = TRUE,
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
      "Intercept" = eggers$estimate, "Tau<sup>2</sup>" = eggers$tau,
      "t" = eggers$statistic,
      "p" = ifelse(round(eggers$p.value, 3) == 0, "< .001", round(eggers$p.value, 3)),
      check.names = FALSE
    )
    eggers_table[1, ] %>% DT::datatable(options = list(dom = "t"), escape = FALSE) %>%
     DT::formatRound(columns = 1:3, digits=3)

  })


  # PCURVE ------------------------------------------------------------------

  output$pcurve <- renderPlot({
    # Plot is created as side-effect in pcurve function - so needs to be recalculated here
    df <- df_filtered()

    pp_first <- df %>%
      dplyr::group_by(metaUI__study_id) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::rename(
        "studlab" = metaUI__study_id,
        "TE" = metaUI__effect_size,
        "seTE" = metaUI__se,
        "n" = metaUI__N
      )

    pcurve_estimates1 <- try(pcurve(pp_first, effect.estimation = FALSE, N = pp_first$n, dmin = 0, dmax = 1), silent = FALSE)

    pcurve_estimates1 <- ifelse(substr(pcurve_estimates1, 1, 5) == "Error", 0, pcurve_estimates1)

    pcurve_estimates1
  })

  # ZCURVE ------------------------------------------------------------------


  output$zcurve <- renderPlot({
    df <- df_filtered()

    # Use only first p-value as they need to be statistically independent
    df_first <- df %>%
      dplyr::group_by(metaUI__study_id) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::rename(
        "studlab" = metaUI__study_id,
        "TE" = metaUI__effect_size,
        "seTE" = metaUI__se,
        "n" = metaUI__N
      )

    df_first$z <- abs(df_first$TE / df_first$seTE)

    zcurve_estimates1 <- try(zcurve::zcurve(df_first$z, bootstrap = FALSE), silent = TRUE)

    zcurve::plot.zcurve(zcurve_estimates1, annotation = TRUE, main = "")
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
      ggplot2::geom_jitter(data = outliers, shape = 16, position = ggplot2::position_jitter(width = .1, height = 0), mapping = ggplot2::aes(text = metaUI__study_id)) +
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

    plotly_plot <- plotly::ggplotly(violinplot, tooltip = "text") %>%
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
    filter_selections <- tibble::tibble(id = "outliers_z_scores", selection = input[["outliers_z_scores"]])
    <FILTER>
    for (i in filters) {
      filter_selections <- rbind(filter_selections, tibble::tibble(id = i$id, selection = input[[i$id]]))
    }
    </FILTER>

    list(
      dataset = df_filtered(),
      summary = as.data.frame(estimatesfiltered()),
      filters = filter_selections
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
    if (state_values$ever_analyzed == TRUE) {
      shinyjs::runjs("$(\'#executeDownload\')[0].click();")
    } else {
      showModal(modalDialog(title = "Download not yet possible", HTML("Click on <i>Analyze data</i> first. As the download will also include model estimates, these need to be created first.")))
    }

}
)

  # UPLOAD ----------------------------------------------------------------

  observeEvent(input$executeUpload, {
    state_values$to_upload <- TRUE

    if (class(try(nrow(input$uploadData))) != "try-error") {
      sheets <- readxl::excel_sheets(input$uploadData$datapath)
      if (!("dataset" %in% sheets && "filters" %in% sheets)) {
      showModal(modalDialog(title = "Invalid file", "The file needs to contain a dataset and a filters sheet. Typically, you should start from a file downloaded from this application."))
      } else {
        shinyjs::runjs("$(\'#go\')[0].click();")
      }
    } else {
      showModal(modalDialog(title = "No file selected", HTML("Make sure to select a file prior to upload")))
    }
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
