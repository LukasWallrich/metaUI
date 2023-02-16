
# Fixed texts
go <- HTML("<p style=\"color:blue\">Choose your set of studies and click on <b>Analyze data</b> to see the results.</p><br/>")

summary_overview_main <- HTML("<br/><br/><h3>Sample Overview</h3>") # <b></b>
summary_table_main <- HTML("<br/><br/><h3>Effect Size Estimates</h3>") # <b></b>
summary_table_notes <- HTML("<i>Notes:</i> LCL = Lower Confidence Limit, UCL = Upper Confidence Limit, k = number of effects.")

#TK - is this true - unsure for funnel plot?
firstvalues <- HTML("<br/><br/><i>Notes:</i> First reported values are selected for analyses.")

qrppb_main <- HTML("<h3>Publication Bias and Questionable Research Practices</h3>")
funnel_main <- HTML("<h3>Funnel Plot of Effects</h3>")

eggers_main <- HTML("<h3>Egger's Test of Funnel Plot Asymmetry</h3>")

pcurve_main <- HTML("<h3>P-Curve of Effects")

# TK - should the first significant value be used or the first value?
pcurve_notes <- HTML(paste(
  "<h5><br/><br/><i>Notes:</i> P-curve can only be plotted if there are 2 or more significant effects. Only the first <i>p</i>-value",
  "coded from each study is considered here, since the <i>p</i>-values need to be statistically independent.</h5>"
))

diagnostics_main <- HTML("<h3>Distribution of Effect Sizes")
diagnostics_het <- HTML("<h3>Heterogeneity (REML)<h5>")
diagnostics_notes <- HTML("<h5><br/><br/><i>Notes:</i> More tools for outlier diagnostics will be added soon.</h5>")

scroll <- HTML("Scroll down to see forest plot.")

# Define UI for application that draws a histogram
# Wrapped in function so that it is only created once custom variables are set

generate_ui <- function(dataset_name, about) {
  fluidPage(

    # Application title
    titlePanel(glue::glue("Dynamic Meta-Analysis of {dataset_name}")),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        uiOutput("userDefinedFilters"),
        actionButton("go", "Analyze data")#,
        #downloadButton("downloadData", "Download dataset") # does not work yet (see roadmap)
      ),

      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Summary",
            go,
            summary_overview_main,
            tableOutput("sample") %>% shinycssloaders::withSpinner(),
            summary_table_main,
            plotOutput("model_comparison", width = "100%") %>% shinycssloaders::withSpinner(),
            tableOutput("effectestimate"),
            summary_table_notes
          ),
          tabPanel("Forest Plot", go, scroll, plotOutput("foreststudies") %>% shinycssloaders::withSpinner(), cellArgs = list(style = "vertical-align: top")),
          tabPanel(
            "QRP/PB", go, qrppb_main, firstvalues, funnel_main, plotOutput("funnel", width = "100%") %>% shinycssloaders::withSpinner(),
            eggers_main, tableOutput("eggers") %>% shinycssloaders::withSpinner(),
            pcurve_main, plotOutput("pcurve") %>% shinycssloaders::withSpinner(), pcurve_notes
          ),
          tabPanel(
            "Outlier Diagnostics",
            diagnostics_main,
            plotly::plotlyOutput("violin", height = 500) %>% shinycssloaders::withSpinner(),
            diagnostics_het,
            tableOutput("heterogeneity") %>% shinycssloaders::withSpinner(),
            diagnostics_notes
          ),
          tabPanel("About", about)
        )
      )
    )
  )
}

# Define server logic
server <- function(input, output) { # , session

  filter_cols <- colnames(metaUI__df) %>% stringr::str_subset("metaUI__filter_")
  for (i in seq_along(filter_cols)) {

    if (is.numeric(metaUI__df[[filter_cols[i]]])) {
      out <- sliderInput(filter_cols[i], stringr::str_remove(filter_cols[i], "metaUI__filter_"),
                         min = min(metaUI__df[[filter_cols[i]]], na.rm = TRUE),
                         max = max(metaUI__df[[filter_cols[i]]], na.rm = TRUE), value = c(min(metaUI__df[[filter_cols[i]]], na.rm = TRUE), max(metaUI__df[[filter_cols[i]]], na.rm = TRUE)), sep = ""
      )

    } else {
      out <- checkboxGroupInput(filter_cols[i], stringr::str_remove(filter_cols[i], "metaUI__filter_"), choices = unique(metaUI__df[[filter_cols[i]]]), selected = unique(metaUI__df[[filter_cols[i]]]))
    }

    insertUI(
      selector = "#userDefinedFilters",
      where = "beforeEnd",
      ui = out
    )
  }
  insertUI(
    selector = "#userDefinedFilters",
    where = "beforeEnd",
    ui = sliderInput("outliers", "Exclude based on z-scores", min = floor(min(metaUI__df$metaUI__es_z)*10)/10, max = ceiling(max(metaUI__df$metaUI__es_z)*10)/10, value = c(floor(min(metaUI__df$metaUI__es_z)*10)/10, ceiling(max(metaUI__df$metaUI__es_z)*10)/10), step = .1, sep = "")
    )

  filters_types <- purrr::map_chr(filter_cols, \(x) {if(is.numeric(metaUI__df[[x]])) "numeric" else "selection"})

  filters <- purrr::map2(filter_cols, filters_types, ~list(id = .x, type = .y))

  # Apply reactive filtering of dataset when clicking on the button
  df_filtered <- eventReactive(input$go, {
    df_reactive()
  })

  df_reactive <- reactive({
    pp <- metaUI__df

    for (i in seq_along(filters)) {
      if (filters[[i]]$type == "numeric") {
        pp <- pp[pp[[filters[[i]]$id]] >= input[[filters[[i]]$id]][1] & pp[[filters[[i]]$id]] <= input[[filters[[i]]$id]][2], ]
      } else {
        pp <- pp[pp[[filters[[i]]$id]] %in% input[[filters[[i]]$id]], ]
      }
    }

    # Filter by zscore
    pp <- pp[pp$metaUI__es_z >= input$outliers[1] & pp$metaUI__es_z <= input$outliers[2], ]
    pp
  })

  # Data for forest plot and table ------------------------------------------

  estimatesfiltered <- eventReactive(input$go, {

    estimatesreactive()$table
  })

  estimatesreactive <- reactive({
    pp <- df_filtered()


    # Check if there are any studies left after filtering
    if (nrow(pp) == 0) {
      showModal(modalDialog(
        title = "No effect sizes selected!",
        "Your selection criteria do not match any effect sizes. Please adjust them and try again."
      ))

      return(NULL)
    }

        metapp_total <- metafor::rma.mv(
      yi = metaUI__effect_size,
      V = metaUI__variance,
      random = ~ 1 | metaUI__study_id,
      tdist = TRUE, # knapp-hartung adjustment
      data = pp,
      method = "REML"
    )

    pp_agg <- pp %>%
      dplyr::group_by(metaUI__study_id) %>%
      dplyr::summarise(
        dplyr::across(c(metaUI__effect_size, metaUI__se, metaUI__N), mean, na.rm = TRUE),
        metaUI__es_type = dplyr::first(metaUI__es_type),
        .groups = "drop"
      )

    pp_agg$vi <- metafor::escalc(
      measure = pp_agg$metaUI__es_type[1],
      yi = pp_agg$metaUI__effect_size,
      sei = pp_agg$metaUI__se,
      ni = pp_agg$metaUI__N,
      data = pp_agg
    )$vi

    metapp_metagen_mean <- meta::metagen(
      TE = metaUI__effect_size,
      seTE = metaUI__se,
      data = pp_agg,
      studlab = pp_agg$metaUI__study_id,
      comb.fixed = FALSE,
      comb.random = TRUE,
      method.tau = "ML", # as recommended by  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4950030/
      hakn = TRUE,
      prediction = TRUE,
      sm = pp_agg$metaUI__es_type[1]
    )

    pp_first <- pp %>%
      dplyr::group_by(metaUI__study_id) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::rename(
        "studlab" = metaUI__study_id,
        "TE" = metaUI__effect_size,
        "seTE" = metaUI__se,
        "n" = metaUI__N
      )

    pp_last <- pp %>%
      dplyr::group_by(metaUI__study_id) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::rename(
        "studlab" = metaUI__study_id,
        "TE" = metaUI__effect_size,
        "seTE" = metaUI__se,
        "n" = metaUI__N
      )

    # TK - this always returns positive es estimates - so what about situations when effect is negative? Just align sign?
    pcurve_estimates1 <- (purrr::possibly(pcurve, otherwise = list(dEstimate = NA, kAnalyzed = NA), quiet = FALSE))(pp_first, effect.estimation = TRUE, N = pp_first$n, dmin = 0, dmax = max(pp_first$TE))
    pcurve_estimates2 <- (purrr::possibly(pcurve, otherwise = list(dEstimate = NA, kAnalyzed = NA), quiet = FALSE))(pp_last, effect.estimation = TRUE, N = pp_first$n, dmin = 0, max(pp_last$TE))

    taf_agg <- metafor::trimfill(metapp_metagen_mean)

    punif_agg <- puniform::puni_star(
      yi = pp_agg$metaUI__effect_size, vi = pp_agg$metaUI__se^2,
      alpha = .05,
      side = "right", method = "ML", boot = FALSE
    )


    # PET PEESE for aggregated data

    pet_agg <- lm(metaUI__effect_size ~ sqrt(vi), data = pp_agg, weights = 1 / vi)

    peese_agg <- lm(metaUI__effect_size ~ vi, data = pp_agg, weights = 1 / vi)

    # RVE
    # TK - should we use user-provided variance here (and then also in other robumeta calls below)
    rve <- robumeta::robu(metaUI__effect_size ~ 1, data = pp, studynum = metaUI__study_id, var.eff.size = metaUI__se * sqrt(metaUI__N), small = FALSE)

    # Hedges-Vevea selection model
    hvsm <- weightr::weightfunct(pp_agg$metaUI__effect_size, pp_agg$vi, steps = c(0.025, 1), fe = FALSE)

    # Generate table and plot

    estimates_explo_agg <- tibble::tribble(
      ~Model, ~es, ~LCL, ~UCL, ~k,
      "Random-Effects Multilevel Model", metapp_total$b, metapp_total$ci.lb, metapp_total$ci.ub, metapp_total$k,
      "Robust Variance Estimation", as.numeric(rve$reg_table$b.r), rve$reg_table$CI.L, rve$reg_table$CI.U, length(rve$k),
      "Trim-and-fill", taf_agg$TE.random, taf_agg$lower.random, taf_agg$upper.random, taf_agg$k,
      "P-uniform star", punif_agg$est, punif_agg$ci.lb, punif_agg$ci.ub, punif_agg$k,
      "Hedges-Vevea Selection Model", as.numeric(hvsm$output_adj$par[2]), hvsm$ci.lb_adj[2], hvsm$ci.ub_adj[2], hvsm$k,
      "P-Curve (first value)", pcurve_estimates1$dEstimate, NA, NA, pcurve_estimates1$kAnalyzed,
      "P-Curve (last value)", pcurve_estimates2$dEstimate, NA, NA, pcurve_estimates2$kAnalyzed,
      "Precision Effect Test", as.numeric(pet_agg$coefficients[1]), confint(pet_agg)[1, 1], confint(pet_agg)[1, 2], pet_agg$df+2,
      "Precision Effect Estimate using Standard Error", as.numeric(peese_agg$coefficients[1]), confint(peese_agg)[1, 1], confint(peese_agg)[1, 2], peese_agg$df+2
    ) %>%
      dplyr::mutate(dplyr::across(c(es, LCL, UCL), as.numeric))

    print(estimates_explo_agg)

    list(metapp_agg = metapp_metagen_mean, table = estimates_explo_agg, metapp_total = metapp_total, p_curve = pcurve_estimates1)
  })



  # Aggregated values meta-analysis ----------------------------------------------


  metapp_agg <- eventReactive(input$go, {
    estimatesreactive()$metapp_agg
  })

  output$sample <- renderTable({
    pp <- df_filtered()

    overview <- tibble::tribble(
      ~Sources, ~Studies,
      ~Effects, ~`Sample size`,
      length(unique(pp$metaUI__article_label)), length(unique(pp$metaUI__study_id)),
      length(pp$metaUI__study_id), sum(aggregate(metaUI__N ~ metaUI__study_id, data = pp, FUN = "min")$metaUI__N) %>% round()
    )

    if (overview$Sources == 0) {
      overview$Sources <- "not specified"
    }

    print(paste("The current dataset contains", overview$Sources, "sources,", overview$Studies,
                "independent studies and", overview$Effects, "effects.",
                sep = " "
    ))

    if (overview$Sources == "not specified") {
      overview$Sources <- NULL
    }

    overview
  })



  # Heterogeneity -----------------------------------------------------------

  metapp_total <- eventReactive(input$go, {
    estimatesreactive()$metapp_total
  })

  output$heterogeneity <- renderTable({
    metapp_total_model <- metapp_total()
    het <- data.frame(
      "Sigma2_Level1" = metapp_total_model$sigma2[1],
      "Sigma2_Level2" = metapp_total_model$sigma2[2],
      "Tau" = metapp_total_model$tau2,
      "Q" = round(metapp_total_model$QE, digits = 2),
      "Q_p" = ifelse(round(metapp_total_model$QEp, digits = 3) == 0,
                     "< .001", round(metapp_total_model$QEp, digits = 3)
      )
    )


    print(het)
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
      estimatesfiltered()
    },
    digits = 2
  )


  # FOREST PLOT FOR ALL INCLUDED STUDIES ------------------------------------
  output$foreststudies <- renderPlot(
    {
      # TK - which CI method do we want here? Do we want to switch to a package that gives nicer plots?
      pp <- df_filtered()

      rve <- robumeta::robu(metaUI__effect_size ~ 1, data = pp, studynum = metaUI__study_id, var.eff.size = metaUI__se * sqrt(metaUI__N), small = FALSE)

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

  output$funnel <- renderPlot({
    metapp_metagen_mean <- metapp_agg()

    metafor::funnel(metapp_metagen_mean, xlab = metaUI_eff_size_type_label, studlab = FALSE, contour = .95, col.contour = "light grey")
  })

  output$eggers <- renderTable({
    metapp_metagen_mean <- metapp_agg()

    # eggers <- eggers.test(x = metapp_metagen_mean)
    # names(eggers) <- c("Intercept",	"95% CI",	"t",	"p")
    eggers <- meta::metabias(metapp_metagen_mean, k.min = 3, method.bias = "Egger")
    eggers_table <- data.frame(
      "Intercept" = eggers$estimate, "Tau²" = eggers$tau,
      "t" = eggers$statistic,
      "p" = ifelse(round(eggers$p.value, 3) == 0, "< .001", round(eggers$p.value, 3))
    )
    print(eggers_table[1, ])
  })


  # PCURVE ------------------------------------------------------------------

  output$pcurve <- renderPlot({
    # Plot is created as side-effect in pcurve function - so needs to be recalculated here
    pp <- df_filtered()

    pp_first <- pp %>%
      dplyr::group_by(metaUI__study_id) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::rename(
        "studlab" = metaUI__study_id,
        "TE" = metaUI__effect_size,
        "seTE" = metaUI__se,
        "n" = metaUI__N
      )

    pcurve_estimates1 <- try(pcurve(pp_first, effect.estimation = FALSE, N = pppcurve_first$nhighlow, dmin = 0, dmax = 1), silent = TRUE)

    pcurve_estimates1 <- ifelse(substr(pcurve_estimates1, 1, 5) == "Error", 0, pcurve_estimates1)

    pcurve_estimates1

  })

  # ZCURVE ------------------------------------------------------------------

  # TK - add from current version of Shiny app

  # VIOLIN PLOTLY -------------------------------------------------------------
  output$violin <- plotly::renderPlotly({
    pp <- df_filtered()

    efm <- mean(pp$metaUI__effect_size)
    efsd <- sd(pp$metaUI__effect_size)

    violinplot <- ggplot2::ggplot(data = pp, ggplot2::aes(x = 1, y = metaUI__effect_size)) +
      ggplot2::xlab("") +
      ggplot2::geom_violin(fill = grDevices::rgb(100 / 255, 180 / 255, 1, .5)) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(name = metaUI_eff_size_type_label) +
      # geom_jitter(shape = 16, position = position_jitter(0)) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      ) +
      ggplot2::geom_boxplot(width = .25) +
      ggplot2::theme(text = ggplot2::element_text(size = 10))

    ay <- list(
      tickfont = list(size = 11.7),
      titlefont = list(size = 14.6),
      overlaying = "y",
      nticks = 5,
      side = "right",
      title = "Standardized effect size"
    )

    #TK - add ability to hover over and get outlier labels: https://stackoverflow.com/a/60078060/10581449

    plotly::ggplotly(violinplot, tooltip = "text") %>%
      plotly::config(modeBarButtons = list(list("toImage")), displaylogo = FALSE) %>%
      plotly::add_lines(
        x = ~1, y = ~ (metaUI__effect_size - efm) / efsd, colors = NULL, yaxis = "y2",
        data = pp, showlegend = FALSE, inherit = FALSE
      ) %>%
      plotly::layout(yaxis2 = ay)
  })




  # DOWNLOAD ----------------------------------------------------------------

  data_list <- reactive({
    filter_selections <- tibble::tibble()
    for (i in filters) {
      filter_selections <- cbind(filter_selections, tibble::tibble(i$id := input[i$id]))
    }

    list(
      dataset = df_filtered(),
      summary = as.data.frame(estimatesfiltered()),
      filters = filter_selections
    )
  })


  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), gsub(" ", "_", dataset_name), "-metaUIdata.xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(data_list())
    }
  )
}



