#' Preview and export an app design
#'
#' A local Shiny helper with colour, font and layout controls. The preview uses
#' the same stylesheet and design settings as generated apps, with illustrative
#' results. Copy or download the R settings and pass them to `generate_shiny()`.
#' The JSON download is the object used by `build_app()`'s `design` field.
#' @param design Initial settings from [app_design()] or a named list.
#' @return A Shiny app object. Printing it launches the helper; assigning it does not.
#' @export
#' @examples
#' if (interactive()) design_app()
design_app <- function(design = app_design()) {
  initial <- validate_design(design)
  shiny::addResourcePath("metaui-design", system.file("template_code", package = "metaUI"))
  picker <- function(id, label) shinyWidgets::colorPickr(id, label,
    selected = initial[[id]], opacity = FALSE, update = "change", theme = "monolith",
    interaction = list(hex = TRUE, input = TRUE, save = FALSE, clear = FALSE),
    i18n = list(`btn:toggle` = paste(label, "colour picker")))
  ui <- shiny::fluidPage(theme = shinythemes::shinytheme("yeti"),
    shiny::tags$head(shiny::tags$title("App design - metaUI"),
      shiny::tags$style(shiny::HTML(designer_css()))),
    shiny::tags$header(class = "designer-header", shiny::tags$a(href = "#preview", class = "designer-skip", "Skip to preview"),
      shiny::tags$span("metaUI"), shiny::h1("App design")),
    shiny::div(class = "designer-layout",
      shiny::tags$aside(class = "designer-controls", `aria-label` = "Design settings",
        shiny::selectInput("palette", "Palette", c("Teal" = "teal", "Blue" = "blue", "Plum" = "plum", "Custom" = "custom"), selected = design_palette_name(initial), selectize = FALSE),
        shiny::div(class = "designer-colours", picker("accent", "Accent"), picker("text", "Text"),
          picker("background", "Page"), picker("surface", "Panels")),
        shiny::selectInput("font", "Interface font", c("System sans" = "system", "Arial" = "arial", "Verdana" = "verdana", "Georgia" = "georgia"), initial$font, selectize = FALSE),
        shiny::p(class = "designer-note", "Fonts come from each reader's device; named fonts may be substituted."),
        shiny::sliderInput("text_size", "Text size (px)", 14, 20, value = initial$text_size, step = 1),
        shiny::radioButtons("spacing", "Spacing", c("Comfortable" = "comfortable", "Compact" = "compact"), initial$spacing, inline = TRUE),
        shiny::sliderInput("radius", "Panel corners (px)", 0, 16, value = initial$radius, step = 1),
        shiny::actionButton("reset_design", "Reset choices"),
        shiny::div(class = "designer-feedback", role = "status", `aria-live` = "polite", shiny::textOutput("contrast"))),
      shiny::tags$main(class = "designer-main",
        shiny::div(class = "designer-preview-bar", shiny::h2("Preview"),
          shiny::radioButtons("viewport", NULL, c("Desktop" = "desktop", "Phone" = "phone"), inline = TRUE)),
        shiny::p(class = "designer-note", "Illustrative results."),
        shiny::div(id = "preview", class = "designer-preview", shiny::uiOutput("preview")),
        shiny::div(class = "designer-export", shiny::h2("Use this design"),
          shiny::p("Pass ", shiny::tags$code("design = design"), " to ", shiny::tags$code("generate_shiny()"), "."),
          shiny::tabsetPanel(shiny::tabPanel("R", shiny::verbatimTextOutput("r_code")),
            shiny::tabPanel("JSON", shiny::verbatimTextOutput("json_code"))),
          shiny::uiOutput("export_downloads")))))
  server <- function(input, output, session) {
    fallback <- function(x, key) if (is.null(x)) initial[[key]] else x
    current_design <- shiny::reactive({
      tryCatch(app_design(accent = substr(fallback(input$accent, "accent"), 1, 7),
        background = substr(fallback(input$background, "background"), 1, 7),
        surface = substr(fallback(input$surface, "surface"), 1, 7),
        text = substr(fallback(input$text, "text"), 1, 7),
        font = fallback(input$font, "font"), text_size = fallback(input$text_size, "text_size"),
        spacing = fallback(input$spacing, "spacing"), radius = fallback(input$radius, "radius")), error = identity)
    })
    valid_design <- shiny::reactive({
      d <- current_design()
      shiny::validate(shiny::need(!inherits(d, "error"), if (inherits(d, "error")) conditionMessage(d) else ""))
      d
    })
    # Keep the preview on the last valid design while invalid inputs are corrected.
    last_valid <- shiny::reactiveVal(initial)
    shiny::observe({ d <- current_design(); if (!inherits(d, "error")) last_valid(d) })
    set_controls <- function(d) {
      for (key in c("accent", "background", "surface", "text")) shinyWidgets::updateColorPickr(session, key, value = d[[key]])
      shiny::updateSelectInput(session, "font", selected = d$font)
      shiny::updateSliderInput(session, "text_size", value = d$text_size)
      shiny::updateRadioButtons(session, "spacing", selected = d$spacing)
      shiny::updateSliderInput(session, "radius", value = d$radius)
    }
    shiny::observeEvent(input$palette, {
      palettes <- design_palettes()
      if (input$palette %in% names(palettes)) {
        d <- palettes[[input$palette]]
        for (key in c("accent", "background", "surface", "text"))
          shinyWidgets::updateColorPickr(session, key, value = d[[key]])
      }
    }, ignoreInit = TRUE)
    shiny::observeEvent(list(input$accent, input$background, input$surface, input$text), {
      d <- current_design()
      target <- if (inherits(d, "error")) "custom" else design_palette_name(d)
      if (!identical(input$palette, target)) shiny::updateSelectInput(session, "palette", selected = target)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$reset_design, {
      set_controls(initial); shiny::updateSelectInput(session, "palette", selected = design_palette_name(initial))
    })
    output$contrast <- shiny::renderText({
      d <- current_design()
      if (inherits(d, "error")) return(paste(conditionMessage(d), "Preview keeps the last valid design; export is unavailable."))
      paste0("Minimum text/accent contrast: ", format(round(min(design_contrasts(d)), 1), nsmall = 1), ":1 (required: 4.5:1).")
    })
    output$preview <- shiny::renderUI({
      phone <- identical(input$viewport, "phone")
      shiny::tags$iframe(title = "App design preview", srcdoc = design_preview(last_valid()),
        style = paste0("width:", if (phone) "390px" else "100%", ";height:720px;max-width:100%;border:0;display:block;margin:auto;background:white;"))
    })
    output$r_code <- shiny::renderText(design_r_code(valid_design()))
    output$json_code <- shiny::renderText(jsonlite::toJSON(valid_design(), auto_unbox = TRUE, pretty = TRUE))
    output$export_downloads <- shiny::renderUI({
      if (inherits(current_design(), "error")) return(NULL)
      shiny::tagList(shiny::downloadButton("download_r", "Download R"), shiny::downloadButton("download_json", "Download JSON"))
    })
    output$download_r <- shiny::downloadHandler(filename = function() "design.R",
      content = function(file) writeLines(design_r_code(valid_design()), file), contentType = "text/plain")
    output$download_json <- shiny::downloadHandler(filename = function() "design.json",
      content = function(file) jsonlite::write_json(valid_design(), file, auto_unbox = TRUE, pretty = TRUE), contentType = "application/json")
  }
  shiny::shinyApp(ui, server)
}

designer_css <- function() paste0('
body{background:#edf3f7;color:#182f46;font-family:"Segoe UI",Arial,sans-serif;font-size:15px;}
.container-fluid{max-width:1600px;padding:0 28px 32px;}
.designer-header{display:flex;align-items:baseline;gap:18px;padding:20px 0;border-bottom:1px solid #cedae3;margin-bottom:24px;}
.designer-header>span{font-size:17px;font-weight:700;color:#096b74;}.designer-header h1{font-size:28px;font-weight:600;margin:0;}
.designer-layout{display:grid;grid-template-columns:290px minmax(0,1fr);gap:28px;align-items:start;}
.designer-controls{background:#fff;border:1px solid #d9e3eb;border-radius:8px;padding:20px;}
.designer-controls .shiny-input-container{width:100%;}.designer-controls .control-label{font-size:14px;font-weight:600;}
.designer-colours{display:grid;grid-template-columns:1fr 1fr;gap:0 14px;}.designer-colours .pickr{margin-bottom:10px;}
.designer-main{min-width:0;}.designer-preview-bar{display:flex;justify-content:space-between;align-items:center;gap:20px;}
.designer-preview-bar .shiny-input-container{width:auto;margin:0;}.designer-preview-bar h2,.designer-export h2{font-size:21px;font-weight:600;margin:0 0 10px;}
.designer-note{font-size:13px;color:#506477;}.designer-preview{border:1px solid #c6d5df;border-radius:8px;overflow:hidden;background:#dce6ed;}
.designer-export{background:white;border:1px solid #d9e3eb;border-radius:8px;padding:20px;margin-top:24px;}
.designer-export pre{max-height:340px;overflow:auto;font-size:13px;}.designer-export .btn{margin:8px 8px 0 0;}
.designer-feedback{font-size:13px;line-height:1.5;margin-top:16px;}.designer-controls .btn{background:white;color:#096b74;border:1px solid #bdd7da;border-radius:5px;}
.designer-skip{position:absolute;left:-9999px;}.designer-skip:focus{position:static;}
.btn:focus-visible,a:focus-visible,input:focus-visible,select:focus-visible{outline:3px solid #096b74;outline-offset:2px;}
@media(max-width:850px){.designer-layout{grid-template-columns:1fr;}.container-fluid{padding:0 16px 24px;}.designer-controls{display:block;}.designer-preview-bar{flex-wrap:wrap;}}
')

design_preview <- function(d) {
  # A fixed illustrative selection, using the generated UI's classes and CSS.
  html <- shiny::tagList(shiny::div(class = "container-fluid",
    shiny::div(class = "metaui-header", shiny::div(class = "metaui-brand", "metaUI"), shiny::h2("Example meta-analysis")),
    shiny::div(class = "row",
      shiny::div(class = "col-sm-3", shiny::tags$form(class = "well", role = "complementary",
        shiny::h2(class = "metaui-panel-title", "Select effects"),
        shiny::tags$label(class = "control-label", "Population"),
        shiny::div(class = "checkbox", shiny::tags$label(shiny::tags$input(type = "checkbox", checked = NA), "Clinical")),
        shiny::div(class = "checkbox", shiny::tags$label(shiny::tags$input(type = "checkbox", checked = NA), "Community")),
        shiny::div(class = "metaui-actions", shiny::tags$button(type = "button", id = "go", class = "btn btn-primary", "Analyze data"),
          shiny::tags$button(type = "reset", id = "resetFilters", class = "btn btn-default", "Reset filters")),
        shiny::tags$details(class = "metaui-help metaui-data-tools", shiny::tags$summary("Download data or code, upload data"), shiny::p("The workbook includes all rows, filters and results.")))),
      shiny::div(class = "col-sm-9",
        shiny::div(class = "metaui-basis", shiny::div(id = "selection_overview", "16 of 16 effects selected")),
        shiny::tags$ul(class = "nav nav-tabs", shiny::tags$li(class = "active", shiny::tags$a(href = "#", "Summary")), shiny::tags$li(shiny::tags$a(href = "#", "Sample")), shiny::tags$li(shiny::tags$a(href = "#", "Forest Plot"))),
        shiny::div(class = "tab-content", shiny::h3("Effect size estimates"),
          shiny::div(class = "metaui-primary", shiny::p(class = "metaui-primary-label", "Primary analysis, declared by the authors"),
            shiny::p(class = "metaui-primary-name", "Random-effects multilevel model"),
            shiny::p(class = "metaui-primary-value", "0.24 ", shiny::tags$span(class = "metaui-primary-ci", "95% CI [0.11, 0.38]")),
            shiny::p(class = "metaui-note", "SMD. k = 16; effect-level fit.")),
          shiny::HTML(paste0('<svg role="img" aria-label="Illustrative model intervals" viewBox="0 0 640 135" style="width:100%;height:auto"><line x1="330" x2="330" y1="10" y2="115" stroke="#9aa9b5" stroke-dasharray="4 4"/><text x="0" y="38" fill="',d$text,'" font-size="14">Multilevel model</text><line x1="365" x2="490" y1="34" y2="34" stroke="',d$accent,'" stroke-width="2"/><circle cx="430" cy="34" r="5" fill="',d$accent,'"/><text x="0" y="88" fill="',d$text,'" font-size="14">Robust variance estimation</text><line x1="340" x2="520" y1="84" y2="84" stroke="',d$text,'"/><circle cx="425" cy="84" r="4" fill="',d$text,'"/></svg>')),
          shiny::tags$table(class = "table table-striped", shiny::tags$thead(shiny::tags$tr(lapply(c("Model", "Estimate", "95% interval", "k"), shiny::tags$th))),
            shiny::tags$tbody(shiny::tags$tr(shiny::tags$td("Multilevel model"), shiny::tags$td("0.24"), shiny::tags$td("[0.11, 0.38]"), shiny::tags$td("16")),
              shiny::tags$tr(shiny::tags$td("Robust variance estimation"), shiny::tags$td("0.23"), shiny::tags$td("[0.03, 0.43]"), shiny::tags$td("8")))))))))
  paste0('<!doctype html><html lang="en"><head><meta name="viewport" content="width=device-width,initial-scale=1"><link rel="stylesheet" href="shinythemes/css/yeti.min.css"><link rel="stylesheet" href="metaui-design/metaui.css"><style>', design_css(d), '</style></head><body>', as.character(html), '</body></html>')
}
