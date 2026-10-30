<!-- Filed 30 October 2026 as https://github.com/r-wasm/webr/issues/634 after independent verification. -->
This is an AI-assisted analysis, prepared with AI coding agents while exploring shinylive exports for the metaUI R package and independently verified by reproduction in native R and in webR. I’m sharing it in case it helps improve the project, and I’m happy to provide more detail.

# webR-patched Shiny: renderPlot stays blank after a silent startup error in reactive height

Environment tested: shinylive R package 0.5.0; shinylive assets 0.10.12; webR 0.6.0; bundled Shiny 1.13.0.8000; Chromium 156.0.8078.4 on Linux. Native comparison: R 4.6.1, Shiny 1.14.0.

Save this as `app.R`:

```r
library(shiny)
ui <- fluidPage(
  actionButton("go", "Analyze"),
  conditionalPanel("input.go > 0", plotOutput("comparison", height = "auto"))
)
server <- function(input, output, session) {
  estimates <- eventReactive(input$go, seq_len(input$go + 2L))
  output$comparison <- renderPlot({
    plot(estimates())
  }, height = function() 400 + 38 * length(estimates()))
}
shinyApp(ui, server)
```

Export the app directory with `shinylive::export("app", "site")`, serve the site over localhost, wait for startup, and click Analyze twice. Compare with `shiny::runApp("app")`.

Expected: the plot appears after the first click and updates after the second, as it does under native Shiny.

Actual in the export: no plot image appears after either click. The client retains an empty-message error with types `shiny.silent.error` and `validation`; its output value remains undefined. The initial silent error is expected before the event; failure to recover is the problem.

The evidence points to the webR-specific `hybrid_chain()` implementation:

- Replacing only native Shiny’s `hybrid_chain()` with the function extracted from the bundled Shiny reproduces the failure in `testServer()`. The extracted fixture matches the bundled function body; bundled `renderPlot()` matches native Shiny 1.14.0’s function body.
- [`renderPlot()` lines 124–149](https://github.com/rstudio/shiny/blob/v1.14.0/R/render-plot.R#L124-L149) initially evaluates `isolate(getDims())` with the default `execOnResize = FALSE`. On error, its catch callback reads `getDims()` without isolation to establish a dependency before propagating the error.
- [CRAN `hybrid_chain()`](https://github.com/rstudio/shiny/blob/v1.14.0/R/utils.R#L1576-L1615) uses an exiting `tryCatch()`. The [webR fork, lines 1574–1591](https://github.com/r-wasm/shiny/blob/ad8cadf9618b460f8362a8d1e1bb9e3f68789f25/R/utils.R#L1574-L1591), instead installs a calling error handler with `.Internal(.addCondHands(..., TRUE))` and forces a delayed `do_catch`.
- A native context probe at the recovery callback reports `plotObj` with CRAN’s helper, but `eventReactive({ input$go })` with the webR helper. Although the delayed expression is evaluated in the helper’s evaluation environment, the nested reactive contexts have not unwound. The recovery dimension read therefore does not establish the dependency on `plotObj` needed for these button changes to invalidate its cached startup error.

The calling-handler change was introduced to reduce stack use on older Safari ([commit](https://github.com/r-wasm/shiny/commit/765d7859632c0e3d6776b3efae066402dbbcc3ce)); this report concerns its recovery semantics.

Two workarounds passed the native reproduction with the webR helper:

- Set `execOnResize = TRUE` on `renderPlot()`.
- Guard the height function before calling `estimates()`, e.g. `if (!isTruthy(input$go)) return(400)`.

A direct `req(input$go)` inside the height function also reproduces the failure in the native helper-swap test. The real webR browser check above used the `eventReactive()` example.

A possible upstream fix is to preserve exiting-handler semantics, so the catch callback runs after nested reactive contexts unwind. I have not tested a general replacement that also preserves the patch’s stack-use goals.
