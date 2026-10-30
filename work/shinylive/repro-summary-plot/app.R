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
