library(shiny)
check_args <- function(args) {
  if (length(args) != 1) {
    stop("Only one argument is expected")
  }
  args <- as.integer(args)
  if (is.na(args)) {
    stop("Argument is not numeric")
  }
  args
}
args <- commandArgs(trailingOnly = TRUE)
args <- check_args(args)
run_app <- function(port) {
  app <- shinyApp(
    ui = bootstrapPage(
      shiny::numericInput('n', 'Number of obs', 100),
      plotOutput('plot')
    ),
    server = function(input, output) {
      output$plot <- renderPlot({ hist(runif(input$n)) })
    }
  )
  runApp(app, port = port)
}
run_app(args)
