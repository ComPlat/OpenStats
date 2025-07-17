open_stats <- function(port = 3838) {
  app <- app()
  shinyApp(app$ui, app$server, options = list(port = port))
}
