opendoe <- function(port = 3838L) {
  app <- app()
  shiny::shinyApp(app$ui, app$server, options = list(port = port))
}
