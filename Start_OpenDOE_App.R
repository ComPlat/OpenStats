library(OpenDOE)
app <- OpenDOE:::app()
shiny::shinyApp(app$ui, app$server)
