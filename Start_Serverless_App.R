# TODO: add version LOCAL and update Readme afterwards
Sys.setenv(RUN_MODE = "LOCAL")
app <- OpenStats:::app()
shiny::shinyApp(app$ui, app$server)
