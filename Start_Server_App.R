Sys.setenv(RUN_MODE = "SERVER")
library(OpenStats)
app <- OpenStats:::app()
shiny::shinyApp(app$ui, app$server)
