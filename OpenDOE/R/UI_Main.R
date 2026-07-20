main_ui <- function() {
  shiny::fluidPage(
    shiny::tags$head(
      shiny::includeCSS(system.file("www/styles.css", package = "OpenDOE"))
    ),
    shiny::titlePanel("OpenDOE"),
    shiny::downloadButton("HISTORY-save_history", "Save results & history"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::conditionalPanel(
          condition = "input.conditionedPanels == 'Predictors'",
          predictorsSidebarUI("PREDICTORS")
        ),
        shiny::conditionalPanel(
          condition = "input.conditionedPanels == 'Sample size'",
          sampleSizeSidebarUI("SAMPLESIZE")
        )
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Predictors",
            predictorsMainUI("PREDICTORS")
          ),
          shiny::tabPanel(
            "Sample size",
            sampleSizeMainUI("SAMPLESIZE")
          ),
          shiny::tabPanel(
            "History",
            historyUI("HISTORY")
          ),
          id = "conditionedPanels"
        ),
        resultsListUI("RESULTS")
      )
    )
  )
}
