resultsListUI <- function(id) {
  shiny::tagList(
    shiny::uiOutput("RESULTS-results_header"),
    shiny::tags$div(id = "RESULTS-results-container")
  )
}
