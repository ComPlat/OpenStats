historyUI <- function(id) {
  shiny::tagList(
    shiny::uiOutput("HISTORY-header"),
    shiny::tags$div(id = "HISTORY-history-container")
  )
}
