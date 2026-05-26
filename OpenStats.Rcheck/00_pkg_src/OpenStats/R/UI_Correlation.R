# nocov start ui-scaffold
corrSidebarUI <- function(id) {
  shiny::tabPanel(
    "Correlation",
    shiny::uiOutput(shiny::NS(id, "CorrelationUI"))
  )
}

corrUI <- function(id) {
  shiny::fluidRow()
}
# nocov end ui-scaffold
