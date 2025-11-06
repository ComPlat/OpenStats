# nocov start ui-scaffold
corrSidebarUI <- function(id) {
  tabPanel(
    "Correlation",
    uiOutput(NS(id, "CorrelationUI"))
  )
}

corrUI <- function(id) {
  fluidRow()
}
# nocov end ui-scaffold
