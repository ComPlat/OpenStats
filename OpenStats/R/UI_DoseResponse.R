# nocov start ui-scaffold
DoseResponseSidebarUI <- function(id) {
  tabPanel(
    "Dose Response analysis",
    uiOutput(NS(id, "substanceNamesUI")),
    uiOutput(NS(id, "DoseResponseUI"))
  )
}

DoseResponseUI <- function(id) {
  fluidRow()
}
# nocov end ui-scaffold
