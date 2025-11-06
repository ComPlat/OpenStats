DoseResponseSidebarUI <- function(id) {
  tabPanel(
    "Dose Response analysis",
    uiOutput(NS(id, "substanceNamesUI")),
    uiOutput(NS(id, "DoseResponseUI"))
  )
}

# nocov start ui-scaffold
DoseResponseUI <- function(id) {
  fluidRow()
}
# nocov end ui-scaffold
