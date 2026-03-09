DoseResponseSidebarUI <- function(id) {
  shiny::tabPanel(
    "Dose Response analysis",
    shiny::uiOutput(shiny::NS(id, "substanceNamesUI")),
    shiny::uiOutput(shiny::NS(id, "DoseResponseUI"))
  )
}

# nocov start ui-scaffold
DoseResponseUI <- function(id) {
  shiny::fluidRow()
}
# nocov end ui-scaffold
