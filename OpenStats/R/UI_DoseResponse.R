DoseResponseSidebarUI <- function(id) {
  shiny::tabPanel(
    "Dose Response analysis",
    shiny::uiOutput(shiny::NS(id, "substanceNamesUI")),
    shiny::uiOutput(shiny::NS(id, "unitNamesUI")),
    shiny::sliderInput(shiny::NS(id, "ic_percentage"), "Percentage if IC",
      min = 1, max = 99, value = 50
    ),
    shiny::uiOutput(shiny::NS(id, "DoseResponseUI"))
  )
}

# nocov start ui-scaffold
DoseResponseUI <- function(id) {
  shiny::fluidRow()
}
# nocov end ui-scaffold
