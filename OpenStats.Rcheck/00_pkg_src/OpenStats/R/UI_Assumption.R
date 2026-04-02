assSidebarUI <- function(id) {
  shiny::tabPanel(
    "Assumption",
    shiny::tags$hr(),
    shiny::uiOutput(shiny::NS(id, "shapiroUI")),
    shiny::uiOutput(shiny::NS(id, "shapiroResidualsUI")),
    shiny::uiOutput(shiny::NS(id, "LeveneUI")),
    shiny::uiOutput(shiny::NS(id, "DiagnosticPlotUI"))
  )
}

# nocov start ui-scaffold
assUI <- function(id) {
  shiny::fluidRow()
}
# nocov end ui-scaffold
