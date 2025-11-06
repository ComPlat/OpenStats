assSidebarUI <- function(id) {
  tabPanel(
    "Assumption",
    tags$hr(),
    uiOutput(NS(id, "shapiroUI")),
    tags$hr(),
    uiOutput(NS(id, "shapiroResidualsUI")),
    tags$hr(),
    uiOutput(NS(id, "LeveneUI")),
    tags$hr(),
    uiOutput(NS(id, "DiagnosticPlotUI"))
  )
}

# nocov start ui-scaffold
assUI <- function(id) {
  fluidRow()
}
# nocov end ui-scaffold
