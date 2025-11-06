# nocov start ui-scaffold
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

assUI <- function(id) {
  fluidRow()
}
# nocov end ui-scaffold
