testsSidebarUI <- function(id) {
  tabPanel(
    "Tests",
    htmltools::br(),
    uiOutput(NS(id, "SidebarTestsUI")),
    uiOutput(NS(id, "padjUI"))
  )
}

# nocov start ui-scaffold
testsUI <- function(id) {
  shiny::fluidRow(
    uiOutput(NS(id, "tabs"))
  )
}
# nocov end ui-scaffold
