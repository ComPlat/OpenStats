testsSidebarUI <- function(id) {
  shiny::tabPanel(
    "Tests",
    htmltools::br(),
    shiny::uiOutput(shiny::NS(id, "SidebarTestsUI")),
    shiny::uiOutput(shiny::NS(id, "padjUI"))
  )
}

# nocov start ui-scaffold
testsUI <- function(id) {
  shiny::fluidRow(
    shiny::uiOutput(shiny::NS(id, "tabs"))
  )
}
# nocov end ui-scaffold
