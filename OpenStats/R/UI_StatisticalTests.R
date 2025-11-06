testsSidebarUI <- function(id) {
  tabPanel(
    "Tests",
    br(),
    uiOutput(NS(id, "SidebarTestsUI")),
    uiOutput(NS(id, "padjUI"))
  )
}

# nocov start ui-scaffold
testsUI <- function(id) {
  fluidRow(
    uiOutput(NS(id, "tabs"))
  )
}
# nocov end ui-scaffold
