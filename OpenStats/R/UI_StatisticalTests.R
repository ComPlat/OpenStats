# nocov start ui-scaffold
testsSidebarUI <- function(id) {
  shiny::tabPanel(
    "Tests",
    shiny::uiOutput(shiny::NS(id, "parametricUI")),
    htmltools::br(),
    shiny::uiOutput(shiny::NS(id, "SidebarTestsLinearParametricUI")),
    shiny::uiOutput(shiny::NS(id, "SidebarTestsLinearNonParametricUI")),
    shiny::uiOutput(shiny::NS(id, "SidebarTestsGeneralizedLinearUI"))
  )
}

testsUI <- function(id) {
  shiny::tagList(
    htmltools::br(),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::uiOutput(shiny::NS(id, "tabs"))
      )
    )
  )
}
# nocov end ui-scaffold
