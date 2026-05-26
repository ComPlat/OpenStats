DoseResponseSidebarUI <- function(id) {
  shiny::tabPanel(
    "Dose Response analysis",
    htmltools::div(
      htmltools::h4("Dose Response analysis"),
      shiny::uiOutput(shiny::NS(id, "substanceNamesUI")),
      shiny::uiOutput(shiny::NS(id, "unitNamesUI")),
      shiny::uiOutput(shiny::NS(id, "Percentage")),
      shiny::uiOutput(shiny::NS(id, "typeSelector")),
      shiny::uiOutput(shiny::NS(id, "DoseResponseUI")),
      class = "boxed-output"
    ),
    htmltools::br(),
    htmltools::br(),
    htmltools::div(
      htmltools::h4("Primary Assay"),
      shiny::uiOutput(shiny::NS(id, "primaryAssayUI")),
      class = "boxed-output"
    )
  )
}

# nocov start ui-scaffold
DoseResponseUI <- function(id) {
  shiny::fluidRow()
}
# nocov end ui-scaffold
