# nocov start ui-scaffold
SplitByGroupUI <- function(id) {
  ui <- shiny::fluidPage(
    shiny::fluidRow(
      htmltools::div(
        shiny::actionButton(
          shiny::NS(id, "split_docu"),
          label = NULL,
          icon = shiny::icon("question-circle")
        ),
        shiny::uiOutput(shiny::NS(id, "colnames_dropdown")),
        class = "boxed-output"
      ),
      htmltools::div(
        shiny::uiOutput(shiny::NS(id, "levels_dropdown")),
        class = "boxed-output"
      ),
      shiny::actionButton(shiny::NS(id, "split_data"), "Split data")
    )
  )
}
# nocov end ui-scaffold
