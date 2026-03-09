# nocov start ui-scaffold
SplitByGroupUI <- function(id) {
  ui <- fluidPage(
    shiny::fluidRow(
      htmltools::div(
        shiny::actionButton(
          NS(id, "split_docu"),
          label = NULL,
          icon = shiny::icon("question-circle")
        ),
        uiOutput(NS(id, "colnames_dropdown")),
        class = "boxed-output"
      ),
      htmltools::div(
        uiOutput(NS(id, "levels_dropdown")),
        class = "boxed-output"
      ),
      shiny::actionButton(NS(id, "split_data"), "Split data")
    )
  )
}
# nocov end ui-scaffold
