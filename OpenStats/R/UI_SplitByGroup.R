# nocov start ui-scaffold
SplitByGroupUI <- function(id) {
  ui <- fluidPage(
    fluidRow(
      div(
        actionButton(
          NS(id, "split_docu"),
          label = NULL,
          icon = icon("question-circle")
        ),
        uiOutput(NS(id, "colnames_dropdown")),
        class = "boxed-output"
      ),
      div(
        uiOutput(NS(id, "levels_dropdown")),
        class = "boxed-output"
      ),
      actionButton(NS(id, "split_data"), "Split data")
    )
  )
}
# nocov end ui-scaffold
