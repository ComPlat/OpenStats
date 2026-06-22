OperatorSidebarUI <- function(id) {
  shiny::tabPanel(
    "DataWrangling",
    htmltools::br(),
    htmltools::div(
      class = "boxed-output",
      shiny::uiOutput(shiny::NS(id, "column_apply"))
    ),
    htmltools::br(),
    htmltools::div(
      class = "boxed-output",
      shiny::fluidRow(
        shiny::column(
          7,
          shiny::actionButton(shiny::NS(id, "run_op_intermediate"), "Run operation and store intermediate results"),
          shiny::textInput(shiny::NS(id, "iv"), "Intermediate variable name:", value = "")
        )
      )
    ),
    htmltools::div(
      class = "boxed-output",
      shiny::fluidRow(
        shiny::column(
          7,
          shiny::actionButton(shiny::NS(id, "run_op"), "Run operation and append to dataset"),
          shiny::textInput(shiny::NS(id, "nc"), "New column name:", value = "")
        )
      )
    )
  )
}

OperatorEditorUI <- function(id) {
  ui <- shiny::fluidPage(
    htmltools::div(
      shiny::uiOutput(shiny::NS(id, "builder")),
      class = "boxed-output"
    ),
    shiny::uiOutput(shiny::NS(id, "intermediate_results"))
  )
}
