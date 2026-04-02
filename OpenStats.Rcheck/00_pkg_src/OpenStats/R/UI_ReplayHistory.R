HistorySidebarUI <- function(id) {
  ui <-shiny::tabPanel(
    "History",
    htmltools::div(
      shiny::uiOutput(shiny::NS(id, "ReplayHistory")),
      class = "boxed-output"
    )
  )
}

# nocov start ui-scaffold
HistoryEditorUI <- function(id) {
  ui <- shiny::fluidPage(
    htmltools::div(
      shiny::textAreaInput(shiny::NS(id, "history_string"), "History-JSON:", value = "", rows = 12),
      class = "boxed-output"
    )
  )
}
# nocov end ui-scaffold
