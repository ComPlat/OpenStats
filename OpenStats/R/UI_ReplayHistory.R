HistorySidebarUI <- function(id) {
  ui <-tabPanel(
    "History",
    div(
      uiOutput(NS(id, "ReplayHistory")),
      class = "boxed-output"
    )
  )
}

# nocov start ui-scaffold
HistoryEditorUI <- function(id) {
  ui <- fluidPage(
    div(
      textAreaInput(NS(id, "history_string"), "History-JSON:", value = "", rows = 12),
      class = "boxed-output"
    )
  )
}
# nocov end ui-scaffold
