HistorySidebarUI <- function(id) {
  ui <-tabPanel(
    "History",
    div(
      actionButton(NS(id, "replay_history"), "Replay history", class = "add-button",
        title = "Copy the history (json format) into the text field and apply it to the current data set"),
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
