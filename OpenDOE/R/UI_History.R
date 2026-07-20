historyUI <- function(id) {
  shiny::tagList(
    shiny::tags$div(
      class = "doe-box",
      shiny::h4("Reproduce from History JSON"),
      shiny::tags$p("Paste the \"History (JSON)\" block from a saved Excel file to recompute its results."),
      shiny::textAreaInput("HISTORY-history_json", NULL, rows = 6, width = "100%"),
      shiny::actionButton("HISTORY-replay_json", "Replay")
    ),
    shiny::uiOutput("HISTORY-replay_status")
  )
}
