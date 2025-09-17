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

HistoryEditorUI <- function(id) {
  ui <- fluidPage(
    div(
      textAreaInput(NS(id, "history_string"), "History-JSON:", value = "", rows = 12),
      class = "boxed-output"
    )
  )
}



HistoryEditorServer <- function(id, DataModelState, ResultsState, DataWranglingState) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$replay_history, {
      print_req(is.data.frame(DataWranglingState$df), "The dataset is missing")
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      print_req(nchar(input$history_string) > 2, "The input history is too short")

      showModal(modalDialog(
        title = "Confirm History Replay",
        "Do you want to run the history? This can take a while",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(NS(id, "confirm_replay"), "Yes, overwrite everything", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_replay, {
      removeModal()
      rh <- replay_history_V1_2$new(input$history_string, DataModelState$df, ResultsState$all_data)
      rh$validate()
      rh$eval(ResultsState)
    })

  })
}

