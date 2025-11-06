HistoryEditorServer <- function(id, DataModelState, ResultsState, DataWranglingState) {
  moduleServer(id, function(input, output, session) {

    # nocov start ui-scaffold
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
    # nocov end ui-scaffold

    observeEvent(input$confirm_replay, {
      removeModal()
      rh <- get_replay_history()$new(input$history_string, DataModelState$df, ResultsState$all_data)
      rh$validate()
      rh$eval(ResultsState)
    })

  })
}

