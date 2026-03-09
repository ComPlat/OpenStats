HistoryEditorServer <- function(id, DataModelState, ResultsState, DataWranglingState) {
  moduleServer(id, function(input, output, session) {

    output[["ReplayHistory"]] <- shiny::renderUI({
      shiny::invalidateLater(250)
      status <- ResultsState$bgp$running_status
      if (status != "Idle") return(htmltools::div())

      shiny::actionButton("HISTORY-replay_history", "Replay history", class = "add-button",
        title = "Copy the history (json format) into the text field and apply it to the current data set")
    })

    # nocov start ui-scaffold
    shiny::observeEvent(input$replay_history, {
      print_req(is.data.frame(DataWranglingState$df), "The dataset is missing")
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      print_req(nchar(input$history_string) > 2, "The input history is too short")

      shiny::showModal(modalDialog(
        title = "Confirm History Replay",
        "Do you want to run the history? This can take a while",
        easyClose = FALSE,
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(NS(id, "confirm_replay"), "Yes, overwrite everything", class = "btn-danger")
        )
      ))
    })
    # nocov end ui-scaffold

    shiny::observeEvent(input$confirm_replay, {
      removeModal()
      rh <- get_replay_history()$new(input$history_string, DataModelState$df, ResultsState$all_data)
      rh$validate()
      rh$eval(ResultsState)
    })

  })
}

