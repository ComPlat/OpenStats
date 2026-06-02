OpenSplitByGroupServer <- function(id, DataModelState, ResultsState, MethodState, conditional_panels) {
  shiny::moduleServer(id, function(input, output, session) {
    output$open_split_by_groupUI <- shiny::renderUI({
      if (conditional_panels() == "DataWrangling") {
        return()
      }
      if (is.null(DataModelState$backup_df)) {
        return(
          htmltools::div(
            class = "boxed-output",
            shiny::actionButton("OPENSPLITBYGROUP-open_split_by_group",
              "Open the split by group functionality",
              title = "Open the split by group helper window"
            ))
        )
      } else {
        return(
          shiny::actionButton("OPENSPLITBYGROUP-remove_filter",
            "Remove the filter from the dataset",
            title = "remove the filter of the dataset",
            disabled = is.null(DataModelState$backup_df) || !is.data.frame(DataModelState$backup_df)
          )
        )
      }
    })
    shiny::observeEvent(input[["open_split_by_group"]], {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      shiny::showModal(shiny::modalDialog(
        title = "SplitByGroup",
        SplitByGroupUI("SG"),
        easyClose = TRUE,
        size = "l",
        footer = NULL
      ))
    })
    shiny::observe({
      output$applied_filter <- shiny::renderText(NULL)
      shiny::req(!is.null(DataModelState$filter_col))
      shiny::req(!is.null(DataModelState$filter_group))
      output$applied_filter <- shiny::renderText({
        paste(
          "The dataset is splitted by the variable(s): [",
          paste(DataModelState$filter_col, collapse = ", "),
          "] group(s) are set to: [",
          paste(DataModelState$filter_group, collapse = ", "),
          "]"
        )
      })
    })
    # Remove filter
    shiny::observeEvent(input[["remove_filter"]], {
      e <- try({
        rf <- get_remove_filter()$new()
        rf$validate()
        rf$eval(ResultsState, DataModelState)
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        print_err(err)
      }
    })
  })
}
