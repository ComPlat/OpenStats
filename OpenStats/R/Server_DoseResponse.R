DoseResponseServer <- function(id, DataModelState, ResultsState) {
  moduleServer(id, function(input, output, session) {

    # Render sidebar
    output[["DoseResponseUI"]] <- shiny::renderUI({
      shiny::invalidateLater(250)
      status <- ResultsState$bgp$running_status
      if (status != "Idle") return(htmltools::div())

      message <- check_dose_response(DataModelState)
      if (!is.null(message)) {
        return(
          info_div(message)
        )
      }
      htmltools::div(
        style = "position: relative;",
        htmltools::br(),
        checkboxInput(
          "DOSERESPONSE-xTransform",
          label = "Log transform x-axis",
          value = FALSE
        ),
        checkboxInput(
          "DOSERESPONSE-yTransform",
          label = "Log transform y-axis",
          value = FALSE
        ),
        shiny::actionButton("DOSERESPONSE-ic50", "Conduct analysis")
      )
    })
    # Render names of substances
    output[["substanceNamesUI"]] <- shiny::renderUI({
      shiny::req(!is.null(DataModelState$df))
      shiny::req(is.data.frame(DataModelState$df))
      shiny::req(inherits(DataModelState$formula, "LinearFormula"))
      colnames <- names(DataModelState$df)
      tooltip <- "Select the column which contains the names of the different substances"
      htmltools::div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("DOSERESPONSE-substanceNames"),
          label = "Column containing the names",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })

    check_dr <- function() {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      shiny::req(input$substanceNames)
      print_form(DataModelState$formula)
      shiny::req(!is.null(DataModelState$formula))
    }

    run_dr <- function(df, new_name) {
      dr <- get_dose_response()$new(
        df, input$xTransform, input$yTransform,
        input$substanceNames, DataModelState$formula
      )
      dr$eval(ResultsState, new_name)
    }

    shiny::observeEvent(input$ic50, {
      check_dr()
      df <- DataModelState$df
      new_name <- paste0(ResultsState$counter + 1, " DoseResponse")
      e <- try(run_dr(df, new_name))
    })

  })

}
