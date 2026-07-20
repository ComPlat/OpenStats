predictorsServer <- function(id, State) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(input$add_predictor, {
      name <- input$predictor_name
      levels <- parse_levels(input$predictor_levels)
      if (is.null(name) || name == "" || length(levels) == 0L) return(invisible())

      if (any_duplicates(levels)) {
        print_err("Found duplicated values for this predictor")
        return(invisible())
      }

      State$predictors[[name]] <- levels
      State$predictor_types <- vapply(State$predictors, infer_predictor_type, character(1))

      shiny::updateTextInput(session, "predictor_name", value = "")
      shiny::updateTextInput(session, "predictor_levels", value = "")
    })

    shiny::observeEvent(input$update_predictor, {
      name <- input$update_predictor_select
      levels <- parse_levels(input$update_predictor_levels)
      if (is.null(name) || name == "" || length(levels) == 0L) return(invisible())

      if (any_duplicates(levels)) {
        print_err("Found duplicated values for this predictor")
        return(invisible())
      }

      State$predictors[[name]] <- levels
      State$predictor_types <- vapply(State$predictors, infer_predictor_type, character(1))

      shiny::updateTextInput(session, "update_predictor_levels", value = "")
    })

    shiny::observeEvent(input$remove_predictor, {
      name <- input$remove_predictor_select
      if (is.null(name) || !(name %in% names(State$predictors))) return(invisible())

      predictors <- State$predictors
      predictors[[name]] <- NULL
      State$predictors <- predictors
      State$predictor_types <- vapply(State$predictors, infer_predictor_type, character(1))
    })

    shiny::observe({
      choices <- names(State$predictors)
      shiny::updateSelectInput(session, "update_predictor_select", choices = choices)
      shiny::updateSelectInput(session, "remove_predictor_select", choices = choices)
    })

    output$predictors_box <- shiny::renderUI({
      if (length(State$predictors) == 0L) return(NULL)
      shiny::tags$div(
        class = "doe-box",
        DT::DTOutput("PREDICTORS-predictors_table")
      )
    })

    output$predictors_table <- DT::renderDT({
      shiny::req(length(State$predictors) > 0)
      df <- build_predictor_df(State$predictors, State$predictor_types)
      DT::datatable(df, options = list(dom = "t", paging = FALSE), rownames = FALSE)
    })

    shiny::observeEvent(input$add_to_results, {
      if (length(State$predictors) == 0L) {
        print_err("Define at least one predictor first")
        return(invisible())
      }
      params <- list(predictors = State$predictors, predictor_types = State$predictor_types)
      df <- build_predictor_df(params$predictors, params$predictor_types)
      add_result(State, "predictor_table", "Predictor table", params, methods::new("predictorTable", df = df))
    })

  })
}
