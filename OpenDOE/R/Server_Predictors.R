predictorsServer <- function(id, State) {
  shiny::moduleServer(id, function(input, output, session) {

    set_predictor <- function(name, levels, type) {
      msg <- validate_predictor(levels, type)
      if (!is.null(msg)) {
        print_err(msg)
        return(FALSE)
      }
      levels <- expand_predictor_levels(levels, type)
      if (any_duplicates(levels)) {
        print_err("Found duplicated values for this predictor")
        return(FALSE)
      }
      types <- State$predictor_types
      types[[name]] <- type
      State$predictors[[name]] <- levels
      State$predictor_types <- types
      TRUE
    }

    # Inferred type is only a preselection: once the user picks a type, it must not be overwritten
    type_state <- new.env(parent = emptyenv())
    type_state$inferred <- NULL
    type_state$touched <- FALSE

    shiny::observeEvent(input$predictor_type, {
      if (!identical(input$predictor_type, type_state$inferred)) type_state$touched <- TRUE
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$predictor_levels, {
      if (type_state$touched) return(invisible())
      levels <- parse_levels(input$predictor_levels)
      if (length(levels) == 0L) return(invisible())
      type_state$inferred <- infer_predictor_type(levels)
      shiny::updateSelectInput(session, "predictor_type", selected = type_state$inferred)
    })

    shiny::observeEvent(input$update_predictor_select, {
      name <- input$update_predictor_select
      if (is.null(name) || !(name %in% names(State$predictor_types))) return(invisible())
      shiny::updateSelectInput(session, "update_predictor_type", selected = State$predictor_types[[name]])
    })

    shiny::observeEvent(input$add_predictor, {
      name <- input$predictor_name
      levels <- parse_levels(input$predictor_levels)
      if (is.null(name) || name == "" || length(levels) == 0L) return(invisible())

      type <- resolve_predictor_type(input$predictor_type, levels)
      if (!set_predictor(name, levels, type)) return(invisible())

      type_state$touched <- FALSE
      type_state$inferred <- NULL
      shiny::updateTextInput(session, "predictor_name", value = "")
      shiny::updateTextInput(session, "predictor_levels", value = "")
    })

    shiny::observeEvent(input$update_predictor, {
      name <- input$update_predictor_select
      levels <- parse_levels(input$update_predictor_levels)
      if (is.null(name) || name == "" || length(levels) == 0L) return(invisible())

      type <- resolve_predictor_type(input$update_predictor_type, levels)
      if (!set_predictor(name, levels, type)) return(invisible())

      shiny::updateTextInput(session, "update_predictor_levels", value = "")
    })

    shiny::observeEvent(input$remove_predictor, {
      name <- input$remove_predictor_select
      if (is.null(name) || !(name %in% names(State$predictors))) return(invisible())

      predictors <- State$predictors
      predictors[[name]] <- NULL
      State$predictors <- predictors
      types <- State$predictor_types
      State$predictor_types <- types[names(types) != name]
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

  })
}
