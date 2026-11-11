BO_PREDICTOR_TYPES <- c("continuous", "discrete", "categorical")

boServer <- function(id, State) {
  shiny::moduleServer(id, function(input, output, session) {

    selected_df <- shiny::reactive({
      choices <- df_result_choices(State)
      df_source <- input$df_source
      if (is.null(df_source) || !(df_source %in% choices)) return(NULL)
      State$results[[df_source]]@df
    })

    output$bo_controls <- shiny::renderUI({
      choices <- df_result_choices(State)
      if (length(choices) == 0L) {
        return(shiny::tags$p("Import measured experiments in the \"Data\" tab first."))
      }
      df_source <- input$df_source
      if (is.null(df_source) || !(df_source %in% choices)) df_source <- choices[[1]]
      cols <- names(State$results[[df_source]]@df)

      shiny::tags$div(
        class = "doe-box",
        shiny::h4("Bayesian optimization"),
        shiny::selectInput("BO-df_source", "Measured experiments", choices = choices, selected = df_source),
        shiny::selectInput(
          "BO-predictor_cols", "Predictor columns", choices = cols,
          selected = intersect(shiny::isolate(input$predictor_cols), cols), multiple = TRUE
        )
      )
    })

    output$type_controls <- shiny::renderUI({
      df <- selected_df()
      cols <- intersect(input$predictor_cols, names(df))
      if (is.null(df) || length(cols) == 0L) return(NULL)
      shiny::tags$div(
        class = "doe-box",
        shiny::h4("Predictor types"),
        lapply(seq_along(cols), function(i) {
          shiny::selectInput(
            paste0("BO-type_", i), cols[[i]],
            choices = BO_PREDICTOR_TYPES, selected = infer_bo_predictor_type(df[[cols[[i]]]])
          )
        })
      )
    })

    output$run_controls <- shiny::renderUI({
      df <- selected_df()
      if (is.null(df)) return(NULL)
      objective_choices <- setdiff(names(df), input$predictor_cols)
      shiny::tags$div(
        class = "doe-box",
        shiny::selectInput("BO-objective", "Objective column", choices = objective_choices),
        shiny::selectInput(
          "BO-direction", "Direction",
          choices = c("Maximize" = "maximize", "Minimize" = "minimize")
        ),
        shiny::numericInput("BO-seed", "Seed", value = 42, step = 1),
        shiny::actionButton("BO-suggest", "Suggest next experiment")
      )
    })

    output$status_box <- shiny::renderUI({
      if (isTRUE(State$bo_running)) {
        shiny::tags$div(
          class = "doe-box",
          shiny::tags$p("Searching next experiment..."),
          shiny::actionButton("BO-cancel", "Cancel", class = "btn-danger")
        )
      }
    })

    shiny::observeEvent(input$cancel, {
      State$bgp$cancel()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$suggest, {
      df_source <- input$df_source
      if (is.null(df_source) || is.null(State$results[[df_source]])) {
        print_err("Select a dataset first")
        return(invisible())
      }
      df <- State$results[[df_source]]@df
      if (ncol(df) < 2L) {
        print_err("Dataset needs at least two columns: predictor(s) and the measured result")
        return(invisible())
      }

      cols <- intersect(input$predictor_cols, names(df))
      if (length(cols) == 0L) {
        print_err("Select at least one predictor column")
        return(invisible())
      }
      predictor_types <- vapply(seq_along(cols), function(i) {
        type <- input[[paste0("type_", i)]]
        if (is.null(type) || type == "") infer_bo_predictor_type(df[[cols[[i]]]]) else type
      }, character(1))
      names(predictor_types) <- cols

      objective <- input$objective
      if (is.null(objective) || !(objective %in% names(df)) || objective %in% cols) {
        print_err("Select an objective column which is not a predictor")
        return(invisible())
      }
      if (!is.numeric(df[[objective]]) && !all(is.na(df[[objective]]))) {
        print_err("Objective column must be numeric")
        return(invisible())
      }
      if (sum(!is.na(df[[objective]])) < 3L) {
        print_err("At least three measured experiments are required (rows with a non-empty objective)")
        return(invisible())
      }

      params <- list(
        df_source = df_source, predictor_types = predictor_types,
        objective = objective, direction = input$direction, seed = input$seed
      )

      State$bo_running <- TRUE
      State$bgp$start(
        fun = run_bo_suggest,
        args = c(list(data = df), params[setdiff(names(params), "df_source")]),
        on_success = function(res) {
          add_result(
            State, "bo_suggest", "Bayesian optimization: next experiment", params,
            methods::new("boSuggestionResult", df = res)
          )
        },
        on_finally = function() State$bo_running <- FALSE
      )
    })
  })
}
