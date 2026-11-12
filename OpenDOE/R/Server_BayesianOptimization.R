BO_PREDICTOR_TYPES <- c("continuous", "discrete", "categorical")

# Variations imported from the ELN: predictors, types and bounds come from the doe_definition.
bo_eln_preset <- function(State, df_source) {
  eln <- State$eln
  if (is.null(eln$df_source) || !identical(df_source, eln$df_source)) return(NULL)
  list(predictors = State$predictors, predictor_types = State$predictor_types, n_points = eln$batch_size)
}

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
      if (is.null(df_source) || !(df_source %in% choices)) {
        df_source <- if (isTRUE(State$eln$df_source %in% choices)) State$eln$df_source else choices[[1]]
      }
      cols <- names(State$results[[df_source]]@df)
      selected_cols <- shiny::isolate(input$predictor_cols)
      preset <- bo_eln_preset(State, df_source)
      if (is.null(selected_cols) && !is.null(preset)) selected_cols <- names(preset$predictors)

      shiny::tags$div(
        class = "doe-box",
        shiny::h4("Bayesian optimization"),
        shiny::selectInput("BO-df_source", "Measured experiments", choices = choices, selected = df_source),
        shiny::selectInput(
          "BO-predictor_cols", "Predictor columns", choices = cols,
          selected = intersect(selected_cols, cols), multiple = TRUE
        )
      )
    })

    output$type_controls <- shiny::renderUI({
      df <- selected_df()
      cols <- intersect(input$predictor_cols, names(df))
      if (is.null(df) || length(cols) == 0L) return(NULL)
      preset_types <- bo_eln_preset(State, input$df_source)$predictor_types
      shiny::tags$div(
        class = "doe-box",
        shiny::h4("Predictor types"),
        lapply(seq_along(cols), function(i) {
          selected <- if (cols[[i]] %in% names(preset_types)) preset_types[[cols[[i]]]] else infer_bo_predictor_type(df[[cols[[i]]]])
          shiny::selectInput(paste0("BO-type_", i), cols[[i]], choices = BO_PREDICTOR_TYPES, selected = selected)
        })
      )
    })

    output$objective_controls <- shiny::renderUI({
      df <- selected_df()
      if (is.null(df)) return(NULL)
      objective_choices <- setdiff(names(df), c(input$predictor_cols, "pareto_optimal"))
      shiny::tags$div(
        class = "doe-box",
        shiny::selectInput(
          "BO-objective", "Measured result(s) (columns to optimize)", choices = objective_choices,
          selected = intersect(shiny::isolate(input$objective), objective_choices), multiple = TRUE
        )
      )
    })

    output$direction_controls <- shiny::renderUI({
      objectives <- input$objective
      if (length(objectives) == 0L) return(NULL)
      shiny::tags$div(
        class = "doe-box",
        shiny::h4("Direction"),
        lapply(seq_along(objectives), function(i) {
          shiny::selectInput(
            paste0("BO-direction_", i), objectives[[i]],
            choices = c("Maximize" = "maximize", "Minimize" = "minimize")
          )
        })
      )
    })

    output$run_controls <- shiny::renderUI({
      df <- selected_df()
      if (is.null(df)) return(NULL)
      preset <- bo_eln_preset(State, input$df_source)
      shiny::tags$div(
        class = "doe-box",
        shiny::numericInput(
          "BO-n_points", "Number of suggested experiments",
          value = if (is.null(preset)) 1L else preset$n_points, min = 1, step = 1
        ),
        shiny::numericInput("BO-seed", "Seed", value = 42, step = 1),
        shiny::actionButton("BO-suggest", "Suggest next experiment(s)")
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
      if (length(objective) == 0L || !all(objective %in% names(df)) || any(objective %in% cols)) {
        print_err("Select at least one measured result column which is not a predictor")
        return(invisible())
      }
      numeric_ok <- vapply(objective, function(o) is.numeric(df[[o]]) || all(is.na(df[[o]])), logical(1))
      if (!all(numeric_ok)) {
        print_err(paste0("Measured result columns must be numeric: ", paste(objective[!numeric_ok], collapse = ", ")))
        return(invisible())
      }
      direction <- vapply(seq_along(objective), function(i) {
        d <- input[[paste0("direction_", i)]]
        if (is.null(d) || d == "") "maximize" else d
      }, character(1))
      # fixed bounds only where the user kept the type from the doe_definition
      preset <- bo_eln_preset(State, df_source)
      fixed <- names(predictor_types)[vapply(names(predictor_types), function(col) {
        col %in% names(preset$predictor_types) && preset$predictor_types[[col]] == predictor_types[[col]]
      }, logical(1))]
      bounds <- if (length(fixed) == 0L) NULL else preset$predictors[fixed]
      conv <- try(bo_predictors(df, predictor_types, bounds), silent = TRUE)
      if (inherits(conv, "try-error")) {
        print_err(conditionMessage(attr(conv, "condition")))
        return(invisible())
      }
      n_measured <- sum(stats::complete.cases(df[objective]))
      n_min <- bo_min_experiments(conv$predictors, conv$predictor_types)
      if (n_measured < n_min) {
        print_err(paste0(
          "At least ", n_min, " measured experiments are required for these predictors (found ", n_measured, "). ",
          "Categorical predictors count with one dimension per level."
        ))
        return(invisible())
      }

      n_points <- input$n_points
      if (is.null(n_points)) n_points <- 1
      if (is.na(n_points) || n_points < 1) {
        print_err("Number of suggested experiments must be at least 1")
        return(invisible())
      }

      params <- list(
        df_source = df_source, predictor_types = predictor_types,
        objective = objective, direction = direction, seed = input$seed,
        n_points = as.integer(round(n_points)), bounds = bounds
      )

      State$bo_running <- TRUE
      State$bgp$start(
        fun = run_bo_suggest,
        args = c(list(data = df), params[setdiff(names(params), "df_source")]),
        on_success = function(res) {
          add_result(
            State, "bo_suggest", paste0("Bayesian optimization: next experiment(s) (n=", params$n_points, ")"), params,
            methods::new("boSuggestionResult", df = res)
          )
        },
        on_finally = function() State$bo_running <- FALSE
      )
    })
  })
}
