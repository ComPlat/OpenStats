block_sizes_ok <- function(grouping, n_groups) {
  sizes <- table(grouping)
  length(sizes) > 0L && all(sizes >= n_groups)
}

randomAssignServer <- function(id, State) {
  shiny::moduleServer(id, function(input, output, session) {

    # dataset import
    # ======================================================================
    output$df_box <- shiny::renderUI({
      choices <- df_result_choices(State)
      if (length(choices) == 0L) return(shiny::tags$p("Import a dataset in the \"Data\" tab first."))
      shiny::tags$div(class = "doe-box", DT::DTOutput("RANDOMASSIGN-df_table"))
    })

    output$df_table <- DT::renderDT({
      shiny::req(!is.null(input$df_source), input$df_source %in% names(State$results))
      DT::datatable(State$results[[input$df_source]]@df, options = list(pageLength = 10), rownames = FALSE)
    })

    # set parameter for random assignment of infinite groups
    # ======================================================================
    output$assign_controls <- shiny::renderUI({
      choices <- df_result_choices(State)
      if (length(choices) == 0L) return(NULL)

      df_source <- input$df_source
      if (is.null(df_source) || !(df_source %in% choices)) df_source <- choices[[1]]
      df <- State$results[[df_source]]@df

      method <- input$randomization_method
      if (is.null(method)) method <- "simple"
      cols <- names(df)

      shiny::tags$div(
        class = "doe-box",
        shiny::h4("Random assignment"),
        shiny::selectInput("RANDOMASSIGN-df_source", "Dataset", choices = choices, selected = df_source),
        shiny::selectInput(
          "RANDOMASSIGN-randomization_method", "Method",
          choices = c("Simple" = "simple", "Block" = "block", "Block + stratified" = "block_stratified"),
          selected = method
        ),
        shiny::textInput("RANDOMASSIGN-groups", "Groups (comma separated)"),
        shiny::textInput("RANDOMASSIGN-ratios", "Ratios (comma separated, same order as groups)"),
        shiny::textInput("RANDOMASSIGN-col", "New column name", value = "group"),
        if (method == "block") {
          shiny::selectInput("RANDOMASSIGN-block_col", "Block column", choices = cols)
        },
        if (method == "block_stratified") {
          shiny::selectInput("RANDOMASSIGN-strata_cols", "Strata columns", choices = cols, multiple = TRUE)
        },
        shiny::numericInput("RANDOMASSIGN-seed", "Seed", value = 42, step = 1),
        shiny::actionButton("RANDOMASSIGN-assign", "Assign")
      )
    })

    # Start assignment
    # ======================================================================
    shiny::observeEvent(input$assign, {
      df_source <- input$df_source
      if (is.null(df_source) || is.null(State$results[[df_source]])) {
        print_err("Select a dataset first")
        return(invisible())
      }
      df <- State$results[[df_source]]@df

      groups <- parse_levels(input$groups)
      if (any_duplicates(groups)) {
        print_err("Found duplicated group names")
        return(invisible())
      }
      ratios <- parse_ratios(input$ratios)
      if (length(groups) < 1L || length(ratios) != length(groups)) {
        print_err("Provide the same number of groups and ratios")
        return(invisible())
      }

      col <- trimws(input$col)
      if (col == "" || col %in% names(df)) {
        print_err("Column name must be non-empty and not already exist in the dataset")
        return(invisible())
      }

      method <- input$randomization_method
      if (is.null(method)) method <- "simple"

      block_col <- NULL
      strata_cols <- NULL
      if (method == "block") {
        if (is.null(input$block_col) || input$block_col == "") {
          print_err("Select a block column")
          return(invisible())
        }
        if (!block_sizes_ok(df[[input$block_col]], length(groups))) {
          print_err("Block column has too many unique values: some blocks would have fewer rows than groups, which biases the assignment toward the first group. Pick a column with fewer distinct values.")
          return(invisible())
        }
        block_col <- input$block_col
      } else if (method == "block_stratified") {
        if (is.null(input$strata_cols) || length(input$strata_cols) == 0L) {
          print_err("Select at least one strata column")
          return(invisible())
        }
        if (!block_sizes_ok(interaction(df[input$strata_cols], drop = TRUE), length(groups))) {
          print_err("Strata columns produce too many unique combinations: some strata would have fewer rows than groups, which biases the assignment toward the first group. Pick fewer/coarser strata columns.")
          return(invisible())
        }
        strata_cols <- input$strata_cols
      }

      call_args <- list(
        groups = groups, ratios = ratios, col = col,
        block_col = block_col, strata_cols = strata_cols,
        randomization_method = method, seed = input$seed
      )
      res <- try(do.call(run_random_assign, c(list(df = df), call_args)), silent = TRUE)
      if (inherits(res, "try-error")) {
        print_err(conditionMessage(attr(res, "condition")))
        return(invisible())
      }
      params <- c(list(df_source = df_source), call_args)
      method_labels <- c(simple = "simple", block = "block", block_stratified = "block + stratified")
      label <- paste0("Random assignment (", method_labels[[method]], ")")
      add_result(State, "random_assign", label, params, methods::new("assignmentResult", df = res))
    })
  })
}
