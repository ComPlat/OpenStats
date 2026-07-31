finiteAssignServer <- function(id, State) {
  shiny::moduleServer(id, function(input, output, session) {

    # parameter for finite groups assignment
    # ======================================================================
    output$assign_controls <- shiny::renderUI({
      design_choices <- df_result_choices(State, classes = c("designResult", "assignmentResult"))
      if (length(design_choices) == 0L) {
        return(shiny::tags$p("Build a design in the \"Design of experiment\" tab first."))
      }

      groups_choices <- df_result_choices(State)
      if (length(groups_choices) == 0L) {
        return(shiny::tags$p("Import a dataset in the \"Data\" tab first."))
      }

      groups_source <- input$groups_source
      if (is.null(groups_source) || !(groups_source %in% groups_choices)) groups_source <- groups_choices[[1]]
      groups_cols <- names(State$results[[groups_source]]@df)

      shiny::tags$div(
        class = "doe-box",
        shiny::h4("Finite group assignment"),
        shiny::selectInput("FINITEASSIGN-groups_source", "Covariate dataset", choices = groups_choices, selected = groups_source),
        shiny::selectInput(
          "FINITEASSIGN-id_col", "ID column (identifies each physical unit, e.g. animal/sample ID)",
          choices = groups_cols
        ),
        shiny::selectInput("FINITEASSIGN-design_source", "Design", choices = design_choices),
        shiny::selectInput(
          "FINITEASSIGN-loss_function", "Loss function",
          choices = c("Default" = "Default", "Mahalanobis" = "Mahalanobis")
        ),
        shiny::numericInput("FINITEASSIGN-max_iter", "Max iterations", value = 50, min = 1, step = 1),
        shiny::numericInput("FINITEASSIGN-seed", "Seed", value = 42, step = 1),
        shiny::actionButton("FINITEASSIGN-assign", "Assign")
      )
    })

    output$status_box <- shiny::renderUI({
      if (isTRUE(State$finite_assign_running)) {
        shiny::tags$div(
          class = "doe-box",
          shiny::tags$p("Optimizing assignment..."),
          shiny::actionButton("FINITEASSIGN-cancel", "Cancel", class = "btn-danger")
        )
      }
    })

    shiny::observeEvent(input$cancel, {
      State$bgp$cancel()
    }, ignoreInit = TRUE)

    # start assignment optimization
    # ======================================================================
    shiny::observeEvent(input$assign, {
      groups_id <- input$groups_source
      if (is.null(groups_id) || is.null(State$results[[groups_id]])) {
        print_err("Select a covariate dataset first")
        return(invisible())
      }
      groups_df_full <- State$results[[groups_id]]@df

      id_col <- input$id_col
      if (is.null(id_col) || id_col == "" || !(id_col %in% names(groups_df_full))) {
        print_err("Select the ID column of the covariate dataset")
        return(invisible())
      }
      ids <- groups_df_full[[id_col]]
      groups_df <- groups_df_full[, setdiff(names(groups_df_full), id_col), drop = FALSE]

      if (ncol(groups_df) == 0L) {
        print_err("The covariate dataset needs at least one column besides the ID column")
        return(invisible())
      }
      if (!all(vapply(groups_df, is.numeric, logical(1)))) {
        print_err("Covariate dataset must have numeric columns only (besides the ID column)")
        return(invisible())
      }

      design_id <- input$design_source
      if (is.null(design_id) || is.null(State$results[[design_id]])) {
        print_err("Select a design")
        return(invisible())
      }
      design_df <- State$results[[design_id]]@df

      if (nrow(groups_df) < nrow(design_df)) {
        print_err("Covariate dataset must have at least as many rows as the design has slots")
        return(invisible())
      }

      params <- list(
        groups_id = groups_id, id_col = id_col, design_id = design_id, max_iter = input$max_iter,
        loss_function = input$loss_function, seed = input$seed
      )

      State$finite_assign_running <- TRUE
      State$bgp$start(
        fun = run_random_finite_assign,
        args = list(
          seed = input$seed, groups = groups_df, design = design_df, ids = ids,
          max_iter = input$max_iter, loss_function = input$loss_function
        ),
        on_success = function(res) {
          add_result(
            State, "random_finite_assign", "Random assignment (finite groups)", params,
            methods::new("finiteAssignmentResult", df = res$df, loss = res$loss)
          )
        },
        on_finally = function() State$finite_assign_running <- FALSE
      )
    })
  })
}
