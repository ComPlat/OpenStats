finiteAssignServer <- function(id, State) {
  shiny::moduleServer(id, function(input, output, session) {

    # parameter for finite groups assignment
    # ======================================================================
    output$assign_controls <- shiny::renderUI({
      choices <- df_result_choices(State)
      if (length(choices) == 0L) {
        return(shiny::tags$p("Import a dataset in the \"Data\" tab, or build a design in the \"Design of experiment\" tab, first."))
      }

      shiny::tags$div(
        class = "doe-box",
        shiny::h4("Finite group assignment"),
        shiny::selectInput("FINITEASSIGN-groups_source", "Covariate dataset (numeric columns only)", choices = choices),
        shiny::selectInput("FINITEASSIGN-design_source", "Design (created in OpenDOE or imported)", choices = choices),
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
      groups_df <- State$results[[groups_id]]@df
      if (!all(vapply(groups_df, is.numeric, logical(1)))) {
        print_err("Covariate dataset must have numeric columns only")
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
        groups_id = groups_id, design_id = design_id, max_iter = input$max_iter,
        loss_function = input$loss_function, seed = input$seed
      )

      State$finite_assign_running <- TRUE
      State$bgp$start(
        fun = run_random_finite_assign,
        args = list(
          seed = input$seed, groups = groups_df, design = design_df,
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
