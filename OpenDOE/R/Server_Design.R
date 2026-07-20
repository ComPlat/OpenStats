sample_size_choices <- function(State) {
  ids <- names(State$results)
  sample_size_ids <- ids[vapply(State$results, function(v) inherits(v, "sampleSizeResult"), logical(1))]

  free <- c("Free (enter manually)" = "free")
  if (length(sample_size_ids) == 0L) return(free)

  labels <- vapply(sample_size_ids, function(id) {
    v <- State$results[[id]]
    paste0(attr(v, "label"), " (n=", v@n, ")")
  }, character(1))
  c(stats::setNames(sample_size_ids, labels), free)
}

designServer <- function(id, State) {
  shiny::moduleServer(id, function(input, output, session) {

    output$design_controls <- shiny::renderUI({
      choices <- sample_size_choices(State)
      selected <- input$n_source
      if (is.null(selected) || !(selected %in% choices)) selected <- choices[[1]]

      shiny::tagList(
        shiny::selectInput(
          "DESIGN-n_source", "Sample size (n per combination)",
          choices = choices, selected = selected
        ),
        if (identical(selected, "free")) {
          shiny::numericInput("DESIGN-custom_n", "Custom n per combination", value = 1, min = 1, step = 1)
        },
        shiny::actionButton("DESIGN-build", "Build design")
      )
    })

    shiny::observeEvent(input$build, {
      if (length(State$predictors) == 0L) {
        print_err("Define predictors first")
        return(invisible())
      }

      n_source <- input$n_source
      if (is.null(n_source)) {
        print_err("Select a sample size")
        return(invisible())
      }

      if (identical(n_source, "free")) {
        n <- input$custom_n
        if (is.null(n) || is.na(n) || n < 1) {
          print_err("Enter a valid custom n")
          return(invisible())
        }
      } else {
        result <- State$results[[n_source]]
        if (is.null(result) || !inherits(result, "sampleSizeResult")) {
          print_err("Selected sample size no longer exists")
          return(invisible())
        }
        n <- result@n
      }
      n <- as.integer(round(n))

      params <- list(predictors = State$predictors, n_per_level = n)
      res <- try(do.call(run_completely_randomised_design, params), silent = TRUE)
      if (inherits(res, "try-error")) {
        print_err(conditionMessage(attr(res, "condition")))
        return(invisible())
      }
      label <- paste0("Design of experiment (n=", n, ")")
      add_result(State, "design", label, params, methods::new("designResult", df = res))
    })
  })
}
