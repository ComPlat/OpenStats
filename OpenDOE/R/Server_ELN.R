elnServer <- function(input, output, session, State) {
  shiny::observeEvent(session$clientData$url_search, {
    query <- shiny::parseQueryString(session$clientData$url_search)
    State$predictors_locked <- !is.null(query$method) && nzchar(query$method)
    if (identical(query$method, ELN_SAMPLE_SIZE_METHOD)) {
      res <- try(import_eln_sample_size(State, fetch_eln_doe(session)), silent = TRUE)
      if (inherits(res, "try-error")) {
        print_err(conditionMessage(attr(res, "condition")))
        return(invisible())
      }
      for (note in res$notes) print_warn(note)
      print_success(paste0(
        "Imported the study protocol from the ELN: ", length(res$predictors), " predictor(s), ",
        length(res$outcomes), " outcome(s), ", length(res$controls), " control(s)."
      ))
      incomplete <- eln_incomplete_predictors(State$predictors)
      if (length(State$predictors) == 0L || length(incomplete) > 0L) {
        if (length(incomplete) > 0L) {
          print_warn(paste0("Assign the control(s) to the predictor(s) with a single level: ", paste(incomplete, collapse = ", ")))
        }
        shiny::updateTabsetPanel(session, "conditionedPanels", selected = "Predictors")
        return(invisible())
      }
      shiny::updateTabsetPanel(session, "conditionedPanels", selected = "Sample size")
      return(invisible())
    }
    if (!identical(query$method, ELN_DOE_METHOD)) return(invisible())
    mode <- query$mode
    if (is.null(mode) || !(mode %in% ELN_DOE_MODES)) {
      print_err(paste0("Unknown mode from the ELN: ", mode))
      return(invisible())
    }

    import <- if (mode == "initial") import_eln_doe else import_eln_doe_variations
    res <- try(import(State, fetch_eln_doe(session)), silent = TRUE)
    if (inherits(res, "try-error")) {
      print_err(conditionMessage(attr(res, "condition")))
      return(invisible())
    }

    if (mode == "initial") {
      print_success(paste0(
        "Imported ", length(State$predictors), " factor(s) from the ELN. ",
        "Build the first ", State$eln$batch_size, " experiments with the Latin hypercube."
      ))
      shiny::updateTabsetPanel(session, "conditionedPanels", selected = "Design of experiment")
      return(invisible())
    }

    if (length(res$invalid_ids) > 0L) {
      print_warn(paste0(
        "Ignored variation(s) ", paste(res$invalid_ids, collapse = ", "),
        ": none or several samples of a material selection are present."
      ))
    }
    print_success(paste0(
      "Imported ", nrow(res$df), " variation(s) from the ELN. Select the measured result(s) and suggest the next batch."
    ))
    shiny::updateTabsetPanel(session, "conditionedPanels", selected = "Bayesian optimization")
  }, once = TRUE)

  output$`ELN-send_controls` <- shiny::renderUI({
    if (is.null(State$eln)) return(NULL)
    choices <- df_result_choices(State, c("designResult", "boSuggestionResult"))
    if (length(choices) == 0L) return(NULL)
    shiny::tags$div(
      class = "doe-box",
      shiny::h4("Send to ELN"),
      shiny::selectInput("ELN-send_source", "Design", choices = choices, selected = choices[[length(choices)]]),
      shiny::actionButton("ELN-send", "Send experiments to the ELN")
    )
  })

  shiny::observeEvent(input$`ELN-send`, {
    result <- State$results[[input$`ELN-send_source`]]
    if (is.null(result) || !inherits(result, c("designResult", "boSuggestionResult"))) {
      print_err("Select a design first")
      return(invisible())
    }
    res <- try(send_eln_doe_batch(session, eln_send_df(State, input$`ELN-send_source`), State$eln), silent = TRUE)
    if (inherits(res, "try-error")) print_err(conditionMessage(attr(res, "condition")))
  }, ignoreInit = TRUE)

  output$`ELN-send_sample_size_controls` <- shiny::renderUI({
    if (is.null(State$eln_sample_size)) return(NULL)
    choices <- df_result_choices(State, "sampleSizeResult")
    if (length(choices) == 0L) return(NULL)
    shiny::tags$div(
      class = "doe-box",
      shiny::h4("Send to ELN"),
      shiny::selectInput("ELN-send_sample_size_source", "Sample size calculation", choices = choices, selected = choices[[length(choices)]]),
      shiny::actionButton("ELN-send_sample_size", "Send sample size to the ELN")
    )
  })

  shiny::observeEvent(input$`ELN-send_sample_size`, {
    id <- input$`ELN-send_sample_size_source`
    result <- State$results[[id]]
    if (is.null(result) || !inherits(result, "sampleSizeResult")) {
      print_err("Select a sample size calculation first")
      return(invisible())
    }
    res <- try({
      out <- eln_sample_size_output(State$eln_sample_size, result, State$history[[id]], State$predictors)
      send_eln_sample_size(session, out)
    }, silent = TRUE)
    if (inherits(res, "try-error")) print_err(conditionMessage(attr(res, "condition")))
  }, ignoreInit = TRUE)
}
