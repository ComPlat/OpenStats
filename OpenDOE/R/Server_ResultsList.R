build_result_box <- function(id, value) {
  box_id <- paste0("RESULTS-result-box-", id)
  label  <- attr(value, "label")
  if (inherits(value, "predictorTable")) {
    htmltools::div(
      id = box_id,
      class = "doe-box",
      htmltools::strong(label),
      DT::DTOutput(paste0("RESULTS-res_", id)),
      shiny::actionButton(paste0("RESULTS-remove_", id), "Remove", class = "btn-danger")
    )
  } else if (inherits(value, "sampleSizeResult")) {
    htmltools::div(
      id = box_id,
      class = "doe-box",
      htmltools::strong(label),
      shiny::verbatimTextOutput(paste0("RESULTS-res_", id)),
      shiny::actionButton(paste0("RESULTS-remove_", id), "Remove", class = "btn-danger")
    )
  } else {
    htmltools::div(
      id = box_id,
      class = "doe-box",
      htmltools::strong(label),
      shiny::verbatimTextOutput(paste0("RESULTS-res_", id)),
      shiny::actionButton(paste0("RESULTS-remove_", id), "Remove", class = "btn-danger")
    )
  }
}

resultsListServer <- function(id, State) {
  shiny::moduleServer(id, function(input, output, session) {

    output$results_header <- shiny::renderUI({
      if (length(State$results) == 0L) return(shiny::tags$p("No results yet."))
      shiny::tags$h4("Results")
    })

    # Insert a box (+ wire its output/remove button) only for results not yet
    # added, instead of rebuilding the whole list on every new result.
    shiny::observe({
      if (length(State$results) == 0L) return()
      all_ids <- names(State$results)
      already <- State$registered_results
      to_add  <- setdiff(all_ids, already)
      if (!length(to_add)) return()

      lapply(to_add, function(result_id) {
        value <- State$results[[result_id]]

        shiny::insertUI(
          selector = "#RESULTS-results-container",
          where = "afterBegin",
          ui = build_result_box(result_id, value)
        )

        if (inherits(value, "predictorTable")) {
          output[[paste0("res_", result_id)]] <- DT::renderDT(
            DT::datatable(value@df, options = list(dom = "t", paging = FALSE), rownames = FALSE)
          )
        } else if (inherits(value, "sampleSizeResult")) {
          output[[paste0("res_", result_id)]] <- shiny::renderPrint(value@n)
        } else {
          output[[paste0("res_", result_id)]] <- shiny::renderPrint(value)
        }

        shiny::observeEvent(input[[paste0("remove_", result_id)]], {
          results <- State$results
          results[[result_id]] <- NULL
          State$results <- results
          shiny::removeUI(selector = paste0("#RESULTS-result-box-", result_id))
        }, ignoreInit = TRUE)
      })

      State$registered_results <- union(already, to_add)
    })
  })
}
