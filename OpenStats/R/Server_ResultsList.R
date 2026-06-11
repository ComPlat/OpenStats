ResultsListServer <- function(id, DataModelState, ResultsState, MethodState, conditional_panels) {
  shiny::moduleServer(id, function(input, output, session) {
    # Results
    # ----------------------------------------------------------
    # Render results list
    output$Results <- shiny::renderUI({
      if (conditional_panels() == "DataWrangling") {
        return(
          htmltools::div(
            class = "var-box-output",
            htmltools::h3(htmltools::strong("The results are displayed in the other tabs"))
          )
        )
      }
      res <- ResultsState$all_data |> rev()
      if (length(res) == 0) {
        return()
      }
      res_ui_list <- lapply(names(res), function(name) {
        temp <- res[[name]]
        if (is.vector(temp)) {
          htmltools::div(
            class = "var-box-output",
            htmltools::div(class = "var-box-name", name),
            shiny::verbatimTextOutput(paste0("RESULTS-res_", name)),
            shiny::actionButton(paste0("RESULTS-remove_res_", name), "Remove", class = "btn-danger")
          )
        } else if (is.data.frame(temp)) {
          htmltools::div(
            class = "var-box-output",
            htmltools::div(class = "var-box-name", name),
            DT::DTOutput(paste0("RESULTS-res_", name)),
            shiny::actionButton(paste0("RESULTS-remove_res_", name), "Remove", class = "btn-danger")
          )
        } else if (inherits(temp, "doseResponse")) {
          htmltools::div(
            class = "var-box-output",
            htmltools::div(class = "var-box-name", name),
            shiny::plotOutput(paste0("RESULTS-res_dose_response_", name), width = "100%", height = "800px"),
            shiny::actionButton(paste0("RESULTS-res_previous_", name), "Previous plot"),
            shiny::actionButton(paste0("RESULTS-res_next_", name), "Next plot"),
            DT::DTOutput(paste0("RESULTS-res_dose_response_df_", name)),
            shiny::actionButton(paste0("RESULTS-remove_res_", name), "Remove", class = "btn-danger")
          )
        } else if (inherits(temp, "plot")) {
          htmltools::div(
            class = "var-box-output",
            htmltools::div(class = "var-box-name", name),
            shiny::plotOutput(paste0("RESULTS-res_", name), width = "100%", height = "800px"),
            shiny::actionButton(paste0("RESULTS-remove_res_", name), "Remove", class = "btn-danger")
          )
        } else if (inherits(temp, "summaryModel")) {
          htmltools::div(
            class = "var-box-output",
            htmltools::div(class = "var-box-name", name),
            shiny::plotOutput(paste0("RESULTS-res_plot_", name)),
            DT::DTOutput(paste0("RESULTS-res_summary_", name)),
            DT::DTOutput(paste0("RESULTS-res_information_criterion_", name)),
            shiny::actionButton(paste0("RESULTS-remove_res_", name), "Remove", class = "btn-danger")
          )
        } else if (inherits(temp, "summaryDataFrame")) {
          htmltools::div(
            class = "var-box-output",
            htmltools::div(class = "var-box-name", name),
            DT::DTOutput(paste0("RESULTS-res_summary_", name)),
            shiny::actionButton(paste0("RESULTS-remove_res_", name), "Remove", class = "btn-danger")
          )
        } else if (inherits(temp, "summaryPlotDataFrame")) {
          htmltools::div(
            class = "var-box-output",
            htmltools::div(class = "var-box-name", name),
            shiny::plotOutput(paste0("RESULTS-res_summary_plot_", name), width = "100%", height = "800px"),
            shiny::actionButton(paste0("RESULTS-remove_res_", name), "Remove", class = "btn-danger")
          )
        } else {
          htmltools::div(
            class = "var-box-output",
            htmltools::div(class = "var-box-name", name),
            shiny::verbatimTextOutput(paste0("RESULTS-res_", name)),
            shiny::actionButton(paste0("RESULTS-remove_res_", name), "Remove", class = "btn-danger")
          )
        }
      })
      if (MethodState$method == "Default") {
        download_stuff <- htmltools::div(
          class = "var-box-output",
          htmltools::h3(htmltools::strong("Results")),
          htmltools::p("The following list contains the results"),
          shiny::actionButton("RESULTS-download", "Save"),
          shiny::textInput("RESULTS-user_filename", "Set filename", value = "")
        )
        do.call(htmltools::tagList, list(download_stuff, res_ui_list))
      }
      else if (MethodState$method == "DoseResponse") {
        download_stuff <- htmltools::div(
          class = "var-box-output",
          htmltools::h3(htmltools::strong("Results")),
          htmltools::p("The following list contains the results"),
          shiny::actionButton("RESULTS-download", "Save")
        )
        do.call(htmltools::tagList, list(download_stuff, res_ui_list))
      }
      else if (MethodState$method == "VariationStatistics") {
        download_stuff <- htmltools::div(
          class = "var-box-output",
          htmltools::h3(htmltools::strong("Results")),
          htmltools::p("The following list contains the results"),
          shiny::actionButton("RESULTS-download", "Save")
        )
        do.call(htmltools::tagList, list(download_stuff, res_ui_list))
      }
    })

    # Show results
    shiny::observe({
      if (length(ResultsState$all_data) == 0) {
        return()
      }
      res <- ResultsState$all_data
      res_ui_list <- lapply(names(res), function(name) {
        rendered <- attributes(ResultsState$all_data[[name]])$rendered
        if (!is.null(rendered) && rendered) return()
        shiny::observeEvent(paste0("RESULTS-", name), {
          temp <- res[[name]]
          set_rendered <- if (inherits(temp, "doseResponse")) FALSE else TRUE
          if (is.vector(temp)) {
            output[[paste0("res_", name)]] <- shiny::renderPrint(temp)
          } else if (is.data.frame(temp)) {
            output[[paste0("res_", name)]] <- render_df(temp)
          } else if (inherits(temp, "doseResponse")) {
            output[[paste0("res_dose_response_", name)]] <- shiny::renderPlot(temp@p[[temp@current_page]])
            output[[paste0("res_dose_response_df_", name)]] <- render_df(temp@df, 2)
          } else if (inherits(temp, "plot")) {
            output[[paste0("res_", name)]] <- shiny::renderPlot(temp@p)
          } else if (inherits(temp, "summaryModel")) {
            output[[paste0("res_plot_", name)]] <- shiny::renderPlot(temp@p)
            output[[paste0("res_summary_", name)]] <- render_df(temp@summary)
            output[[paste0("res_information_criterion_", name)]] <- render_df(temp@information_criterions)
          } else if (inherits(temp, "summaryDataFrame")) {
            output[[paste0("res_summary_", name)]] <- render_df(temp@summary)
          } else if (inherits(temp, "summaryPlotDataFrame")) {
            output[[paste0("res_summary_plot_", name)]] <- shiny::renderPlot(temp@p)
          } else {
            output[[paste0("res_", name)]] <- shiny::renderPrint(temp)
          }
          if (set_rendered) attr(ResultsState$all_data[[name]], "rendered") <- TRUE
        })
      })
      do.call(htmltools::tagList, res_ui_list)
    })

    # Observe remove buttons
    shiny::observe({
      if (length(ResultsState$all_data) == 0) {
        return()
      }
      current_list <- ResultsState$all_data
      lapply(names(current_list), function(name) {
        shiny::observeEvent(input[[paste0("remove_res_", name)]],
          {
            e <- try({
              rr <- get_remove_results()$new(name)
              rr$eval(ResultsState)
            })
            if (inherits(e, "try-error")) {
              err <- conditionMessage(attr(e, "condition"))
              print_err(err)
            }
          },
          ignoreInit = TRUE
        )
      })
    })
    # Handle previous & next buttons of dose response plots
    shiny::observe({
      if (length(ResultsState$all_data) == 0) return()
      all_names <- names(ResultsState$all_data)
      already   <- ResultsState$registered_pagers
      to_add    <- setdiff(all_names, already)
      if (!length(to_add)) return()
      lapply(to_add, function(name) {
        shiny::observeEvent(input[[paste0("res_previous_", name)]], ignoreInit = TRUE, {
          obj <- ResultsState$all_data[[name]]
          if (is.null(obj)) return()
          if (obj@current_page >= 2L) {
            obj@current_page <- obj@current_page - 1L
            ResultsState$all_data[[name]] <- obj
          }
        })
        shiny::observeEvent(input[[paste0("res_next_", name)]], ignoreInit = TRUE, {
          obj <- ResultsState$all_data[[name]]
          if (is.null(obj)) return()
          n <- length(obj@p)
          if (obj@current_page < n) {
            obj@current_page <- obj@current_page + 1L
            ResultsState$all_data[[name]] <- obj
          }
        })
      })
      ResultsState$registered_pagers <- union(already, to_add)
    })
  })
}
