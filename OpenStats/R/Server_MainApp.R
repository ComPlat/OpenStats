app <- function() {

  ui <- main_app_ui()

  server <- function(input, output, session) {
    # States
    # ----------------------------------------------------------
    # Create background process instance
    bgp <- get_bg_process()$new()

    # States
    MethodState <- shiny::reactiveValues(
      method = "Default", storage_class = NULL
    )
    DataModelState <- shiny::reactiveValues(
      df = NULL, formula = NULL,
      backup_df = NULL, filter_col = NULL, filter_group = NULL,
      active_df_name = NULL,
      rhs_string = NULL
    )

    ResultsState <- shiny::reactiveValues(
      all_data = list(),
      history = list(),
      counter = 0,
      bgp = bgp,
      registered_pagers = character()
    )

    DataWranglingState <- shiny::reactiveValues(
      df = NULL, df_name = "df",
      current_page = 1, total_pages = 1,
      counter_id = 0,
      intermediate_vars = list(),
      code_string = NULL
    )
    bgp$init(ResultsState, DataModelState, DataWranglingState) # NOTE: creates the polling observer

    shiny::observeEvent(ResultsState$counter, { # For testing
      session$userData$export <- ResultsState$all_data
    }, ignoreInit = TRUE)
    shiny::observe({ # For testing; need to use the invalidate later pattern to gather the code_string
      shiny::invalidateLater(500)
      session$userData$export_iv <- DataWranglingState$intermediate_vars
      session$userData$export_code_string <- DataWranglingState$code_string
      session$userData$export_formula_rhs <- DataModelState$rhs_string
    })
    shiny::exportTestValues(result_list = ResultsState$all_data)

    # Running status
    # ----------------------------------------------------------
    shiny::observe({
      shiny::invalidateLater(250)
      status <- ResultsState$bgp$running_status
      if (status == "Running...") {
        # disable the buttons of the tabs different from the current tab
        shinyjs::disable("VIS-CreatePlotBox")
        shinyjs::disable("VIS-CreatePlotScatter")
        shinyjs::disable("VIS-CreatePlotLine")
        shinyjs::disable("VIS-CreateModelBox")
        shinyjs::disable("VIS-CreateModelScatter")
        shinyjs::disable("VIS-CreateModelLine")

        shinyjs::disable("DOSERESPONSE-ic50")

        shinyjs::disable("HISTORY-replay_history")

        shinyjs::enable("ASS-DiagnosticPlot")

        shinyjs::enable("TESTS-PermANOVATest")
      }
    })
    # React to press cancel
    shiny::observeEvent(input$confirm_stop, {
      ResultsState$bgp$cancel()
    })

    # Show running_status
    shiny::observe({
      shiny::invalidateLater(250)
      status <- ResultsState$bgp$running_status
      output$running_status <- shiny::renderText(status)
      if ((status == "Running...") && !ResultsState$bgp$cancel_clicked) {
        shinyjs::show("running_status")
        shinyjs::show("confirm_stop")
      } else {
        shinyjs::hide("running_status")
        shinyjs::hide("confirm_stop")
      }
    })

    # docu
    # ----------------------------------------------------------
    show_docu(input)

    # upload local file from user or download from user
    # ----------------------------------------------------------
    download_file <- shiny::reactive({
      out_dir <- file.path(tempdir(), "openstats-results")
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

      if (!requireNamespace("COMELN", quietly = TRUE)) {
        shiny::validate(shiny::need(FALSE,
          paste(
            "Feature requires the 'COMELN' package.",
            "Install it to enable downloads.",
            'For GitHub: remotes::install_github("ComPlat/OpenStats", subdir = "comeln")'
          )
        ))
        return(NULL)
      }
      downloader <- getExportedValue("COMELN", "download")

      file <- downloader(session, out_dir) #"/home/shiny/results"

      if (MethodState$method == "Default") {
        env_import$read_data(file, DataModelState, ResultsState)
      } else if (MethodState$method == "DoseResponse") {
        MethodState$storage_class <- new("MethodDoseResponse", id = "", request_id = "")
        env_import_dose_response$import_dose_response_json(file, DataModelState, ResultsState, MethodState)
      }
      print_req(
        is.data.frame(DataModelState$df),
        "File can not be used. Upload into R failed!"
      )
      tryCatch(
        {
          unlink(file)
        },
        warning = function(warn) {
          print_warn(paste("A warning occurred: ", conditionMessage(warn)))
        },
        error = function(err) {
          print_err(paste("An error occurred: ", conditionMessage(err)))
        }
      )
      shiny::req(is.data.frame(DataModelState$df))
    })

    get_method <- shiny::reactive({
      if (!requireNamespace("COMELN", quietly = TRUE)) {
        shiny::validate(shiny::need(FALSE,
          paste(
            "Feature requires the 'COMELN' package.",
            "Install it to enable downloads.",
            'For GitHub: remotes::install_github("ComPlat/OpenStats", subdir = "comeln")'
          )
        ))
        return(NULL)
      }
      get_method <- getExportedValue("COMELN", "getMethod")
      m <- get_method()
      if (m == "DoseResponse") {
        MethodState$method <- "DoseResponse"
        MethodState$storage_class <- new(
          "MethodDoseResponse",
          id = "", request_id = ""
        )
      }
    })

    if (Sys.getenv("RUN_MODE") == "SERVER") {
      shiny::observe({
        shiny::req(is.null(DataModelState$df))
        e <- try(get_method(), silent = TRUE)
        if (inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
          print_err(err)
        }
        e <- try(download_file(), silent = TRUE)
        if (inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
          print_err(err)
        }
      })
    } else {
      shiny::observeEvent(input$file, {
        e <- try(env_import$read_data(input$file$datapath, DataModelState, ResultsState))
        if (inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
          print_err(err)
        }
      })
    }

    # dataset
    # ----------------------------------------------------------
    output$df <- DT::renderDT({
      shiny::req(DataModelState$df)
      DT::datatable(DataModelState$df, options = list(pageLength = 10))
    })
    shiny::observe({
      shiny::req(!is.null(DataModelState$df))
      shiny::req(is.data.frame(DataModelState$df))
      if (length(ResultsState$history) == 0) {
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(type = "Version", Nr = get_current_version())
      }
      output$df <- DT::renderDT(
        DT::datatable(DataModelState$df, options = list(pageLength = 10))
      )
    })
    # Observe tables
    output[["active_df"]] <- shiny::renderUI({
      if (input$conditionedPanels == "DataWrangling") {
        return()
      }
      shiny::req(!is.null(DataModelState$df))
      shiny::req(is.data.frame(DataModelState$df))
      if (length(ResultsState$all_data) == 0) {
        return(NULL)
      }
      table_indices <- which(sapply(ResultsState$all_data, is.data.frame))
      names <- names(ResultsState$all_data)
      names <- names[table_indices]
      tooltip <- "Select the active dataset (the dataset with which you can work)"
      htmltools::div(
        class = "boxed-output",
        shiny::tags$label(
          "active dataset",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        shiny::selectInput(
          inputId = "tables-dropdown",
          label = "active dataset",
          choices = names,
          selected = DataModelState$active_df_name
        )
      )
    })
    shiny::observeEvent(input[["tables-dropdown"]], {
      shiny::req(!is.null(DataModelState$df))
      shiny::req(is.data.frame(DataModelState$df))
      shiny::req(input[["tables-dropdown"]])
      sat <- get_set_active_table()$new(input[["tables-dropdown"]])
      sat$eval(ResultsState, DataModelState)
    })

    # Other tabs
    # ----------------------------------------------------------
    OperationEditorServer("OP", DataModelState, ResultsState, DataWranglingState)
    corrServer("CORR", DataModelState, ResultsState)
    visServer("VIS", DataModelState, ResultsState)
    assServer("ASS", DataModelState, ResultsState)
    testsUIServer("TESTS", DataModelState, ResultsState)
    testsUISidebarServer("TESTS", DataModelState, ResultsState)
    testsServer("TESTS", DataModelState, ResultsState)
    DoseResponseServer("DOSERESPONSE", DataModelState, ResultsState)
    FormulaEditorServer("FO", DataModelState, ResultsState)
    SplitByGroupServer("SG", DataModelState, ResultsState)
    HistoryEditorServer("HISTORY", DataModelState, ResultsState, DataWranglingState)

    # Results
    # ----------------------------------------------------------
    # Render results list
    output$Results <- shiny::renderUI({
      if (input$conditionedPanels == "DataWrangling") {
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
            htmltools::div(
              class = "var-box-name",
              name
            ),
            shiny::verbatimTextOutput(paste0("res_", name)),
            shiny::actionButton(paste0("remove_res_", name), "Remove", class = "btn-danger")
          )
        } else if (is.data.frame(temp)) {
          htmltools::div(
            class = "var-box-output",
            htmltools::div(
              class = "var-box-name",
              name
            ),
            DT::DTOutput(paste0("res_", name)),
            shiny::actionButton(paste0("remove_res_", name), "Remove", class = "btn-danger")
          )
        } else if (inherits(temp, "doseResponse")) {
          htmltools::div(
            class = "var-box-output",
            htmltools::div(
              class = "var-box-name",
              name
            ),
            shiny::plotOutput(paste0("res_dose_response_", name),
              width = "100%", height = "800px"
            ),
            shiny::actionButton(paste0("res_previous_", name), "Previous plot"),
            shiny::actionButton(paste0("res_next_", name), "Next plot"),
            DT::DTOutput(paste0("res_dose_response_df_", name)),
            shiny::actionButton(paste0("remove_res_", name), "Remove", class = "btn-danger")
          )
        } else if (inherits(temp, "plot")) {
          htmltools::div(
            class = "var-box-output",
            htmltools::div(
              class = "var-box-name",
              name
            ),
            shiny::plotOutput(paste0("res_", name), width = "100%", height = "800px"),
            shiny::actionButton(paste0("remove_res_", name), "Remove", class = "btn-danger")
          )
        } else if (inherits(temp, "summaryModel")) {
          htmltools::div(
            class = "var-box-output",
            htmltools::div(
              class = "var-box-name",
              name
            ),
            shiny::plotOutput(paste0("res_plot_", name)),
            DT::DTOutput(paste0("res_summary_", name)),
            DT::DTOutput(paste0("res_information_criterion_", name)),
            shiny::actionButton(paste0("remove_res_", name), "Remove", class = "btn-danger")
          )
        } else {
          htmltools::div(
            class = "var-box-output",
            htmltools::div(
              class = "var-box-name",
              name
            ),
            shiny::verbatimTextOutput(paste0("res_", name)),
            shiny::actionButton(paste0("remove_res_", name), "Remove", class = "btn-danger")
          )
        }
      })
      if (MethodState$method == "Default") {
        download_stuff <- htmltools::div(
          class = "var-box-output",
          htmltools::h3(htmltools::strong("Results")),
          htmltools::p("The following list contains the results"),
          shiny::actionButton("download", "Save"),
          shiny::textInput("user_filename", "Set filename", value = "")
        )
        do.call(htmltools::tagList, list(download_stuff, res_ui_list))
      } else if (MethodState$method == "DoseResponse") {
        download_stuff <- htmltools::div(
          class = "var-box-output",
          htmltools::h3(htmltools::strong("Results")),
          htmltools::p("The following list contains the results"),
          shiny::actionButton("download", "Save")
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
        shiny::observeEvent(res[[name]], {
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

    # Observe open formula editor
    # ----------------------------------------------------------
    output$open_formula_editor_main <- shiny::renderUI({
      if (input$conditionedPanels == "DataWrangling") {
        return()
      }
      htmltools::div(
        class = "boxed-output",
        shiny::actionButton("open_formula_editor",
          "Open formula editor",
          title = "Open the formula editor to create or modify a formula"
        )
      )
    })
    shiny::observeEvent(input[["open_formula_editor"]], {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      shiny::showModal(shiny::modalDialog(
        title = htmltools::div(style = "display: flex; align-items: center; justify-content: space-between;",
          htmltools::span("FormulaEditor"),
          shiny::actionButton("FO-formula_docu", label = NULL, icon = shiny::icon("question-circle"))
        ),
        FormulaEditorUI("FO"),
        easyClose = TRUE,
        size = "l",
        footer = htmltools::tagList(
          shiny::modalButton("Close")
        )
      ))
    })
    # display current formula
    output[["formulaUI"]] <- shiny::renderUI({
      if (input$conditionedPanels == "DataWrangling") {
        return()
      } else {
        shiny::renderUI({
          if (inherits(DataModelState$formula, "LinearFormula")) {
            htmltools::div(
              class = "var-box-output",
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  htmltools::p("Linear model"),
                  deparse(DataModelState$formula@formula),
                )
                # shiny::column(
                #   width = 6,
                #   shiny::actionButton(
                #     "open_predictor_editor",
                #     "Open the prediction editor",
                #     title = "Open the prediction editor to apply a model to (new) data"
                #   )
                # )
              )
            )
          } else if (inherits(DataModelState$formula, "GeneralisedLinearFormula")) {
            htmltools::div(
              class = "var-box-output",
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  htmltools::p("Generalised Linear Model"),
                  deparse(DataModelState$formula@formula),
                  htmltools::br(),
                  paste0("Family: ", deparse(DataModelState$formula@family)),
                  htmltools::br(),
                  paste0("Link fct.: ", deparse(DataModelState$formula@link_fct))
                )
                # shiny::column(
                #   width = 6,
                #   shiny::actionButton(
                #     "open_predictor_editor",
                #     "Open the prediction editor",
                #     title = "Open the prediction editor to apply a model to (new) data"
                #   )
                # )
              )
            )
          } else if (inherits(DataModelState$formula, "OptimFormula")) {
            htmltools::div(
              class = "var-box-output",
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  htmltools::p("Optimization Model"),
                  deparse(DataModelState$formula@formula),
                  htmltools::br(),
                  paste0("Optimization method: ", DataModelState$formula@method),
                  htmltools::br(),
                  paste0("Lower boundary: ", deparse(DataModelState$formula@lower)),
                  paste0("Upper boundary: ", deparse(DataModelState$formula@upper)),
                  htmltools::br(),
                  paste0("Seed: ", deparse(DataModelState$formula@seed))
                )
                # shiny::column(
                #   width = 6,
                #   shiny::actionButton(
                #     "open_predictor_editor",
                #     "Open the prediction editor",
                #     title = "Open the prediction editor to apply a model to (new) data"
                #   )
                # )
              )
            )
          } else {
            ""
          }
        })
      }
    })

    # Render split by group
    # ----------------------------------------------------------
    output[["open_split_by_groupUI"]] <- shiny::renderUI({
      if (input$conditionedPanels == "DataWrangling") {
        return()
      }
      if (is.null(DataModelState$backup_df)) {
        return(
          htmltools::div(
            class = "boxed-output",
            shiny::actionButton("open_split_by_group",
              "Open the split by group functionality",
              title = "Open the split by group helper window"
            ))
        )
      } else {
        return(
          shiny::actionButton("remove_filter",
            "Remove the filter from the dataset",
            title = "remove the filter of the dataset",
            disabled = is.null(DataModelState$backup_df) || !is.data.frame(DataModelState$backup_df)
          )
        )
      }
    })
    shiny::observeEvent(input[["open_split_by_group"]], {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      shiny::showModal(shiny::modalDialog(
        title = "SplitByGroup",
        SplitByGroupUI("SG"),
        easyClose = TRUE,
        size = "l",
        footer = NULL
      ))
    })
    shiny::observe({
      output$applied_filter <- shiny::renderText(NULL)
      shiny::req(!is.null(DataModelState$filter_col))
      shiny::req(!is.null(DataModelState$filter_group))
      output$applied_filter <- shiny::renderText({
        paste(
          "The dataset is splitted by the variable(s): [",
          paste(DataModelState$filter_col, collapse = ", "),
          "] group(s) are set to: [",
          paste(DataModelState$filter_group, collapse = ", "),
          "]"
        )
      })
    })
    # Remove filter
    shiny::observeEvent(input[["remove_filter"]], {
      e <- try({
        rf <- get_remove_filter()$new()
        rf$validate()
        rf$eval(ResultsState, DataModelState)
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        print_err(err)
      }
    })

    # Download
    # ----------------------------------------------------------
    shiny::observeEvent(input$download, {
      if (MethodState$method == "Default") {
        if (!env_utils$is_valid_filename(input$user_filename)) {
          shinyjs::runjs("document.getElementById('user_filename').focus();")
          print_noti(
            env_utils$why_filename_invalid(input$user_filename)
          )
        }
        print_req(
          env_utils$is_valid_filename(input$user_filename),
          "Defined filename is not valid"
        )
      }
      print_req(length(ResultsState$all_data) > 0, "No results to save")
      l <- ResultsState$all_data
      history_json <- jsonlite::toJSON(ResultsState$history, pretty = TRUE, auto_unbox = TRUE)
      history_table <- history_to_table(ResultsState$history)
      l_history <- c("HistoryTable" = history_table)
      l <- c(l_history, l)
      l <- c(l, "HistoryJSON" = history_json)
      # Sent data to ChemotionELN
      # -----------------------------------------------------------------------
      if (Sys.getenv("RUN_MODE") == "SERVER") {
        if (!requireNamespace("COMELN", quietly = TRUE)) {
          shiny::validate(shiny::need(FALSE,
            paste(
              "Feature requires the 'COMELN' package.",
              "Install it to enable uploads",
              'For GitHub: remotes::install_github("ComPlat/OpenStats", subdir = "comeln")'
            )
          ))
          return(NULL)
        }
        uploader <- getExportedValue("COMELN", "upload")

        if (MethodState$method == "Default") {
          print_req(
            env_utils$check_filename_for_server(input$user_filename),
            "Defined filename does not have xlsx as extension"
          )
          excelFile <- env_utils$create_excel_file(l)
          uploader(session, excelFile, new_name = input$user_filename)
          unlink(excelFile)
        }
        else if (MethodState$method == "DoseResponse") {
          jsonFile <- try(env_import_dose_response$dose_response_to_json(MethodState, ResultsState$all_data))
          excelFile <- try(env_utils$create_excel_file(l))
          if (!is.character(jsonFile) || length(jsonFile) != 1L || !file.exists(jsonFile)) {
            print_err("Cannot convert results to json")
          }
          if (!is.character(excelFile) || length(excelFile) != 1L || !file.exists(excelFile)) {
            print_err("Cannot store the results in an excelFile")
          }
          fn <- tempfile(fileext = ".zip")
          utils::zip(fn, c(jsonFile, excelFile))
          if (!file.exists(fn) || file.info(fn)$size <= 0) {
            print_err("Could not create the zip archive storing the final results")
          }
          uploader(session, fn, new_name = "result.zip")
          unlink(fn)
          unlink(excelFile)
          unlink(jsonFile)
        }
      }
      # Running OpenStats locally
      # -----------------------------------------------------------------------
      else if (Sys.getenv("RUN_MODE") == "LOCAL") {
        print_req(
          env_utils$check_filename_for_server(input$user_filename) || env_utils$check_filename_for_serverless(input$user_filename),
          "Defined filename does not have xlsx or zip as extension"
        )
        ex <- env_utils$extract_extension(input$user_filename)
        if (ex == "xlsx") {
          excelFile <- env_utils$create_excel_file(l)
          file_content <- readBin(excelFile, "raw", file.info(excelFile)$size)
          file_content_base64 <- jsonlite::base64_enc(file_content)
          session$sendCustomMessage(
            type = "downloadExcel",
            list(
              fileContent = file_content_base64,
              filename = input$user_filename
            )
          )
          unlink(excelFile)
        } else {
          string_and_names <- env_utils$create_js_string(l)
          session$sendCustomMessage(
            type = "downloadZip",
            list(
              numberOfResults = length(string_and_names[[1]]),
              FileContent = string_and_names[[1]],
              Filename = input$user_filename,
              ResultNames = string_and_names[[2]]
            )
          )
        }
      } else {
        print_req(
          env_utils$check_filename_for_serverless(input$user_filename),
          "Defined filename does not have zip as extension"
        )
        string_and_names <- env_utils$create_js_string(l)
        session$sendCustomMessage(
          type = "downloadZip",
          list(
            numberOfResults = length(string_and_names[[1]]),
            FileContent = string_and_names[[1]],
            Filename = input$user_filename,
            ResultNames = string_and_names[[2]]
          )
        )
      }
    })
  }
  return(list(ui = ui, server = server))
}
