js_scripts <- function() {
  if (Sys.getenv("RUN_MODE") == "BROWSER") {
    tagList(
      includeScript("www/FileSaver.min.js"),
      includeScript("www/html2canvas.min.js"),
      includeScript("www/jszip.min.js"),
      includeScript("www/download.js")
    )
  } else {
    tagList(
      includeScript(system.file("www/FileSaver.min.js", package = "OpenStats")),
      includeScript(system.file("www/html2canvas.min.js", package = "OpenStats")),
      includeScript(system.file("www/jszip.min.js", package = "OpenStats")),
      includeScript(system.file("www/download.js", package = "OpenStats"))
    )
  }
}

upload_ui_field <- function() {
  if (Sys.getenv("RUN_MODE") != "SERVER") {
    res <- conditionalPanel(
      condition = "input.conditionedPanels == 'Data'",
      fileInput("file", "Choose CSV File",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv",
          ".xlsx"
        )
      )
    )
    return(res)
  }
}

app <- function() {
  js <- js_scripts()
  uploadUIField <- upload_ui_field()
  ui <- fluidPage(
    useShinyjs(),
    js,
    # includeScript(system.file("www/FileSaver.min.js", package = "OpenStats")),
    # includeScript(system.file("www/html2canvas.min.js", package = "OpenStats")),
    # includeScript(system.file("www/jszip.min.js", package = "OpenStats")),
    # includeScript(system.file("www/download.js", package = "OpenStats")),
    tags$head(
      # tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.7.1/jszip.min.js"),
      # tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"),
      # tags$script(src tags$script(src = "js/jszip.min.js"),
      tags$style(HTML("
        .boxed-output {
        border: 2px solid #900C3F;
        padding: 10px;
        border-radius: 5px;
        margin-top: 10px;
        }
        .add-button {
        position: relative;
        padding-right: 20px;
        }
        .add-button::after {
        position: absolute;
        top: 1.1px;
        right: 5px;
        font-size: 16px;
        font-weight: bold;
        color: #900C3F;
        width: 15px;
        height: 15px;
        display: flex;
        justify-content: center;
        align-items: center;
        }
        .df-button {
        background-color: #d5f5e3;  /* light green */
        border: 1px solid #a2d9b1;
        color: #000;
        margin: 3px;
        }
        .colnames-button {
        background-color: #d0eaff;  /* light blue */
        border: 1px solid #90c5f0;
        color: #000;
        margin: 3px;
        }
        .model {
        background-color: #f8f9fa;
        padding: 15px;
        border: 2px solid #c8c8c8;
        border-radius: 5px;
        margin-top: 10px;
        }
        .title {
        font-size: 14px;
        font-weight: bold;
        margin-bottom: 10px;
        color: #333;
        }
        .create_button{
        background-color: #04AA6D; /* Green */
        border: none;
        color: black;
        padding: 15px 32px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 16px;
        }
        .var-output {
        border: 2px solid #900C3F;
        padding: 10px;
        border-radius: 5px;
        margin-top: 10px;
        display: inline-block;
        width: auto;
        }
        .var-box-output {
        border: 2px solid #900C3F;
        padding: 10px;
        border-radius: 5px;
        margin-top: 10px;
        }
        .var-box-name {
        font-size: 15px;
        font-weight: 600;
        color: #1f2937;
        margin-bottom: 8px;
        margin-top: 4px;
        }
        .info-box {
        background-color: #e6f2ff; /* light blue */
        border: 1px solid #99ccff; /* soft blue border */
        border-radius: 8px;
        padding: 16px;
        margin-top: 12px;
        font-size: 16px;
        color: #003366; /* dark blue text for contrast */
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
        }
        .info-box h3 {
        margin: 0;
        font-weight: bold;
        }
        .nav-tabs > li > a {
        text-decoration: none;
        color: #900C3F;
        font-weight: bold;
        margin-right: 15px;
        }
        .nav-tabs > li > a:hover {
        text-decoration: underline;
        color: #333;
        }
        "))
    ),
    sidebarLayout(
      sidebarPanel(
        div(
          style = "display: flex; align-items: center; gap: 6px;",
          actionButton(
            "docu",
            label = NULL,
            icon = icon("question-circle")
          ),
          uiOutput("running_status") # TODO: when an error ocurred it does not vanish
        ),
        uiOutput("open_formula_editor_main"),
        uiOutput("formulaUI"),
        br(),
        uiOutput("open_split_by_groupUI"),
        uiOutput("data_splitted"),
        verbatimTextOutput("applied_filter"),
        br(),
        uiOutput("active_df"),
        br(),
        div(
          conditionalPanel(
            condition = "input.conditionedPanels == 'Data'",
            uploadUIField,
            tags$hr()
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'DataWrangling'",
            OperatorEditorSidebar("OP")
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'Visualisation'",
            visSidebarUI("VIS")
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'Assumption'",
            assSidebarUI("ASS")
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'Correlation'",
            corrSidebarUI("CORR")
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'Tests'",
            testsSidebarUI("TESTS")
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'Dose Response analysis'",
            DoseResponseSidebarUI("DOSERESPONSE")
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'History'",
            HistorySidebarUI("HISTORY")
          )
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Data",
            DTOutput("df")
          ),
          tabPanel(
            "DataWrangling",
            OperatorEditorUI("OP")
          ),
          tabPanel(
            "Visualisation",
            visUI("VIS")
          ),
          tabPanel(
            "Assumption",
            assUI("ASS")
          ),
          tabPanel(
            "Correlation",
            corrUI("CORR")
          ),
          tabPanel(
            "Tests",
            testsUI("TESTS")
          ),
          tabPanel(
            "Dose Response analysis",
            DoseResponseUI("DOSERESPONSE")
          ),
          tabPanel(
            "History",
            HistoryEditorUI("HISTORY")
          ),
          id = "conditionedPanels"
        ),
        uiOutput("Results")
      )
    )
  )

  server <- function(input, output, session) {
    # Create background process instance
    bgp <- bg_process_V1_2$new()

    # States
    DataModelState <- reactiveValues(
      df = NULL, formula = NULL,
      backup_df = NULL, filter_col = NULL, filter_group = NULL,
      active_df_name = NULL
    )

    ResultsState <- reactiveValues(
      curr_data = NULL, curr_name = NULL,
      all_data = list(), all_names = list(),
      history = list(),
      counter = 0,
      bgp = bgp
    )

    DataWranglingState <- reactiveValues(
      df = NULL, df_name = "df",
      current_page = 1, total_pages = 1,
      counter_id = 0,
      intermediate_vars = list()
    )
    bgp$init(ResultsState, DataModelState, DataWranglingState) # NOTE: creates the polling observer

    # React to press cancel
    observeEvent(input$confirm_stop, {
      ResultsState$bgp$cancel()
    })

    # Show running_status
    output$running_status <- renderUI({
      invalidateLater(750)
      status <- ResultsState$bgp$running_status
      if ((status == "Running...") && !ResultsState$bgp$cancel_clicked) {
        return(
          div(
            style = "display: flex; align-items: center; gap: 6px;",
            tags$p(status, style = "margin: 0;"),
            icon("spinner", class = "fa-spin", style = "color: #007BFF;"),
            actionButton("confirm_stop", "Stop process", class = "btn-danger")
          )
        )
      } else {
        NULL
      }
    })

    # docu
    observeEvent(input[["docu"]], {
      path_list <- get_docu(input$conditionedPanels)
      if (length(path_list) == 4) {
        path1 <- path_list[[1]]
        path2 <- path_list[[2]]
        plot_path <- path_list[[3]]
        title <- path_list[[4]]
        showModal(modalDialog(
          title = title,
          includeHTML(path1),
          br(),
          renderImage(
            {
              list(
                src = plot_path,
                contentType = "image/jpg",
                width = 650,
                height = 500,
                alt = "Basic Plot"
              )
            },
            deleteFile = FALSE
          ),
          br(),
          br(),
          br(),
          br(),
          br(),
          includeHTML(path2),
          easyClose = TRUE,
          footer = NULL,
          size = "l"
        ))
      } else {
        path <- path_list[[1]]
        title <- path_list[[2]]
        showModal(modalDialog(
          title = title,
          includeHTML(path),
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    # docu formula editor
    observeEvent(input[["FO-formula_docu"]], {
      type <- input[["FO-model_type"]]
      path_list <- get_docu(paste0(type, "Formula"))
      showModal(modalDialog(
        title = path_list[[2]],
        includeHTML(path_list[[1]]),
        easyClose = TRUE,
        footer = NULL,
        size = "l"
      ))
    })
    # docu split by group
    observeEvent(input[["SG-split_docu"]], {
      path_list <- get_docu("Split")
      showModal(modalDialog(
        title = path_list[[2]],
        includeHTML(path_list[[1]]),
        easyClose = TRUE,
        footer = NULL,
        size = "l"
      ))
    })

    download_file <- reactive({
      file <- COMELN::download(session, "/home/shiny/results")
      readData(file, DataModelState, ResultsState)
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
      req(is.data.frame(DataModelState$df))
    })
    if (Sys.getenv("RUN_MODE") == "SERVER") {
      observe({
        req(is.null(DataModelState$df))
        e <- try(download_file())
        if (inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
          print_err(err)
        }
      })
    } else {
      observeEvent(input$file, {
        e <- try(readData(input$file$datapath, DataModelState, ResultsState))
        if (inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
          print_err(err)
        }
      })
    }
    output$df <- renderDT({
      req(DataModelState$df)
      datatable(DataModelState$df, options = list(pageLength = 10))
    })
    observe({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      if (length(ResultsState$history) == 0) {
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(type = "Version", Nr = get_current_version())
      }
      output$df <- renderDT(
        datatable(DataModelState$df, options = list(pageLength = 10))
      )
    })

    # Observe tables
    output[["active_df"]] <- renderUI({
      if (input$conditionedPanels == "DataWrangling") {
        return()
      }
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      if (length(ResultsState$all_data) == 0) {
        return(NULL)
      }
      table_indices <- which(sapply(ResultsState$all_data, is.data.frame))
      names <- names(ResultsState$all_data)
      names <- names[table_indices]
      tooltip <- "Select the active dataset (the dataset with which you can work)"
      div(
        class = "boxed-output",
        tags$label(
          "active dataset",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = "tables-dropdown",
          label = "active dataset",
          choices = names,
          selected = DataModelState$active_df_name
        )
      )
    })

    observeEvent(input[["tables-dropdown"]], {
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      req(input[["tables-dropdown"]])
      sat <- set_active_table_V1_2$new(input[["tables-dropdown"]])
      sat$eval(ResultsState, DataModelState)
    })

    OperationEditorServer("OP", DataModelState, ResultsState, DataWranglingState)
    corrServer("CORR", DataModelState, ResultsState)
    visServer("VIS", DataModelState, ResultsState)
    assServer("ASS", DataModelState, ResultsState)
    testsServer("TESTS", DataModelState, ResultsState)
    DoseResponseServer("DOSERESPONSE", DataModelState, ResultsState)
    FormulaEditorServer("FO", DataModelState, ResultsState)
    SplitByGroupServer("SG", DataModelState, ResultsState)
    HistoryEditorServer("HISTORY", DataModelState, ResultsState, DataWranglingState)

    # Render results list
    output$Results <- renderUI({
      if (input$conditionedPanels == "DataWrangling") {
        return(
          div(
            class = "var-box-output",
            h3(strong("The results are displayed in the other tabs"))
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
          div(
            class = "var-box-output",
            div(
              class = "var-box-name",
              name
            ),
            verbatimTextOutput(paste0("res_", name)),
            actionButton(paste0("remove_res_", name), "Remove", class = "btn-danger")
          )
        } else if (is.data.frame(temp)) {
          div(
            class = "var-box-output",
            div(
              class = "var-box-name",
              name
            ),
            DTOutput(paste0("res_", name)),
            actionButton(paste0("remove_res_", name), "Remove", class = "btn-danger")
          )
        } else if (inherits(temp, "plot")) {
          div(
            class = "var-box-output",
            div(
              class = "var-box-name",
              name
            ),
            plotOutput(paste0("res_", name), width = "100%", height = "800px"),
            actionButton(paste0("remove_res_", name), "Remove", class = "btn-danger")
          )
        } else if (inherits(temp, "summaryModel")) {
          div(
            class = "var-box-output",
            div(
              class = "var-box-name",
              name
            ),
            plotOutput(paste0("res_plot_", name)),
            DTOutput(paste0("res_summary_", name)),
            DTOutput(paste0("res_information_criterion_", name)),
            actionButton(paste0("remove_res_", name), "Remove", class = "btn-danger")
          )
        } else {
          div(
            class = "var-box-output",
            div(
              class = "var-box-name",
              name
            ),
            verbatimTextOutput(paste0("res_", name)),
            actionButton(paste0("remove_res_", name), "Remove", class = "btn-danger")
          )
        }
      })
      download_stuff <- div(
        class = "var-box-output",
        h3(strong("Results")),
        p("The following list contains the results"),
        actionButton("download", "Save"),
        textInput("user_filename", "Set filename", value = "")
      )
      do.call(tagList, list(download_stuff, res_ui_list))
    })

    # Show results
    observe({
      if (length(ResultsState$all_data) == 0) {
        return()
      }
      res <- ResultsState$all_data
      res_ui_list <- lapply(names(res), function(name) {
        observeEvent(res[[name]], {
          temp <- res[[name]]
          if (is.vector(temp)) {
            output[[paste0("res_", name)]] <- renderPrint(temp)
          } else if (is.data.frame(temp)) {
            output[[paste0("res_", name)]] <- renderDT(temp)
          } else if (inherits(temp, "plot")) {
            output[[paste0("res_", name)]] <- renderPlot(temp@p)
          } else if (inherits(temp, "doseResponse")) {
            message <- paste0(
              "Dose response analysis. (Outliers: ",
              paste0(temp@outlier_info, collapse = ";"),
              "). Too long to display",
              collapse = " "
            )
            output[[paste0("res_", name)]] <- renderPrint(message)
          } else if (inherits(temp, "summaryModel")) {
            output[[paste0("res_plot_", name)]] <- renderPlot(temp@p)
            output[[paste0("res_summary_", name)]] <- renderDT(temp@summary)
            output[[paste0("res_information_criterion_", name)]] <- renderDT(temp@information_criterions)
          } else {
            output[[paste0("res_", name)]] <- renderPrint(temp)
          }
        })
      })
      do.call(tagList, res_ui_list)
    })

    # Observe remove buttons
    observe({
      if (length(ResultsState$all_data) == 0) {
        return()
      }
      current_list <- ResultsState$all_data
      lapply(names(current_list), function(name) {
        observeEvent(input[[paste0("remove_res_", name)]],
          {
            e <- try({
              rr <- remove_result_V1_2$new(name)
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

    # Observe open formula editor
    output$open_formula_editor_main <- renderUI({
      if (input$conditionedPanels == "DataWrangling") {
        return()
      }
      div(
        class = "boxed-output",
        actionButton("open_formula_editor",
          "Open formula editor",
          title = "Open the formula editor to create or modify a formula"
        )
      )
    })
    observeEvent(input[["open_formula_editor"]], {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      showModal(modalDialog(
        title = div(style = "display: flex; align-items: center; justify-content: space-between;",
          span("FormulaEditor"),
          actionButton("FO-formula_docu", label = NULL, icon = icon("question-circle"))
        ),
        FormulaEditorUI("FO"),
        easyClose = TRUE,
        size = "l",
        footer = tagList(
          modalButton("Close")
        )
      ))
    })
    # display current formula
    output[["formulaUI"]] <- renderUI({
      if (input$conditionedPanels == "DataWrangling") {
        return()
      } else {
        renderUI({
          if (inherits(DataModelState$formula, "LinearFormula")) {
            div(
              class = "var-box-output",
              fluidRow(
                column(
                  width = 6,
                  p("Linear model"),
                  deparse(DataModelState$formula@formula),
                )
                # column(
                #   width = 6,
                #   actionButton(
                #     "open_predictor_editor",
                #     "Open the prediction editor",
                #     title = "Open the prediction editor to apply a model to (new) data"
                #   )
                # )
              )
            )
          } else if (inherits(DataModelState$formula, "GeneralisedLinearFormula")) {
            div(
              class = "var-box-output",
              fluidRow(
                column(
                  width = 6,
                  p("Generalised Linear Model"),
                  deparse(DataModelState$formula@formula),
                  br(),
                  paste0("Family: ", deparse(DataModelState$formula@family)),
                  br(),
                  paste0("Link fct.: ", deparse(DataModelState$formula@link_fct))
                )
                # column(
                #   width = 6,
                #   actionButton(
                #     "open_predictor_editor",
                #     "Open the prediction editor",
                #     title = "Open the prediction editor to apply a model to (new) data"
                #   )
                # )
              )
            )
          } else if (inherits(DataModelState$formula, "OptimFormula")) {
            div(
              class = "var-box-output",
              fluidRow(
                column(
                  width = 6,
                  p("Optimization Model"),
                  deparse(DataModelState$formula@formula),
                  br(),
                  paste0("Lower boundary: ", deparse(DataModelState$formula@lower)),
                  paste0("Upper boundary: ", deparse(DataModelState$formula@upper)),
                  br(),
                  paste0("Seed: ", deparse(DataModelState$formula@seed))
                )
                # column(
                #   width = 6,
                #   actionButton(
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
    output[["open_split_by_groupUI"]] <- renderUI({
      if (input$conditionedPanels == "DataWrangling") {
        return()
      }
      if (is.null(DataModelState$backup_df)) {
        return(
          div(
            class = "boxed-output",
            actionButton("open_split_by_group",
              "Open the split by group functionality",
              title = "Open the split by group helper window"
            ))
        )
      } else {
        return(
          actionButton("remove_filter_V1_2",
            "Remove the filter from the dataset",
            title = "remove the filter of the dataset",
            disabled = is.null(DataModelState$backup_df) || !is.data.frame(DataModelState$backup_df)
          )
        )
      }
    })
    observeEvent(input[["open_split_by_group"]], {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      showModal(modalDialog(
        title = "SplitByGroup",
        SplitByGroupUI("SG"),
        easyClose = TRUE,
        size = "l",
        footer = NULL
      ))
    })
    observe({
      output$applied_filter <- renderText(NULL)
      req(!is.null(DataModelState$filter_col))
      req(!is.null(DataModelState$filter_group))
      output$applied_filter <- renderText({
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
    observeEvent(input[["remove_filter_V1_2"]], {
      e <- try({
        rf <- remove_filter_V1_2$new()
        rf$validate()
        rf$eval(ResultsState, DataModelState)
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        print_err(err)
      }
    })

    # Download
    observeEvent(input$download, {
      if (!is_valid_filename(input$user_filename)) {
        runjs("document.getElementById('user_filename').focus();")
        print_noti(
          why_filename_invalid(input$user_filename)
        )
      }
      print_req(
        is_valid_filename(input$user_filename),
        "Defined filename is not valid"
      )
      print_req(length(ResultsState$all_data) > 0, "No results to save")
      l <- ResultsState$all_data
      history_json <- jsonlite::toJSON(ResultsState$history, pretty = TRUE, auto_unbox = TRUE)
      history_table <- history_to_table(ResultsState$history)
      l_history <- c("HistoryTable" = history_table)
      l <- c(l_history, l)
      l <- c(l, "HistoryJSON" = history_json)
      if (Sys.getenv("RUN_MODE") == "SERVER") {
        print_req(
          check_filename_for_server(input$user_filename),
          "Defined filename does not have xlsx as extension"
        )
        excelFile <- createExcelFile(l)
        upload(session, excelFile, new_name = input$user_filename)
      } else if (Sys.getenv("RUN_MODE") == "LOCAL") {
        print_req(
          check_filename_for_server(input$user_filename) || check_filename_for_serverless(input$user_filename),
          "Defined filename does not have xlsx or zip as extension"
        )
        ex <- extract_extension(input$user_filename)
        if (ex == "xlsx") {
          excelFile <- createExcelFile(l)
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
          string_and_names <- createJSString(l)
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
          check_filename_for_serverless(input$user_filename),
          "Defined filename does not have zip as extension"
        )
        string_and_names <- createJSString(l)
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
