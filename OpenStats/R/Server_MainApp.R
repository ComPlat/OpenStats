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

        shinyjs::disable("ASS-DiagnosticPlot")

        shinyjs::disable("TESTS-PermANOVATest")
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
    show_docu(input, DataModelState)

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
    output[["summaryUI"]] <- shiny::renderUI({
      shiny::req(DataModelState$df)

      tooltip <- "Choose grouping variables"
      cols <- names(DataModelState$df)
      preselected <- NULL
      if ("group" %in% cols) {
        preselected <- which(cols == "group")
      }
      fcts <- c("Mean", "Median", "SD")
      htmltools::div(
        shiny::actionButton(
          "docu_summary_dataset",
          label = NULL,
          icon = shiny::icon("question-circle")
        ),
        shiny::tags$label(
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        shiny::selectInput(
          inputId = "column_by",
          label = "Variables to group your data",
          choices = cols,
          selected = cols[preselected],
          multiple = TRUE
        ),
        shiny::selectInput(
          inputId = "for_which",
          label = "Which Variables to summarize",
          choices = cols,
          selected = NULL,
          multiple = TRUE
        ),
        shiny::selectInput(
          inputId = "what_to_do",
          label = "Which functions should be applied",
          choices = fcts,
          selected = NULL,
          multiple = TRUE
        ),
        shiny::actionButton("summarize", "summarize", class = "add-button"),
        shiny::actionButton("summarize_plot", "summarize plot", class = "add-button"),
        class = "var-box-output"
      )
    })
    shiny::observeEvent(input$docu_summary_dataset, {
      path <- docu_path("summarize_data.html")
      title <- "Summarize dataset"
      shiny::showModal(shiny::modalDialog(
        title = title,
        shiny::includeHTML(path),
        easyClose = TRUE,
        footer = NULL,
        size = "l"
      ))
    })

    shiny::observeEvent(input$summarize, {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      background <- !getOption("OpenStats.background", TRUE)
      s <- get_summary()$new(DataModelState$df, input$column_by, input$for_which, input$what_to_do)
      e <- try({
        s$validate()
        s$eval(ResultsState)
      }, silent = TRUE)
      if (inherits(e, "try-error")) {
        print_err(attributes(e)$condition$message)
      }
    })
    shiny::observeEvent(input$summarize_plot, {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      background <- !getOption("OpenStats.background", TRUE)
      s <- get_summary_plot()$new(DataModelState$df, input$column_by, input$for_which)
      e <- try({
        s$validate()
        s$eval(ResultsState)
      }, silent = TRUE)
      if (inherits(e, "try-error")) {
        print_err(attributes(e)$condition$message)
      }
    })

    # Other servers
    # ----------------------------------------------------------
    downloadServer("DOWNLOAD", DataModelState, ResultsState, MethodState) # its called download because of the connection to the ELN. Using it locally is actully an upload
    OperationEditorServer("OP", DataModelState, ResultsState, DataWranglingState)
    corrServer("CORR", DataModelState, ResultsState)
    visServer("VIS", DataModelState, ResultsState)
    assServer("ASS", DataModelState, ResultsState)
    testsUIServer("TESTS", DataModelState, ResultsState)
    LinearParametricTestsUISidebarServer("TESTS", DataModelState, ResultsState)
    LinearNonParametricTestsUISidebarServer("TESTS", DataModelState, ResultsState)
    GeneralizedLinearTestsUISidebarServer("TESTS", DataModelState, ResultsState)
    LinearMixedTestsUISidebarServer("TESTS", DataModelState, ResultsState)
    testsServer("TESTS", DataModelState, ResultsState)
    DoseResponseServer("DOSERESPONSE", DataModelState, ResultsState, MethodState)
    FormulaEditorServer("FO", DataModelState, ResultsState)
    SplitByGroupServer("SG", DataModelState, ResultsState)
    HistoryEditorServer("HISTORY", DataModelState, ResultsState, DataWranglingState)
    ResultsListServer("RESULTS", DataModelState, ResultsState, MethodState, shiny::reactive(input$conditionedPanels))
    OpenFormulaServer("OPENFORMULA", DataModelState, ResultsState, MethodState, shiny::reactive(input$conditionedPanels))
    OpenSplitByGroupServer("OPENSPLITBYGROUP", DataModelState, ResultsState, MethodState, shiny::reactive(input$conditionedPanels))
    ExportResultsServer("RESULTS", DataModelState, ResultsState, MethodState)
  }
  return(list(ui = ui, server = server))
}
