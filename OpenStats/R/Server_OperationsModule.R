OperationEditorServer <- function(id, DataModelState, ResultsState, DataWranglingState) {

  refresh <- shiny::reactiveVal(0)
  expr_builder_server(
    id, DataWranglingState, refresh = refresh, check_variables = FALSE,
    operator_set = data_wrangling_operator_set(), docs = data_wrangling_docs(),
    specs = data_wrangling_specs(),
    df_doc = paste0(
      "The whole dataset. Type its name to use the entire table, ",
      "or use a column name to work with a single column."
    )
  )

  shiny::moduleServer(id, function(input, output, session) {

    output$builder <- shiny::renderUI({
      shiny::req(DataWranglingState$df)
      intermediate <- setdiff(names(DataWranglingState$intermediate_vars), DataWranglingState$df_name)
      vars <- unique(c(
        DataWranglingState$df_name,
        names(DataWranglingState$df),
        intermediate
      ))
      data_wrangling_expr_builder_ui(
        id, data_wrangling_palette(), vars,
        df_name = DataWranglingState$df_name, intermediate = intermediate
      )
    })
    # Render even when the DataWrangling tab is hidden, so the input binding
    # exists when expr_builder_server pushes its payload (data is loaded on the
    # Data tab, while this output would otherwise be suspended).
    shiny::outputOptions(output, "builder", suspendWhenHidden = FALSE)
    shiny::observeEvent(
      list(DataWranglingState$df, DataWranglingState$intermediate_vars),
      refresh(refresh() + 1)
    )

    # Data
    shiny::observe({
      shiny::req(DataModelState$active_df_name)
      shiny::req(ResultsState$all_data[[DataModelState$active_df_name]])
      df <- ResultsState$all_data[[DataModelState$active_df_name]]
      shiny::req(is.data.frame(df))
      DataWranglingState$df <- df

      DataWranglingState$df_name <- env_utils$create_df_name(DataWranglingState$df_name, names(df))
      DataWranglingState$intermediate_vars[[DataWranglingState$df_name]] <- df
      output$head <- shiny::renderUI({
        col_info <- sapply(df, function(col) class(col)[1]) |>
          t() |>
          as.data.frame()
        names(col_info) <- names(df)
        htmltools::div(
          class = "var-box-output",
          htmltools::div(
            title = "This displays the current types for each column",
            shiny::renderTable({
              col_info
            })
          ),
          shiny::renderTable({
            utils::head(DataWranglingState$df)
          })
        )
      })
    })

    output[["column_apply"]] <- shiny::renderUI({
      shiny::req(DataWranglingState$df_name)
      shiny::req(DataWranglingState$df)

      tooltip <- "Apply a function to each group based on the selected column(s). Moreover, apply is only used for intermediate variables."
      cols <- names(DataWranglingState$df)
      htmltools::div(
        shiny::tags$label(
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        shiny::selectInput(
          inputId = shiny::NS(id, "group_apply"),
          label = "Subset dataset by:",
          choices = cols,
          selected = NULL,
          multiple = TRUE
        )
      )
    })

    # Observe intermediate results
    output$intermediate_results <- shiny::renderUI({
      iv_list <- DataWranglingState$intermediate_vars
      if (length(iv_list) == 1) return()
      iv_list <- iv_list[names(iv_list) != DataWranglingState$df_name]
      iv_ui <- lapply(names(iv_list), function(name) {
        htmltools::div(
          class = "var-box-output",
          htmltools::strong(name),
          shiny::verbatimTextOutput(shiny::NS(id, paste0("iv_", name))),
          shiny::actionButton(shiny::NS(id, paste0("remove_iv_", name)), "Remove", class = "btn-danger")
        )
      })
      do.call(htmltools::tagList, iv_ui)
    })

    # Show intermediate variables
    shiny::observe({
      iv_list <- DataWranglingState$intermediate_vars
      lapply(names(iv_list), function(name) {
        shiny::observeEvent(DataWranglingState$intermediate_vars[[name]], {
          output[[paste0("iv_", name)]] <- shiny::renderPrint({
            DataWranglingState$intermediate_vars[[name]]
          })
        }, ignoreInit = TRUE)
      })
    })

    # Observe remove of intermediate variables
    shiny::observe({
      iv_list <- DataWranglingState$intermediate_vars
      for (name in names(iv_list)) {
        output[[paste0("iv_", name)]] <- shiny::renderPrint({
          iv_list[[name]]
        })
        shiny::observeEvent(input[[paste0("remove_iv_", name)]], {
          e <- try({
            riv = get_remove_intermediate_var()$new(name)
            riv$validate()
            riv$eval(ResultsState, DataWranglingState)
          }, silent = TRUE)
          if (inherits(e, "try-error")) {
            return()
          }
        }, ignoreInit = TRUE)
      }
    })

    # Run operation and store in intermediate result
    shiny::observeEvent(input$run_op_intermediate, {
      print_req(is.data.frame(DataWranglingState$df), "The dataset is missing")
      if (input$iv == "") {
        shinyjs::runjs("document.getElementById('OP-iv').focus();")
      }
      background <- !getOption("OpenStats.background", TRUE)
      string <- if (background) DataWranglingState$code_string else input$expr$text
      civ <- get_create_intermediate_var()$new(
        df = DataWranglingState$df, df_name = DataWranglingState$df_name,
        intermediate_vars = DataWranglingState$intermediate_vars,
        operation = string,
        name = input$iv,
        group_apply = input$group_apply
      )
      e <- try({
        civ$validate()
        civ$eval(ResultsState, DataWranglingState)
      }, silent = TRUE)
      if (inherits(e, "try-error")) {
        return()
      }
      shiny::exportTestValues(
        iv_list = DataWranglingState$intermediate_vars
      )
    })

    # Run operation and append to df
    shiny::observeEvent(input$run_op, {
      print_req(is.data.frame(DataWranglingState$df), "The dataset is missing")
      if (input$nc== "") {
        shinyjs::runjs("document.getElementById('OP-nc').focus();")
      }
      background <- !getOption("OpenStats.background", TRUE)
      string <- if (background) DataWranglingState$code_string else input$expr$text
      cnc <- get_create_new_col()$new(
        df = DataWranglingState$df, df_name = DataWranglingState$df_name,
        intermediate_vars = DataWranglingState$intermediate_vars,
        operation = string,
        name = input$nc
      )
      e <- try({
        cnc$validate()
        cnc$eval(ResultsState, DataWranglingState, DataModelState)
      }, silent = TRUE)
      if (inherits(e, "try-error")) {
        return()
      }
      output$head <- shiny::renderTable(utils::head(DataWranglingState$df, 10))
    })

  })
} 
