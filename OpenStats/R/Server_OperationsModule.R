OperationEditorServer <- function(id, DataModelState, ResultsState, DataWranglingState) {

  moduleServer(id, function(input, output, session) {
    # Data
    observe({
      req(DataModelState$active_df_name)
      req(ResultsState$all_data[[DataModelState$active_df_name]])
      df <- ResultsState$all_data[[DataModelState$active_df_name]]
      req(is.data.frame(df))
      DataWranglingState$df <- df

      DataWranglingState$df_name <- env_utils$create_df_name(DataWranglingState$df_name, names(df))
      DataWranglingState$intermediate_vars[[DataWranglingState$df_name]] <- df
      output$head <- renderUI({
        col_info <- sapply(df, function(col) class(col)[1]) |>
          t() |>
          as.data.frame()
        names(col_info) <- names(df)
        div(
          class = "var-box-output",
          actionButton(
            paste0("OP-dataset_", DataWranglingState$df_name, "_", DataWranglingState$counter_id),
            label = "Dataset",
            title =
            "This is the dataset. Using the text df you can access the entire dataset. If you only want to work with one of the column you can use the respective column title. As a side note only the first 6 rows of the data table are shown.",
            class = "add-button"
          ),
          div(
            title = "This displays the current types for each column",
            renderTable({
              col_info
            })
          ),
          renderTable({
            head(DataWranglingState$df)
          })
        )
      })
    })

    update_code_text <- function(updated_text) {
      DataWranglingState$code_string <- updated_text
      updateTextAreaInput(session, "editable_code", value = updated_text)
    }

    # React to df button
    observe({
      req(DataWranglingState$df)
      var <- DataWranglingState$df_name
      observeEvent(input[[paste0("dataset_", var, "_", DataWranglingState$counter_id)]], {
        current_text <- input[["editable_code"]]
        updated_text <- paste(current_text, var, sep = " ")
        update_code_text(updated_text)
      })
    })

    # Create colnames button
    output[["colnames_list"]] <- renderUI({
      message <- check_data_wrangling(DataWranglingState)
      if (!is.null(message)) {
        return(
          info_div(message)
        )
      }
      DataWranglingState$df_name <- env_utils$create_df_name(DataWranglingState$df_name, names(DataModelState$df))
      colnames <- c(DataWranglingState$df_name, names(DataWranglingState$df))
      button_list <- lapply(colnames[1:length(colnames)], function(i) {
        if (i == DataWranglingState$df_name) {
          return(actionButton(
            inputId = paste0("OP-colnames_", i, "_", DataWranglingState$counter_id),
            label = paste(i),
            title = paste0("Click button if you want to use the entire dataset"),
            class = "add-button df-button"
          ))
        } else {
          return(actionButton(
            inputId = paste0("OP-colnames_", i, "_", DataWranglingState$counter_id),
            label = paste(i),
            title = paste0("Click button if you want to use the column: ", i),
            class = "add-button colnames-button"
          ))
        }
      })
      do.call(tagList, button_list)
    })

    # React to colnames buttons
    observe({
      req(DataWranglingState$df)
      DataWranglingState$df_name <- env_utils$create_df_name(DataWranglingState$df_name, names(DataModelState$df))
      colnames <- c(DataWranglingState$df_name, names(DataWranglingState$df))
      lapply(colnames, function(col) {
        observeEvent(input[[paste0("colnames_", col, "_", DataWranglingState$counter_id)]], {
          current_text <- input[["editable_code"]]
          updated_text <- paste(current_text, col, sep = " ")
        update_code_text(updated_text)
        })
      })
    })

    # Observe intermediate results
    output$intermediate_results <- renderUI({
      iv_list <- DataWranglingState$intermediate_vars
      if (length(iv_list) == 1) return()
      iv_list <- iv_list[names(iv_list) != DataWranglingState$df_name]
      iv_ui <- lapply(names(iv_list), function(name) {
        div(
          class = "var-box-output",
          actionButton(
            inputId = paste0("OP-intermediate_vars_", name, "_", DataWranglingState$counter_id),
            label = name,
            title = paste0("This is the variable ", name,
            ". You can use it by entering: ", name, " within the Operation text field."),
            class = "add-button"),
          verbatimTextOutput(NS(id, paste0("iv_", name))),
          actionButton(NS(id, paste0("remove_iv_", name)), "Remove", class = "btn-danger")
        )
      })
      do.call(tagList, iv_ui)
    })

    # Show intermediate variables
    observe({
      iv_list <- DataWranglingState$intermediate_vars
      lapply(names(iv_list), function(name) {
        observeEvent(DataWranglingState$intermediate_vars[[name]], {
          output[[paste0("iv_", name)]] <- renderPrint({
            DataWranglingState$intermediate_vars[[name]]
          })
        }, ignoreInit = TRUE)
      })
    })

    # Observe remove of intermediate variables
    observe({
      iv_list <- DataWranglingState$intermediate_vars
      for (name in names(iv_list)) {
        output[[paste0("iv_", name)]] <- renderPrint({
          iv_list[[name]]
        })
        observeEvent(input[[paste0("remove_iv_", name)]], {
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

    # React to intermediate variables buttons
    observe({
      req(DataWranglingState$df)
      req(length(DataWranglingState$intermediate_vars) >= 1)
      iv_list <- DataWranglingState$intermediate_vars
      iv_list <- iv_list[names(iv_list) != DataWranglingState$df_name]
      lapply(names(iv_list), function(var) {
        observeEvent(input[[paste0("intermediate_vars_", var, "_", DataWranglingState$counter_id)]], {
          current_text <- input[["editable_code"]]
          updated_text <- paste(current_text, var, sep = " ")
          update_code_text(updated_text)
        })
      })
    })

    # Run operation and store in intermediate result
    observeEvent(input$run_op_intermediate, {
      print_req(is.data.frame(DataWranglingState$df), "The dataset is missing")
      if (input$iv == "") {
        runjs("document.getElementById('OP-iv').focus();")
      }
      background <- !getOption("OpenStats.background", TRUE)
      string <- if (background) DataWranglingState$code_string else input$editable_code
      civ <- get_create_intermediate_var()$new(
        df = DataWranglingState$df, df_name = DataWranglingState$df_name,
        intermediate_vars = DataWranglingState$intermediate_vars,
        operation = string,
        name = input$iv
      )
      e <- try({
        civ$validate()
        civ$eval(ResultsState, DataWranglingState)
      }, silent = TRUE)
      if (inherits(e, "try-error")) {
        return()
      }
      exportTestValues(
        iv_list = DataWranglingState$intermediate_vars
      )
    })

    # Run operation and append to df
    observeEvent(input$run_op, {
      print_req(is.data.frame(DataWranglingState$df), "The dataset is missing")
      if (input$nc== "") {
        runjs("document.getElementById('OP-nc').focus();")
      }
      background <- !getOption("OpenStats.background", TRUE)
      string <- if (background) DataWranglingState$code_string else input$editable_code
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
      output$head <- renderTable(head(DataWranglingState$df, 10))
    })

    observeEvent(input$add, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "+", sep = " ")
      update_code_text(updated_text)
    })

    observeEvent(input$sub, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "-", sep = " ")
      update_code_text(updated_text)
    })

    observeEvent(input$mul, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "*", sep = " ")
      update_code_text(updated_text)
    })

    observeEvent(input$div, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "/", sep = " ")
      update_code_text(updated_text)
    })

    observeEvent(input$bracket_open, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$bracket_close, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, ")", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$comma, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, ",", sep = " ")
      update_code_text(updated_text)
    })

    observeEvent(input$log, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "log(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$log10, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "log10(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$sqrt, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "sqrt(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$exp, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "exp(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$exponent, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "^(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$sin, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "sin(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$cos, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "cos(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$tan, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "tan(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$sinh, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "sinh(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$cosh, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "cosh(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$tanh, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "tanh(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$asin, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "asin(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$acos, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "acos(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$atan, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "atan(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$abs, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "abs(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$ceil, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "ceiling(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$floor, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "floor(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$trunc, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "trunc(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$round, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "round(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$larger, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, ">", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$smaller, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "<", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$larger_eq, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, ">=", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$smaller_eq, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "<=", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$eq, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "==", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$not_eq, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "!=", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$paste, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "paste(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$paste0, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "paste0(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$tolower, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "tolower(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$toupper, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "toupper(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$get_elem, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "get_elem(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$get_rows, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "get_rows(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$get_cols, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "get_cols(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$mean, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Mean(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$sd, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "SD(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$median, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Median(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$sum, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Sum(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$min, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Min(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$max, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Max(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$c, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "C(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$seq, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Seq(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$df, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "DataFrame(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$as_char, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "as.char(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$as_int, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "as.int(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$as_real, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "as.real(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$as_fact, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "as.fact(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$dnorm, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Dnorm(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$pnorm, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Pnorm(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$qnorm, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Qnorm(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$rnorm, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Rnorm(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$dbinom, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Dbinom(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$pbinom, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Pbinom(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$qbinom, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Qbinom(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$rbinom, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Rbinom(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$dpois, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Dpois(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$ppois, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Ppois(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$rpois, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Rpois(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$dunif, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Dunif(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$punif, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Punif(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$qunif, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Qunif(", sep = " ")
      update_code_text(updated_text)
    })
    observeEvent(input$runif, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Runif(", sep = " ")
      update_code_text(updated_text)
    })

  })
} 
