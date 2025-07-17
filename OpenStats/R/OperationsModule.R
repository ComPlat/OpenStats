OperatorEditorSidebar <- function(id) {
  ui <- fluidPage(
    div(
      h3("Variables"),
      uiOutput(NS(id, "colnames_list")),
      class = "boxed-output"
    ),
    br(),
    div(
      h3("Arithmetic Operators"),
      actionButton(NS(id, "add"), "+", class = "add-button"),
      actionButton(NS(id, "sub"), "-", class = "add-button"),
      actionButton(NS(id, "mul"), "*", class = "add-button"),
      actionButton(NS(id, "div"), "/", class = "add-button"),
      actionButton(NS(id, "bracket_open"), "(", class = "add-button"),
      actionButton(NS(id, "bracket_close"), ")", class = "add-button"),
      actionButton(NS(id, "comma"), ",", class = "add-button"),
      class = "boxed-output"
    ),
    div(
      h3("Math Functions"),
      actionButton(NS(id, "log"), "log", class = "add-button", title = "logarithm to the base e"),
      actionButton(NS(id, "log10"), "log10", class = "add-button", title = "logarithm to the base 10"),
      actionButton(NS(id, "sqrt"), "sqrt", class = "add-button", title = "Square root"),
      actionButton(NS(id, "exp"), "exp", class = "add-button", title = "Exponential function (e^x) equate to exp(x)"),
      actionButton(NS(id, "exponent"), "^", class = "add-button"),
      actionButton(NS(id, "sin"), "sin", class = "add-button"),
      actionButton(NS(id, "cos"), "cos", class = "add-button"),
      actionButton(NS(id, "tan"), "tan", class = "add-button"),
      actionButton(NS(id, "sinh"), "sinh", class = "add-button"),
      actionButton(NS(id, "cosh"), "cosh", class = "add-button"),
      actionButton(NS(id, "tanh"), "tanh", class = "add-button"),
      actionButton(NS(id, "asin"), "asin", class = "add-button"),
      actionButton(NS(id, "acos"), "acos", class = "add-button"),
      actionButton(NS(id, "atan"), "atan", class = "add-button"),
      actionButton(NS(id, "abs"), "abs", class = "add-button", title = "absolute value |x| equivalent to abs(x)"),
      actionButton(NS(id, "ceil"), "ceiling", class = "add-button", title = "Round up to the nearest integer."),
      actionButton(NS(id, "floor"), "floor", class = "add-button", title = "Round down to the nearest integer (⌊x⌋)"),
      actionButton(NS(id, "trunc"), "trunc", class = "add-button", title = "Truncate decimal part, keeping only the integer part"),
      actionButton(NS(id, "round"), "round", class = "add-button", title = "Round to the nearest integer or specified precision"),
      class = "boxed-output"
    ),
    div(
      h3("Comparison Operators"),
      actionButton(NS(id, "larger"), ">", class = "add-button", title = "Greater than (e.g., 5 > 3 results in TRUE, '5 is greater than 3')"),
      actionButton(NS(id, "smaller"), "<", class = "add-button", title = "Less than (e.g., 3 < 5 results in TRUE, '3 is less than 5')"),
      actionButton(NS(id, "larger_eq"), ">=", class = "add-button", title = "Greater than or equal to (e.g., 5 >= 5 results in TRUE, '5 is greater than or equal to 5')"),
      actionButton(NS(id, "smaller_eq"), "<=", class = "add-button", title = "Less than or equal to (e.g., 3 <= 5 results in TRUE, '3 is less than or equal to 5')"),
      actionButton(NS(id, "eq"), "==", class = "add-button", title = "Equal to (e.g., 5 == 5 results in TRUE, '5 is equal to 5')"),
      actionButton(NS(id, "not_eq"), "!=", class = "add-button", title = "Not equal to (e.g., 5 != 3 results in TRUE, '5 is not equal to 3')"),
      class = "boxed-output"
    ),
    div(
      h3("Statistical & Utils Functions"),
      actionButton(NS(id, "mean"), "Mean", class = "add-button", title = "Calculate the average of numbers (e.g., Mean(ColName))"),
      actionButton(NS(id, "sd"), "standard deviation", class = "add-button", title = "SD(ColName) gives the standard deviation"),
      actionButton(NS(id, "median"), "Median", class = "add-button", title = "Median(ColName) calculates the median)"),
      actionButton(NS(id, "sum"), "Sum", class = "add-button", title = "Add up all the numbers Sum(ColName)"),
      actionButton(NS(id, "min"), "Min", class = "add-button", title = "Find the smallest number (e.g., Min(c(1, 2, 3)) gives 1)"),
      actionButton(NS(id, "max"), "Max", class = "add-button", title = "Find the largest number (e.g., Max(c(1, 2, 3)) gives 3)"),
      actionButton(NS(id, "c"), "concatenate", class = "add-button", title = "Combine values into a list (e.g., C(1, 2, 3) gives [1, 2, 3])"),
      actionButton(NS(id, "seq"), "sequence", class = "add-button", title = "Create a sequence of elements (e.g. Seq(1, 10, 0.1) which creates a sequence starting from 1 to 10 in steps of 0.1)."),
      actionButton(NS(id, "df"), "DataFrame", class = "add-button", title = "Create a table (e.g. DataFrame(Variable1, Variable2))"),
      actionButton(NS(id, "get_elem"), "get one element", class = "add-button",
      title = "Extract one element from a variable. This can either be ColName or a tabular dataset. In case it is a ColName the syntax is get_elem(ColName, idx) where idx is an integer number e.g. 1. In case one specific element of a dataset should be retrieved the syntax is get_elem(df, idx_row, idx_col). Again idx_row and idx_col have to be integers. The first one specifies the row number and the second one the column number."),
      actionButton(NS(id, "get_rows"), "get_rows", class = "add-button",
        title = 'Filter by row. For example get_rows(df, ColName == "Control") or get_rows(df, colName == 10)'),
      actionButton(NS(id, "get_cols"), "get_cols", class = "add-button",
        title = 'Extract column from a data frame (a table).
        For example get_cols(df, ColName) or get_cols(df, ColName1, ColName2)'),
      class = "boxed-output"
    ),
    div(
      h3("String Functions"),
      actionButton(NS(id, "paste"), "paste", class = "add-button", title = "Join pieces of text (e.g., paste('Hello', 'World') gives 'Hello World')."),
      actionButton(NS(id, "paste0"), "paste0", class = "add-button", title = "Join pieces of text without spaces (e.g., paste0('Hello', 'World') gives 'HelloWorld'). This is very practical if you want to join two columns e.g. paste0(ColName1, ColName2)"),
      actionButton(NS(id, "tolower"), "tolower", class = "add-button", title = "Convert text to lowercase (e.g., tolower('Hello') gives 'hello')"),
      actionButton(NS(id, "toupper"), "toupper", class = "add-button", title = "Convert text to uppercase (e.g., toupper('hello') gives 'HELLO')"),
      class = "boxed-output"
    ),
    div(
      h3("Convert types of columns"),
      actionButton(NS(id, "as_char"), "convert to character",
        title = "Convert a column of the dataset or an intermediate variable to character. For example as.char(ColName)",
        class = "add-button"),
      actionButton(NS(id, "as_int"), "convert to integer",
        title = "Convert a column of the dataset or an intermediate variable to integer. For example as.int(ColName)",
        class = "add-button"),
      actionButton(NS(id, "as_real"), "convert to real number",
        title = "Convert a column of the dataset or an intermediate variable to a real number. For example as.real(ColName)",
        class = "add-button"),
      actionButton(NS(id, "as_fact"), "convert to factors",
        title = "Convert a column of the dataset or an intermediate variable to a factor. For example as.fact(ColName)",
        class = "add-button"),
      class = "boxed-output"
    ),
    div(
      h3("Random number functions"),
      actionButton(NS(id, "dnorm"), "Dnorm", class = "add-button",
        title = "Probability density function for normal distribution (e.g., Dnorm(0) gives the height of the normal curve at 0)"),
      actionButton(NS(id, "pnorm"), "Pnorm", class = "add-button",
        title = "Cumulative distribution function for normal distribution (e.g., Pnorm(1) gives the probability that a random variable is less than 1)"),
      actionButton(NS(id, "qnorm"), "Qnorm", class = "add-button",
        title = "Quantile function for normal distribution (e.g., Qnorm(0.95) gives the value corresponding to the 95th percentile)"),
      actionButton(NS(id, "rnorm"), "Rnorm", class = "add-button",
        title = "Generate random numbers from a normal distribution (e.g., Rnorm(5) gives 5 random numbers from a normal distribution)"),
      actionButton(NS(id, "dbinom"), "Dbinom", class = "add-button",
        title = "Probability mass function for binomial distribution (e.g., Dbinom(2, 5, 0.5) gives the probability of getting exactly 2 successes in 5 trials with probability 0.5)"),
      actionButton(NS(id, "pbinom"), "Pbinom", class = "add-button",
        title = "Cumulative distribution function for binomial distribution (e.g., Pbinom(2, 5, 0.5) gives the probability of getting 2 or fewer successes in 5 trials)"),
      actionButton(NS(id, "qbinom"), "Qbinom", class = "add-button",
        title = "Quantile function for binomial distribution (e.g., Qbinom(0.95, 5, 0.5) gives the number of successes corresponding to the 95th percentile)"),
      actionButton(NS(id, "rbinom"), "Rbinom", class = "add-button",
        title = "Generate random numbers from a binomial distribution (e.g., Rbinom(5, 10, 0.5) gives 5 random binomial values with 10 trials and probability 0.5)"),
      actionButton(NS(id, "dpois"), "Dpois", class = "add-button",
        title = "Probability mass function for Poisson distribution (e.g., Dpois(3, 2) gives the probability of getting exactly 3 events when the average is 2)"),
      actionButton(NS(id, "ppois"), "Ppois", class = "add-button",
        title = "Cumulative distribution function for Poisson distribution (e.g., Ppois(3, 2) gives the probability of getting 3 or fewer events when the average is 2)"),
      actionButton(NS(id, "rpois"), "Rpois", class = "add-button",
        title = "Generate random numbers from a Poisson distribution (e.g., Rpois(5, 2) gives 5 random Poisson values with mean 2)"),
      actionButton(NS(id, "dunif"), "Dunif", class = "add-button",
        title = "Probability density function for uniform distribution (e.g., Dunif(0.5, min = 0, max = 1) gives the height of the uniform distribution at 0.5)"),
      actionButton(NS(id, "punif"), "Punif", class = "add-button",
        title = "Cumulative distribution function for uniform distribution (e.g., Punif(0.5, min = 0, max = 1) gives the probability of getting a value less than or equal to 0.5)"),
      actionButton(NS(id, "qunif"), "Qunif", class = "add-button",
        title = "Quantile function for uniform distribution (e.g., Qunif(0.95, min = 0, max = 1) gives the value corresponding to the 95th percentile)"),
      actionButton(NS(id, "runif"), "Runif", class = "add-button",
        title = "Generate random numbers from a uniform distribution (e.g., Runif(5, min = 0, max = 1) gives 5 random values between 0 and 1)"),

      class = "boxed-output"
    )
  )
}

OperatorEditorUI <- function(id) {
  ui <- fluidPage(
    div(
      textAreaInput(NS(id, "editable_code"), "Operation:", value = "", rows = 12),
      class = "boxed-output"
    ),
    br(),
    div(
      class = "boxed-output",
      fluidRow(
        column(
          7,
          actionButton(NS(id, "run_op_intermediate"), "Run operation and store intermediate results"),

        ),
        column(
          4,
          textInput(NS(id, "iv"), "Intermediate variable name:", value = "")
        )
      )
    ),
    br(),
    div(
      class = "boxed-output",
      fluidRow(

        column(
          7,
          actionButton(NS(id, "run_op"), "Run operation and append to dataset")
        ),
        column(
          4,
          textInput(NS(id, "nc"), "New column name:", value = "")
        )
      )
    ),
    uiOutput(NS(id, "head")),
    uiOutput(NS(id, "intermediate_results"))
  )
}

OperationEditorServer <- function(id, DataModelState, ResultsState, DataWranglingState) {

  moduleServer(id, function(input, output, session) {
    # Data
    observe({
      req(DataModelState$active_df_name)
      req(ResultsState$all_data[[DataModelState$active_df_name]])
      df <- ResultsState$all_data[[DataModelState$active_df_name]]
      req(is.data.frame(df))
      DataWranglingState$df <- df

      DataWranglingState$df_name <- create_df_name(DataWranglingState$df_name, names(df))
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

    # React to df button
    observe({
      req(DataWranglingState$df)
      var <- DataWranglingState$df_name
      observeEvent(input[[paste0("dataset_", var, "_", DataWranglingState$counter_id)]], {
        current_text <- input[["editable_code"]]
        updated_text <- paste(current_text, var, sep = " ")
        updateTextAreaInput(session, "editable_code", value = updated_text)
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
      DataWranglingState$df_name <- create_df_name(DataWranglingState$df_name, names(DataModelState$df))
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
      DataWranglingState$df_name <- create_df_name(DataWranglingState$df_name, names(DataModelState$df))
      colnames <- c(DataWranglingState$df_name, names(DataWranglingState$df))
      lapply(colnames, function(col) {
        observeEvent(input[[paste0("colnames_", col, "_", DataWranglingState$counter_id)]], {
          current_text <- input[["editable_code"]]
          updated_text <- paste(current_text, col, sep = " ")
          updateTextAreaInput(session, "editable_code", value = updated_text)
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
            riv = remove_intermediate_var_V1_2$new(name)
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
          updateTextAreaInput(session, "editable_code", value = updated_text)
        })
      })
    })

    # Run operation and store in intermediate result
    observeEvent(input$run_op_intermediate, {
      print_req(is.data.frame(DataWranglingState$df), "The dataset is missing")
      if (input$iv == "") {
        runjs("document.getElementById('OP-iv').focus();")
      }
      civ <- create_intermediate_var_V1_2$new(
        df = DataWranglingState$df, df_name = DataWranglingState$df_name,
        intermediate_vars = DataWranglingState$intermediate_vars,
        operation = input$editable_code,
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
      cnc <- create_new_col_V1_2$new(
        df = DataWranglingState$df, df_name = DataWranglingState$df_name,
        intermediate_vars = DataWranglingState$intermediate_vars,
        operation = input$editable_code,
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
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$sub, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "-", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$mul, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "*", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$div, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "/", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$bracket_open, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$bracket_close, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, ")", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$comma, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, ",", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

    observeEvent(input$log, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "log(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$log10, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "log10(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$sqrt, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "sqrt(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$exp, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "exp(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$exponent, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "^(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$sin, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "sin(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$cos, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "cos(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$tan, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "tan(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$sinh, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "sinh(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$cosh, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "cosh(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$tanh, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "tanh(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$asin, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "asin(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$acos, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "acos(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$atan, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "atan(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$abs, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "abs(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$ceil, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "ceiling(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$floor, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "floor(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$trunc, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "trunc(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$round, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "round(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$larger, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, ">", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$smaller, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "<", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$larger_eq, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, ">=", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$smaller_eq, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "<=", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$eq, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "==", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$not_eq, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "!=", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$paste, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "paste(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$paste0, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "paste0(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$tolower, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "tolower(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$toupper, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "toupper(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$get_elem, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "get_elem(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$get_rows, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "get_rows(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$get_cols, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "get_cols(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$mean, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Mean(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$sd, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "SD(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$median, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Median(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$sum, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Sum(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$min, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Min(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$max, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Max(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$c, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "C(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$seq, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Seq(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$df, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "DataFrame(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$as_char, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "as.char(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$as_int, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "as.int(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$as_real, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "as.real(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$as_fact, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "as.fact(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$dnorm, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Dnorm(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$pnorm, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Pnorm(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$qnorm, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Qnorm(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$rnorm, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Rnorm(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$dbinom, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Dbinom(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$pbinom, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Pbinom(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$qbinom, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Qbinom(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$rbinom, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Rbinom(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$dpois, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Dpois(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$ppois, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Ppois(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$rpois, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Rpois(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$dunif, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Dunif(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$punif, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Punif(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$qunif, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Qunif(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })
    observeEvent(input$runif, {
      current_text <- input$editable_code
      updated_text <- paste(current_text, "Runif(", sep = " ")
      updateTextAreaInput(session, "editable_code", value = updated_text)
    })

  })
} 
