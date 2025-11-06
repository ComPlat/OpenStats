bg_process_V1_2 <- R6::R6Class("bg_process_V1_2",
  public = list(
    process = NULL,
    result_val = NULL,
    poll_interval = NULL,
    cancel_clicked = FALSE,
    warnings = NULL,
    com = NULL,
    running_status = NULL,
    is_running = NULL,
    promise_result_name = NULL,
    promise_history_entry = NULL,
    in_backend = FALSE,

    initialize = function(poll_interval = 250, com = communicator_V1_2) {
      self$com <- com$new()
      self$poll_interval <- poll_interval
      self$process <- NULL
      self$result_val <- NULL
      self$running_status <- "Idle"
      self$is_running <- FALSE
      self$promise_result_name <- NULL
      self$promise_history_entry <- NULL
    },

    tick = function(ResultsState, DataModelState, DataWranglingState) {
      if (!is.null(ResultsState$bgp$process) && !ResultsState$bgp$process$is_alive()) {
        res <- tryCatch(ResultsState$bgp$process$get_result(), error = function(e) e)
        if (ResultsState$bgp$cancel_clicked) {
          ResultsState$bgp$running_status <- "Canceled process"
          self$process$interrupt()
          ResultsState$bgp$cancel_clicked <- FALSE
        } else if (inherits(res, "error") && !ResultsState$bgp$cancel_clicked) {
          ResultsState$bgp$running_status <- sprintf("Error failed with: %s", res$parent$message)
          ResultsState$bgp$com$print_err(res$parent$message)
        } else {
          ResultsState$bgp$warnings <- ResultsState$bgp$process$read_error()
          if (ResultsState$bgp$warnings != "") {
            ResultsState$bgp$com$print_warn(ResultsState$bgp$warnings)
          }
          e <- try(env_utils_V1_2$check_rls(ResultsState$all_data, res))
          if (inherits(e, "try-error")) {
            self$com$print_err(conditionMessage(e))
            return()
          }
          if (inherits(res, "HistoryReplayResult")) {
            DataModelState$df <- res$DataModelState$df
            DataModelState$formula <- res$DataModelState$formula
            DataModelState$backup_df <- res$DataModelState$backup_df
            DataModelState$filter_col <- res$DataModelState$filter_col
            DataModelState$filter_group <- res$DataModelState$filter_group

            ResultsState$all_data <- c(ResultsState$all_data, res$ResultsState$all_data)
            ResultsState$history <- c(ResultsState$history, res$ResultsState$history)
            ResultsState$counter <- length(ResultsState$all_data)

            DataWranglingState$df <- res$DataWranglingState$df
            DataWranglingState$df_name <- res$DataWranglingState$df_name
            DataWranglingState$current_page <- res$DataWranglingState$current_page
            DataWranglingState$total_pages <- res$DataWranglingState$total_pages
            DataWranglingState$counter_id <- res$DataWranglingState$counter_id
            DataWranglingState$intermediate_vars <- res$DataWranglingState$intermediate_vars
            print_success("Successfully applied the analysis to the dataset")
          } else {
            ResultsState$all_data[[ResultsState$bgp$promise_result_name]] <- res
          }
          ResultsState$counter <- ResultsState$counter + 1
          ResultsState$history[[length(ResultsState$history) + 1]] <- ResultsState$bgp$promise_history_entry
          ResultsState$bgp$result_val <- res
          ResultsState$bgp$running_status <- "Idle"

          if (!self$in_backend) {
            exportTestValues(
              result_list = ResultsState$all_data
            )
          }
        }
        ResultsState$bgp$process <- NULL
        ResultsState$bgp$is_running <- FALSE
        ResultsState$bgp$promise_result_name <- NULL
        ResultsState$bgp$promise_history_entry <- NULL
      }
    },

    init = function(ResultsState, DataModelState, DataWranglingState) {
      # Polling loop
      observe({
        invalidateLater(250)
        self$tick(ResultsState, DataModelState, DataWranglingState)
      })
    },

    start_direct = function(fun, args = list(), promise_result_name, promise_history_entry, ResultsState) {
      res <- try(do.call(fun, args), silent = TRUE)
      if (!inherits(res, "try-error")) {
        ResultsState$all_data[[promise_result_name]] <- res
        ResultsState$counter <- ResultsState$counter + 1
        ResultsState$history[[length(ResultsState$history) + 1]] <- promise_history_entry
        if (!self$in_backend) {
          exportTestValues(
            result_list = ResultsState$all_data
          )
        }
      } else {
        self$com$print_err(res)
      }
    },

    start = function(fun, args = list(), promise_result_name, promise_history_entry,
                     in_background = TRUE, ResultsState = NULL) {
      # NOTE: ResultsState is required to store the results when no background process is used.
      # Therefore, default is NULL
      background <- getOption("OpenStats.background", TRUE)
      if (!in_background || !background) {
        req(!is.null(ResultsState), "`ResultsState` must be provided when not running in background.")
        self$start_direct(fun, args, promise_result_name, promise_history_entry, ResultsState)
        return()
      }
      req(is.null(self$process) || !self$process$is_alive())
      self$promise_result_name <- promise_result_name
      self$promise_history_entry <- promise_history_entry
      self$is_running <- TRUE
      r6_args <- names(args)[sapply(args, function(x) inherits(x, "R6"))]
      req(length(r6_args) == 0, paste("Cannot pass R6 objects to background process:", paste(r6_args, collapse = ", ")))
      self$result_val <- NULL
      self$process <- callr::r_bg(fun, args = args)
      self$running_status <- "Running..."
    },

    cancel = function() {
      req(!is.null(self$process), self$process$is_alive())
      self$running_status <- "Canceled"
      self$cancel_clicked <- TRUE
    },

    get_result = function() self$result_val

  )
)

# When running the app in interactive mode this functions equivalent is:
# the observer spawnd by init
backend_get_result_V1_2 <- function(ResultsState) {
  # This is basically the polling in the backend
  while(!is.null(ResultsState$bgp$process) && ResultsState$bgp$process$is_alive()) {
    Sys.sleep(0.5)
  }

  # After the background process has finished check whether evrything was fine or not
  res <- tryCatch(ResultsState$bgp$process$get_result(), error = function(e) e)
  if (inherits(res, "error") && !ResultsState$bgp$cancel_clicked) {
    ResultsState$bgp$running_status <- sprintf("Error failed with: %s", res$message)
    try(ResultsState$bgp$com$print_err(res$parent$message), silent = TRUE) # TODO: the different error types have to be handled
    ResultsState$bgp$cancel_clicked <- FALSE
    ResultsState$bgp$is_running <- FALSE
  } else if (ResultsState$bgp$cancel_clicked) {
    ResultsState$bgp$running_status <- "Canceled process"
    ResultsState$bgp$cancel_clicked <- FALSE
    ResultsState$bgp$is_running <- FALSE
  } else {
    ResultsState$bgp$warnings <- ResultsState$bgp$process$read_error()
    if (ResultsState$bgp$warnings != "") {
      ResultsState$bgp$com$print_warn(ResultsState$bgp$warnings)
    }
    ResultsState$bgp$result_val <- res
    ResultsState$bgp$running_status <- "Completed"
    ResultsState$bgp$cancel_clicked <- FALSE
    ResultsState$bgp$is_running <- FALSE
  }
  ResultsState$bgp$process <- NULL
  # If everything was fine store the result
  if (ResultsState$bgp$running_status == "Completed") {
    res <- ResultsState$bgp$get_result()
    env_utils_V1_2$check_rls(ResultsState$all_data, res)
    ResultsState$all_data[[ResultsState$bgp$promise_result_name]] <- res
    ResultsState$counter <- ResultsState$counter + 1
    ResultsState$bgp$running_status <- "Idle"
    ResultsState$history[[length(ResultsState$history) + 1]] <- ResultsState$bgp$promise_history_entry
    ResultsState$bgp$promise_result_name <- NULL
  }

}

backend_result_state_V1_2 <- R6::R6Class(
  "backend_result_state_V1_2",
  public = list(
    all_data = list(),
    history = list(),
    counter = 0,
    bgp = NULL,
    initialize = function(all_data) {
      self$all_data <- all_data
      self$bgp <- bg_process_V1_2$new(500, backend_communicator_V1_2)
    }
  )
)

backend_data_model_state_V1_2 <- R6::R6Class(
  "backend_data_model_state_V1_2",
  public = list(
    df = NULL,
    formula = NULL,
    backup_df = NULL,
    filter_col = NULL,
    filter_group = NULL,
    active_df_name = NULL,
    rhs_string = NULL,
    initialize = function(df) {
      self$df <- df
    }
  )
)

backend_data_wrangling_state_V1_2 <- R6::R6Class(
  "backend_data_wrangling_state_V1_2",
  public = list(
    df = NULL,
    df_name = "df",
    current_page = 1,
    total_pages = 1,
    counter_id = 0,
    intermediate_vars = list(),
    code_string = NULL,
    initialize = function(DataModelState) {
      self$df_name <- env_utils_V1_2$create_df_name(self$df_name, names(DataModelState$df))
      self$df <- DataModelState$df
    }
  )
)

communicator_V1_2 <- R6::R6Class(
  "communicator_V1_2",
  public = list(
    print_warn = NULL,
    print_err = NULL,
    # check and print notifications
    print_req = NULL,
    # print notification without check
    print_noti = NULL,
    print_success = NULL,
    initialize = function() {
      self$print_warn <- print_warn
      self$print_err <- print_err
      self$print_noti <- print_noti
      self$print_req <- print_req
      self$print_success <- print_success
    }
  )
)
backend_communicator_V1_2 <- R6::R6Class(
  "backend_communicator_V1_2",
  public = list(
    print_warn = function(msg) warning(msg),
    print_err = function(msg) stop(msg),
    print_req = function(cond, msg) if (!cond) stop(msg),
    print_noti = function(msg) message(msg),
    print_success = function(msg) message(msg),
    initialize = function() {}
  )
)

correlation_V1_2 <- R6::R6Class(
  "correlation_V1_2",
  public = list(
    com = NULL,
    df = NULL,
    formula = NULL,
    dep = NULL,
    indep = NULL,
    method = NULL,
    alternative = NULL,
    conflevel = NULL,
    initialize = function(df, formula, method, alternative, conflevel, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula
      self$method <- method
      self$alternative <- alternative
      self$conflevel <- conflevel
      self$com <- com$new()
    },
    validate = function() {
      stopifnot("Formula is not of type linear" = inherits(self$formula, "LinearFormula"))
      formula <- as.character(self$formula@formula)
      self$dep <- formula[2]
      self$indep <- formula[3]
      env_check_ast_V1_2$check_ast(str2lang(self$indep), colnames(self$df))
      env_check_ast_V1_2$check_ast(str2lang(self$dep), colnames(self$df))
    },
    eval = function(ResultsState) {
      withCallingHandlers(
        expr = {
          new_name <- paste0(
            ResultsState$counter + 1, " Correlation ", env_utils_V1_2$firstup(self$method)
          )
          promise_history_entry <- self$create_history(new_name)
          ResultsState$bgp$start(
            fun = function(dep, indep, method, alternative, conflevel) {
              broom::tidy(
                cor.test(
                  dep, indep,
                  method = method,
                  alternative = alternative,
                  conf.level = conflevel
                ))
            },
            args = list(
              dep = self$df[, self$dep], indep = self$df[, self$indep],
              method = self$method, alternative = self$alternative,
              conflevel = self$conflevel
            ),
            promise_result_name = new_name,
            promise_history_entry = promise_history_entry,
            in_background = FALSE, ResultsState
          )
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
    },
    create_history = function(new_name) {
      list(
        type = "Correlation",
        formula = deparse(self$formula@formula),
        "Correlation method" = self$method,
        "Alternative hypothesis" = self$alternative,
        "Confidence level of the interval" = self$conflevel,
        "Result name" = new_name
      )
    }
  )
)

visualisation_V1_2 <- R6::R6Class(
  "visualisation_V1_2",
  public = list(
    com = NULL,
    df = NULL,
    x = NULL,
    y = NULL,
    method = NULL,
    xlabel = NULL,
    type_of_x = NULL,
    ylabel = NULL,
    colour_var = NULL,
    colour_legend_title = NULL,
    colour_theme = NULL,
    fill_var = NULL,
    fill_legend_title = NULL,
    fill_theme = NULL,
    facet_mode = NULL,
    facet_var = NULL,
    facet_y_scaling = NULL,
    xrange = NULL,
    yrange = NULL,
    width = NULL,
    height = NULL,
    resolution = NULL,
    col_names = NULL,

    initialize = function(df, x, y, method, xlabel, type_of_x, ylabel,
                          colour_var, colour_legend_title, colour_theme,
                          fill_var, fill_legend_title, fill_theme,
                          facet_var, facet_y_scaling,
                          xrange, yrange, width, height, resolution,
                          com = communicator_V1_2) {
      self$com <- com$new()

      # User input
      self$df <- df
      self$x <- x
      self$y <- y
      self$method <- method
      self$xlabel <- xlabel
      self$type_of_x <- type_of_x
      self$ylabel <- ylabel
      self$colour_var <- colour_var
      self$colour_legend_title <- colour_legend_title
      self$colour_theme <- colour_theme
      self$fill_var <- fill_var
      self$fill_legend_title <- fill_legend_title
      self$fill_theme <- fill_theme
      self$facet_var <- facet_var
      self$facet_y_scaling <- facet_y_scaling
      self$xrange <- xrange
      self$yrange <- yrange
      self$width <- width
      self$height <- height
      self$resolution <- resolution
      # Additional information based on user input
      self$facet_mode <- "none"
      if (self$facet_var != "") {
        self$facet_mode <- "facet_wrap"
      }

      self$col_names <- names(self$df)

      if (self$type_of_x == "numeric") {
        self$df[, x] <- as.numeric(self$df[, x])
      }
    },
    validate = function() {
      # Run first checks:
      if (self$width < 0) {
        self$com$print_warn("width has to be a positive number; It is set to 10 cm")
        self$width <- 10
      }
      if (self$height < 0) {
        self$com$print_warn("height has to be a positive number; It is set to 10 cm")
        self$height <- 10
      }
      if (self$width > 100) {
        self$com$print_warn("width exceeds max value of 100 cm; It is changed to 100 cm")
        self$width <- 100
      }
      if (self$height > 100) {
        self$com$print_warn("height exceeds max value of 100 cm; It is changed to 100 cm")
        self$height <- 100
      }
    },
    eval = function(ResultsState) {
      new_result_name <- paste0(
        ResultsState$counter + 1,
        " Visualization ", c(box = "Boxplot", dot = "Scatterplot", line = "Lineplot")[self$method]
      )
      promise_history_entry <- self$create_history(new_result_name)
      ResultsState$bgp$start(
        fun = function(method, df, x, y, xlabel, ylabel,
                       fill_var, fill_legend_title, fill_theme,
                       colour_var, colour_legend_title, colour_theme,
                       facet_mode, facet_var, facet_y_scaling,
                       xrange_min, xrange_max, yrange_min, yrange_max,
                       width, height, resolution) {
          withCallingHandlers(
            {
              if (method == "box") {
                p <- OpenStats:::env_plotting_V1_2$boxplot_fct(
                  df, x, y, xlabel, ylabel,
                  fill_var, fill_legend_title, fill_theme,
                  colour_var, colour_legend_title, colour_theme,
                  facet_mode, facet_var, facet_y_scaling,
                  xrange_min, xrange_max, yrange_min, yrange_max
                )
              } else if (method == "dot") {
                p <- OpenStats:::env_plotting_V1_2$dotplot_fct(
                  df, x, y, xlabel, ylabel,
                  colour_var, colour_legend_title, colour_theme,
                  facet_mode, facet_var, facet_y_scaling,
                  xrange_min, xrange_max, yrange_min, yrange_max
                )
              } else if (method == "line") {
                p <- OpenStats:::env_plotting_V1_2$lineplot_fct( # TODO: define an environment for all internal code analog to the internal plotting
                  df, x, y, xlabel, ylabel,
                  colour_var, colour_legend_title, colour_theme,
                  facet_mode, facet_var, facet_y_scaling,
                  xrange_min, xrange_max, yrange_min, yrange_max
                )
              }
            },
            warning = function(warn) {
              com$print_warn(warn$message)
              invokeRestart("muffleWarning")
            }
          )
          ggplot2::ggplot_build(p) # NOTE: invokes errors and warnings by building but not rendering plot
          new("plot", p = p, width = width, height = height, resolution = resolution)
        },
        args = list(self$method, self$df, self$x, self$y, self$xlabel, self$ylabel,
          self$fill_var, self$fill_legend_title, self$fill_theme,
          self$colour_var, self$colour_legend_title, self$colour_theme,
          self$facet_mode, self$facet_var, self$facet_y_scaling,
          self$xrange[[1]], self$xrange[[2]], self$yrange[[1]], self$yrange[[2]],
          self$width, self$height, self$resolution
        ),
        promise_result_name = new_result_name,
        promise_history_entry = promise_history_entry,
        in_background = TRUE, ResultsState
      )
    },
    create_history = function(new_result_name) {
      list(
        type = "Visualisation",
        x = self$x, y = self$y, "Plot-type" = self$method,
        "X axis label" = self$xlabel, "Y axis label" = self$ylabel, "Type of x" = self$type_of_x,
        "Colour variable" = self$colour_var, "Legend title for colour" = self$colour_legend_title,
        "Colour theme" = self$colour_theme,
        "Fill variable" = self$fill_var, "Legend title for fill" = self$fill_legend_title,
        "Fill theme" = self$fill_theme,
        "Split in subplots" = self$facet_mode, "Split by" = self$facet_var,
        "How to scale y in subplots" = self$facet_y_scaling,
        "X-Range" = as.character(self$xrange),
        "Y-Range" = as.character(self$yrange),
        Width = self$width, Height = self$height, Resolution = self$resolution,
        "Result name" = new_result_name
      )
    }
  )
)
visualisation_model_V1_2 <- R6::R6Class(
  "visualisation_model_V1_2",
  public = list(
    df = NULL,
    formula = NULL,
    layer = NULL,
    com = NULL,
    initialize = function(df, formula, layer, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula
      self$layer <- layer
      self$com = com$new()
    },

    validate = function() {},

    create_history = function(new_name) {
      list(
        type = "VisualizationModel",
        formula = deparse(self$formula@formula),
        "Layer" = self$layer,
        "Result name" = new_name
      )
    },

    eval = function(ResultsState) {
      withCallingHandlers(
        expr = {
          new_name <- paste0(
            ResultsState$counter + 1, " Visualization Model ", c(box = "Boxplot", dot = "Scatterplot", line = "Lineplot")[self$layer]
          )
          promise_history_entry <- self$create_history(new_name)
          ResultsState$bgp$start(
            fun = function(df, formula, layer) {
              p <- OpenStats:::env_summarising_model_V1_2$plot_model(df, formula, layer)
              ggplot2::ggplot_build(p) # NOTE: invokes errors and warnings by building but not rendering plot
              return(
                new("plot", p = p, width = 10, height = 10, resolution = 600)
              )
            },
            args = list(
              df = self$df, formula = self$formula, layer = self$layer
            ),
            promise_result_name = new_name,
            promise_history_entry = promise_history_entry,
            in_background = TRUE, ResultsState
          )
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
    }
  )
)

apply_filter_V1_2 <- R6::R6Class(
  "apply_filter_V1_2",
  public = list(
    selected_cols = NULL,
    selected_groups = NULL,
    com = NULL,
    initialize = function(selected_cols, selected_groups, com = communicator_V1_2) {
      self$selected_cols <- selected_cols
      self$selected_groups <- selected_groups

      self$com <- com$new()
    },
    validate = function() {
      try(
        {
          if (length(self$selected_groups) == 0 || length(self$selected_cols) == 0) {
            stop("Invalid subset either no columns or now levels of the columns were selected")
          }
        },
        silent = TRUE
      )
    },
    eval = function(DataModelState, ResultsState) {
      DataModelState$backup_df <- DataModelState$df
      DataModelState$df <- env_utils_V1_2$split_groups(DataModelState$df, self$selected_cols, self$selected_groups)
      DataModelState$filter_col <- self$selected_cols
      DataModelState$filter_group <- self$selected_groups
      self$create_history(ResultsState)
    },
    create_history = function(ResultsState) {
      ResultsState$history[[length(ResultsState$history) + 1]] <- list(
        type = "ApplyFilter",
        Variable = self$selected_cols,
        "Variable levels" = self$selected_groups
      )
    }
  )
)
remove_filter_V1_2 <- R6::R6Class(
  "remove_filter_V1_2",
  public = list(
    initialize = function() {},
    validate = function() {},
    eval = function(ResultsState, DataModelState) {
      DataModelState$df <- DataModelState$backup_df
      DataModelState$backup_df <- NULL
      DataModelState$filter_col <- NULL
      DataModelState$filter_group <- NULL
      self$create_history(ResultsState, DataModelState)
    },
    create_history = function(ResultsState, DataModelState) {
      ResultsState$history[[length(ResultsState$history) + 1]] <- list(
        type = "RemoveFilter",
        Variable = paste(DataModelState$filter_col, collapse = ", "),
        "Variable levels" = paste(DataModelState$filter_group, collapse = ", ")
      )
    }
  )
)

create_intermediate_var_V1_2 <- R6::R6Class(
  "create_intermediate_var_V1_2",
  public = list(
    df = NULL,
    df_name = NULL,
    intermediate_vars = NULL,
    operation = NULL,
    name = NULL,
    com = NULL,

    var_name = NULL,

    initialize = function(df, df_name, intermediate_vars, operation, name, com = communicator_V1_2) {
      self$df <- df
      self$df_name <- df_name
      self$intermediate_vars <- intermediate_vars
      self$operation <- operation
      self$name <- name
      self$com <- com$new()
    },

    validate = function() {
      self$com$print_req(self$name != "", "Please set a name for the intermediate variable")

      # Check the new name of the intermediate variable
      var_name <- self$name |> make.names()
      already_used_names <- names(self$df)
      if (length(self$intermediate_vars) >= 1) {
        already_used_names <- c(already_used_names, names(self$intermediate_vars))
      }
      if (var_name %in% already_used_names) {
        self$com$print_err("Found invalid variable name as it is already used by a column of the data table, or as other intermediate variable")
        stop("Invalid variable name")
      }
      if (var_name == self$df_name) {
        self$com$print_err("Found invalid variable name df. This name is reserved for the dataset")
        stop("Invalid variable name")
      }
      self$var_name <- var_name

      # Can string be converted to R language object
      op <- try(str2lang(self$operation), silent = TRUE)
      if (inherits(op, "try-error")) {
        self$com$print_err("Could not convert operation to R code")
        self$com$print_err(op)
        stop("Invalid code")
      }

      # Check the ast and check that all variables used are found
      e <- try({
        check_length_code(self$operation)
        vars <- c(self$df_name, names(self$df))
        if (length(self$intermediate_vars) >= 1) {
          vars <- c(vars, names(self$intermediate_vars))
        }
        env_check_ast_V1_2$check_ast(op, vars)
      })
      if (inherits(e, "try-error")) {
        self$com$print_err(e)
        stop("Invalid ast")
      }
    },

    eval = function(ResultsState, DataWranglingState) {
      e <- try({
        eval_env <- create_run_env()
        list2env(self$intermediate_vars, envir = eval_env)
        list2env(DataWranglingState$df, envir = eval_env) # NOTE: this adds each column as own variable
        eval_env[[DataWranglingState$df_name]] <- self$df
        new <- eval(parse(text = self$operation), envir = eval_env)
        check_type_res(new)
        env_utils_V1_2$check_rls(ResultsState$all_data, new)
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        self$com$print_err(err)
        stop("Error in create intermediate variable")
      } else {
        DataWranglingState$intermediate_vars[[self$var_name]] <- new
        ResultsState$counter <- ResultsState$counter + 1

        # NOTE: for better saving the results
        if (!inherits(new, "data.frame")) {
          new_df <- setNames(data.frame(new), self$name)
        } else {
          new_df <- new
        }
        ResultsState$all_data[[paste0(ResultsState$counter, " ", self$var_name)]] <- new_df
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "CreateIntermediateVariable",
          operation = self$operation,
          name = self$name
        )
      }
    }
  )
)
remove_intermediate_var_V1_2 <- R6::R6Class(
  "remove_intermediate_var_V1_2",
  public = list(
    name = NULL,
    com = NULL,

    initialize = function(name, com = communicator_V1_2) {
      self$name <- name
      self$com <- com$new()
    },

    validate = function() {},

    eval = function(ResultsState, DataWranglingState) {
      if (!is.null(DataWranglingState$intermediate_vars[[self$name]])) {
        DataWranglingState$intermediate_vars[[self$name]] <- NULL
        self$com$print_success(paste("Removed intermediate result:", self$name))
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "RemoveIntermediateVariable",
          "Intermediate variable" = self$name
        )
      }
    }
  )
)
create_new_col_V1_2 <- R6::R6Class(
  "create_new_col_V1_2",
  public = list(
    df = NULL,
    df_name = NULL,
    intermediate_vars = NULL,
    operation = NULL,
    name = NULL,
    com = NULL,

    initialize = function(df, df_name, intermediate_vars, operation, name, com = communicator_V1_2) {
      self$df <- df
      self$df_name <- df_name
      self$intermediate_vars <- intermediate_vars
      self$operation <- operation
      self$name <- name
      self$com <- com$new()
    },

    validate = function() {
      self$com$print_req(self$name != "", "Please set a name for the new column")
 

      # Check the new name of the column
      var_name <- self$name |> make.names()
      already_used_names <- names(self$df)
      if (length(self$intermediate_vars) >= 1) {
        already_used_names <- c(already_used_names, names(self$intermediate_vars))
      }
      if (var_name %in% already_used_names) {
        self$com$print_err("Found invalid variable name as it is already used by a column of the data table, or as other intermediate variable")
        stop("Invalid variable name")
      }
      if (var_name == self$df_name) {
        self$com$print_err("Found invalid variable name df. This name is reserved for the dataset")
        stop("Invalid variable name")
      }
      self$name <- var_name

      # Can string be converted to R language object
      op <- try(str2lang(self$operation), silent = TRUE)
      if (inherits(op, "try-error")) {
        self$com$print_err("Could not convert operation to R code")
        stop("Invalid code")
      }

      # Check the ast and check that all variables used are found
      e <- try({
        check_length_code(self$operation)
        vars <- c(self$df_name, names(self$df))
        if (length(self$intermediate_vars) >= 1) {
          vars <- c(vars, names(self$intermediate_vars))
        }
        env_check_ast_V1_2$check_ast(op, vars)
      })
      if (inherits(e, "try-error")) {
        self$com$print_err(e)
        stop("Invalid ast")
      }

    },

    eval = function(ResultsState, DataWranglingState, DataModelState) {
      e <- try({
        eval_env <- create_run_env()
        list2env(self$intermediate_vars, envir = eval_env)
        list2env(self$df, envir = eval_env)  # NOTE: this adds each column as own variable
        eval_env[[DataWranglingState$df_name]] <- self$df
        new <- eval(parse(text = self$operation), envir = eval_env)
        check_type_res(new)
        env_utils_V1_2$check_rls(ResultsState$all_data, new)
        self$df[, self$name] <- new
        DataWranglingState$df <- self$df
        if (!is.null(DataModelState$backup_df)) {
          eval_env <- create_run_env()
          list2env(self$intermediate_vars, envir = eval_env)
          list2env(DataModelState$backup_df, envir = eval_env)  # NOTE: this adds each column as own variable
          new <- eval(parse(text = self$operation), envir = eval_env)
          check_type_res(new)
          DataModelState$backup_df[, self$name] <- new
          self$com$print_warn("Conducted operation also for entire dataset and not only for the subset")
        }
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        self$com$print_err(err)
        stop("Error in create new column")
      } else {
        DataModelState$df <- DataWranglingState$df
        ResultsState$all_data[[DataModelState$active_df_name]] <- DataWranglingState$df
        DataWranglingState$counter_id <- DataWranglingState$counter_id + 1
        ResultsState$counter <- ResultsState$counter + 1
        new_name <- paste0(ResultsState$counter, " Dataset")
        ResultsState$all_data[[new_name]] <- DataModelState$df
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "CreateNewColumn",
          operation = self$operation,
          "column name" = self$name
        )
      }
    }
  )
)

summarise_model_V1_2 <- R6::R6Class(
  "summarise_model_V1_2",
  public = list(
    df = NULL,
    formula = NULL,
    com = NULL,
    initialize = function(df, formula, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula
      self$com <- com$new()
    },

    validate = function() {},

    eval_lm = function(ResultsState, new_name, promise_history_entry) {
      withCallingHandlers(
        {
          expr = {
            ResultsState$bgp$start(
              fun = function(df, formula) {
                p <- OpenStats:::env_summarising_model_V1_2$plot_pred_lm(df, formula)
                ggplot2::ggplot_build(p)
                p <- new("plot", p = p, width = 15, height = 15, resolution = 600)
                model <- lm(formula@formula, data = df)
                summary <- broom::tidy(model)
                ic <- OpenStats:::env_summarising_model_V1_2$create_information_criterions(model)
                new("summaryModel", p = p, summary = summary, information_criterions = ic)
              },
              args = list(df = self$df, formula = self$formula),
              promise_result_name = new_name,
              promise_history_entry = promise_history_entry,
              in_background = FALSE, ResultsState
            )
          }
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
    },

    eval_glm = function(ResultsState, new_name, promise_history_entry) {
      withCallingHandlers(
        {
          expr = {
            ResultsState$bgp$start(
              fun = function(df, formula) {
                p <- OpenStats:::env_summarising_model_V1_2$plot_pred_glm(df, formula)
                ggplot2::ggplot_build(p)
                p <- new("plot", p = p, width = 15, height = 15, resolution = 600)
                family <- self$formula@family
                link_fct <- self$formula@link_fct
                family <- str2lang(paste0("stats::", family, "(\"", link_fct, "\")"))
                model <- glm(self$formula@formula, data = self$df, family = eval(family))
                summary <- broom::tidy(model)
                ic <- OpenStats:::env_summarising_model_V1_2$create_information_criterions(model)
                new("summaryModel", p = p, summary = summary, information_criterions = ic)
              },
              args = list(df = self$df, formula = self$formula),
              promise_result_name = new_name,
              promise_history_entry = promise_history_entry,
              in_background = FALSE, ResultsState
            )
          }
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
    },

    eval_optim = function(ResultsState, new_name, promise_history_entry) {
      withCallingHandlers(
        {
          expr = {
            ResultsState$bgp$start(
              fun = function(df, formula) {
                res <- OpenStats:::env_optim_V1_2$optimize(formula, df)
                p <- OpenStats:::env_optim_V1_2$plot_model_optim(formula, res)
                summary <- OpenStats:::env_optim_V1_2$summary_model_optim(formula, res)
                ic <- OpenStats:::env_optim_V1_2$information_criterion_optim(res)
                new("summaryModel", p = p, summary = summary, information_criterions = ic)
              },
              args = list(df = self$df, formula = self$formula),
              promise_result_name = new_name,
              promise_history_entry = promise_history_entry,
              in_background = FALSE, ResultsState
            )
          }
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )

    },

    eval = function(ResultsState) {
      new_name <- paste0(ResultsState$counter + 1, " Model summary")
      promise_history_entry <- self$create_history(new_name)
      if (inherits(self$formula, "LinearFormula")) {
        res <- self$eval_lm(ResultsState, new_name, promise_history_entry)
      } else if (inherits(self$formula, "GeneralisedLinearFormula")) {
        res <- self$eval_glm(ResultsState, new_name, promise_history_entry)
      } else if (inherits(self$formula, "OptimFormula")) {
        res <- self$eval_optim(ResultsState, new_name, promise_history_entry)
      }
    },
    create_history = function(new_name) {
      list(
        type = "ModelSummary",
        formula = deparse(self$formula@formula),
        "Result name" = new_name
      )
    }

  )
)
create_formula_V1_2 <- R6::R6Class(
  "create_formula_V1_2",
  public = list(
    response_var = NULL,
    right_site = NULL,
    df = NULL,
    com = NULL,
    com_class = NULL,

    initialize = function(response_var, right_site, df, com = communicator_V1_2) {
      self$response_var <- response_var
      self$right_site <- right_site
      self$df <- df
      self$com_class <- com
      self$com <- self$com_class$new()
    },

    validate = function() {},

    eval_eq = function(ResultsState, DataModelState, model_type, ...) {
      eq <- NULL
      if (model_type == "Linear") {
        formula <- paste(self$response_var, " ~ ", self$right_site)
        formula <- as.formula(formula)
        env_check_ast_V1_2$check_ast(formula, colnames(self$df))
        DataModelState$formula <- new("LinearFormula", formula = formula)
        model <- lm(formula, data = self$df)
        eq <- extract_eq(model, wrap = TRUE)
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "CreateFormula",
          formula = deparse(formula),
          "Model Type" = "Linear",
          details = ""
        )
      } else if (model_type == "Generalised Linear Model") {
        details <- c(...)
        family <- details[[1]]
        link_fct <- details[[2]]
        formula <- paste(self$response_var, " ~ ", self$right_site)
        formula <- as.formula(formula)
        env_check_ast_V1_2$check_ast(formula, colnames(self$df))
        DataModelState$formula <- new("GeneralisedLinearFormula",
          formula = formula, family = family, link_fct = link_fct
        )
        family <- str2lang(paste0("stats::", family, "(\"", link_fct, "\")"))
        model <- glm(formula, data = self$df, family = eval(family))
        eq <- extract_eq(model, wrap = TRUE)
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "CreateFormula",
          formula = deparse(formula),
          "Model Type" = "Generalised Linear Model",
          family = details[[1]],
          "Link function" = link_fct
        )
      } else if (model_type == "Optimization Model") {
        details <- c(...)
        lower <- details[[1]]
        upper <- details[[2]]
        seed <- details[[3]]
        formula <- paste0(self$response_var, " ~ ", self$right_site)
        formula <- as.formula(formula)
        DataModelState$formula <- env_optim_V1_2$create_formula_optim(formula, self$df, lower, upper, seed)
        eq <- paste0(self$response_var, " = ", self$right_site)
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "CreateFormula",
          formula = deparse(formula),
          lower = lower, upper = upper, seed = seed,
          "Model Type" = "Optimization Model"
        )
      }

      return(eq)
    },

    eval = function(ResultsState, DataModelState, model_type, ...) {
      e <- try({
        eq <- self$eval_eq(ResultsState, DataModelState, model_type, ...)
        pm <- summarise_model_V1_2$new(DataModelState$df, DataModelState$formula, self$com_class)
        pm$validate()
        pm$eval(ResultsState)
        eq
      })
      if (inherits(e, "try-error")) {
        self$com$print_err(e)
      }
      return(e)
    }
  )
)

shapiro_on_data_V1_2 <- R6::R6Class(
  "shapiro_on_data_V1_2",
  public = list(
    df = NULL,
    formula = NULL,
    com = NULL,

    initialize = function(df, formula, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula@formula
      self$com = com$new()
    },

    validate = function() {},

    eval = function(ResultsState) {
      withCallingHandlers(
        {
          new_name <- paste0(
            ResultsState$counter + 1, " Shapiro on data"
          )
          promise_history_entry <- self$create_history(new_name)
          ResultsState$bgp$start(
            fun = function(df, formula) {
              res <- list()
              dat <- OpenStats:::env_utils_V1_2$split_data(df, formula)
              for (i in unique(dat[, 2])) {
                tempDat <- dat[dat[, 2] == i, ]
                temp <- broom::tidy(shapiro.test(tempDat[, 1]))
                if (!is.null(temp)) {
                  temp$variable <- i
                  temp$`Normal distributed` <- temp$p.value > 0.05
                  res[[length(res) + 1]] <- temp
                }
              }
              res <- do.call(rbind, res)
              res
            },
            args = list(df = self$df, formula = self$formula),
            promise_result_name = new_name,
            promise_history_entry = promise_history_entry,
            in_background = FALSE, ResultsState = ResultsState
          )
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
    },

    create_history = function(new_name) {
      list(
        type = "ShapiroOnData",
        formula = deparse(self$formula),
        "Result name" = new_name
      )
    }
  )
)
shapiro_on_residuals_V1_2 <- R6::R6Class(
  "shapiro_on_residuals_V1_2",
  public = list(
    df = NULL,
    formula = NULL,
    com = NULL,

    initialize = function(df, formula, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula
      self$com <- com$new()
    },

    validate = function() {
      stopifnot("Formula is not of type linear" = inherits(self$formula, "LinearFormula"))
    },

    eval = function(ResultsState) {
      withCallingHandlers(
        expr = {
          new_name <- paste0(ResultsState$counter + 1, " Shapiro on residuals")
          promise_history_entry <- self$create_history(new_name)
          ResultsState$bgp$start(
            fun = function(df, formula) {
              fit <- lm(formula@formula, data = df)
              r <- resid(fit)
              res <- broom::tidy(shapiro.test(r))
              res$`Residuals normal distributed` <- res$p.value > 0.05
              res
            },
            args = list(df = self$df, formula = self$formula),
            promise_result_name = new_name,
            promise_history_entry = promise_history_entry,
            in_background = FALSE, ResultsState = ResultsState
          )
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
    },

    create_history = function(new_name) {
      list(
        type = "ShapiroOnResiduals",
        formula = deparse(self$formula@formula),
        "Result name" = new_name
      )
    }
  )
)
levene_V1_2 <- R6::R6Class(
  "levene_V1_2",
  public = list(
    df = NULL,
    formula = NULL,
    center = NULL,
    com = NULL,

    initialize = function(df, formula, center, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula
      self$center <- center
      self$com <- com$new()
    },

    validate = function() {
      stopifnot("Formula is not of type linear" = inherits(self$formula, "LinearFormula"))
    },

    eval = function(ResultsState) {
      withCallingHandlers(
        expr = {
          new_name <- paste0(ResultsState$counter + 1, " Levene test")
          promise_history_entry <- self$create_history(new_name)
          ResultsState$bgp$start(
            fun = function(df, formula, center) {
              res <- broom::tidy(
                car::leveneTest(formula, data = df, center = center)
              )
              res$`Variance homogenity` <- res$p.value > 0.05
              res
            },
            args = list(df = self$df, formula = self$formula@formula, center = self$center),
            promise_result_name = new_name,
            promise_history_entry = promise_history_entry,
            in_background = FALSE, ResultsState = ResultsState
          )
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
    },

    create_history = function(new_name) {
      list(
        type = "LeveneTest",
        formula = deparse(self$formula@formula),
        "Data center" = self$center,
        "Result name" = new_name
      )
    }
  )
)
diagnostic_plots_V1_2 <- R6::R6Class(
  "diagnostic_plots_V1_2",
  public = list(
    df = NULL,
    formula = NULL,
    com = NULL,

    initialize = function(df, formula, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula
      self$com <- com$new()
    },

    validate = function() {},

    eval = function(ResultsState) {
      withCallingHandlers(
        expr = {
          new_name <- paste0(ResultsState$counter + 1, " Diagnostic plot")
          promise_history_entry <- self$create_history(new_name)
          ResultsState$bgp$start(
            fun = function(df, formula) {
              p <- OpenStats:::env_diagnostic_plots_V1_2$diagnostic_plots(df, formula)
              new("plot", p = p, width = 15, height = 15, resolution = 600)
            },
            args = list(df = self$df, formula = self$formula),
            promise_result_name = new_name,
            promise_history_entry = promise_history_entry,
            in_background = TRUE, ResultsState
          )
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
    },

    create_history = function(new_name) {
      list(
        type = "DiagnosticPlots",
        formula = deparse(self$formula@formula),
        "Result name" = new_name
      )
    }
  )
)

dose_response_V1_2 <- R6::R6Class(
  "dose_response_V1_2",
  public = list(
    df = NULL,
    is_xlog = NULL,
    is_ylog = NULL,
    substance_names = NULL,
    formula = NULL,
    com = NULL,
    res_df = NULL,
    res_p = NULL,

    initialize = function(df,
                          is_xlog, is_ylog,
                          substance_names,
                          formula, com = communicator_V1_2) {
      self$df <- df
      self$df[, substance_names] <- self$df[, substance_names] |> as.character()
      self$is_xlog <- is_xlog
      self$is_ylog <- is_ylog
      self$substance_names <- substance_names
      self$formula <- formula@formula
      self$com <- com$new()
    },

    validate = function() {},

    eval = function(ResultsState, new_name) {
      withCallingHandlers(
        expr = {
          promise_history_entry <- self$create_history(new_name)
          ResultsState$bgp$start(
            fun = function(df, formula, substance_names, is_xlog, is_ylog) {
              f <- as.character(formula)
              dep <- f[2]
              indep <- f[3]
              res_p <- NULL
              res_df <- NULL
              err <- try({
                OpenStats:::check_formula(formula)
                OpenStats:::env_check_ast_V1_2$check_ast(str2lang(indep), colnames(df))
                OpenStats:::env_check_ast_V1_2$check_ast(str2lang(dep), colnames(df))

                res <- OpenStats:::env_lc_V1_2$ic50(
                  df, dep,
                  indep, substance_names,
                  is_xlog, is_ylog
                )
                if (inherits(res, "errorClass")) {
                  stop(res$error_message)
                }
                res_df <- lapply(res, function(x) {
                  if (inherits(x, "errorClass")) {
                    return(NULL)
                  }
                  return(x[[1]])
                })
                res_df <- res_df[!is.null(res_df)]
                res_df <- res_df[!sapply(res_df, is.null)]
                res_df <- Reduce(rbind, res_df)
                res_p <- lapply(res, function(x) {
                  if (inherits(x, "errorClass")) {
                    return(NULL)
                  }
                  return(x[[2]])
                })
                res_p <- res_p[!is.null(res_p)]
                res_p <- res_p[!sapply(res_p, is.null)]
              })
              if (inherits(err, "try-error")) {
                stop(err)
              }
              new("doseResponse", df = res_df, p = res_p,
                name_col = substance_names, formula = formula,
                xTransform = is_xlog, yTransform = is_ylog,
                current_page = 1L)
            },
            args = list(
              df = self$df, formula = self$formula, substance_names = self$substance_names,
              is_xlog = self$is_xlog, is_ylog = self$is_ylog
            ),
            promise_result_name = new_name,
            promise_history_entry = promise_history_entry,
            in_background = TRUE, ResultsState
          )
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
    },

    create_history = function(new_name) {
      list(
        type = "DoseResponse",
        "Column containing the names" = self$substance_names,
        "Log transform x-axis" = self$is_xlog,
        "Log transform y-axis" = self$is_ylog,
        "formula" = deparse(self$formula),
        "Result name" = new_name
      )
    }

  )
)

t_test_V1_2 <- R6::R6Class(
  "t_test_V1_2",
  public = list(
    df = NULL,
    formula = NULL,
    variances_equal = NULL,
    conf_level = NULL,
    alternative_hyp = NULL,
    com = NULL,

    initialize = function(df, formula,
                          variances_equal, conf_level,
                          alternative_hyp, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula@formula
      self$variances_equal <- variances_equal
      self$conf_level <- conf_level
      self$alternative_hyp <- alternative_hyp
      self$com = com$new()
    },

    validate = function() {},

    eval = function(ResultsState) {
      withCallingHandlers(
        {
          new_name <- paste0( ResultsState$counter + 1, " T-Test")
          promise_history_entry <- self$create_history(new_name)
          ResultsState$bgp$start(
            fun = function(formula, df, conf_level, alternative_hyp, variances_equal) {
              eq <- TRUE
              if (variances_equal == "noeq") {
                eq <- FALSE
              }
              broom::tidy(t.test(formula,
                data = df, conf.level = conf_level,
                alternative = alternative_hyp, var.equal = eq
              ))
            },
            args = list(
              formula = self$formula, df = self$df,
              conf_level = self$conf_level, alternative_hyp = self$alternative_hyp,
              variances_equal = self$variances_equal
            ),
            promise_result_name = new_name,
            promise_history_entry = promise_history_entry,
            in_background = TRUE, ResultsState
          )
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
    },

    create_history = function(new_name) {
      list(
        type = "TTest",
        formula = deparse(self$formula),
        "Confidence level of the interval" = self$conf_level,
        "alternative hypothesis" = self$alternative_hyp,
        "The two variances are" = self$variances_equal,
        "Result name" = new_name
      )
    }
  )
)

statistical_tests_V1_2 <- R6::R6Class(
  "statistical_tests_V1_2",
  public = list(
    df = NULL,
    formula = NULL,
    balanced_design = NULL,
    p_val = NULL,
    p_val_adj_method = NULL,
    dep = NULL,
    indep = NULL,
    com = NULL,

    initialize = function(df, formula, balanced_design, p_val, p_val_adj_method, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula
      self$balanced_design <- balanced_design
      self$p_val <- p_val
      self$p_val_adj_method <- p_val_adj_method
      self$com <- com$new()
    },

    validate = function() {},

    eval_lm = function(method, new_name, ResultsState) {
      withCallingHandlers(
        expr = {
          promise_history_entry <- self$create_history(new_name, method)
          ResultsState$bgp$start(
            fun = function(formula, df, dep, indep, balanced_design, p_val_adj_method, p_val, method) {
              fit <- NULL
              switch(method,
                aov = {
                  fit <- broom::tidy(aov(
                    formula@formula,
                    data = df
                  ))
                },
                kruskal = {
                  fit <- broom::tidy(
                    kruskal.test(formula@formula, data = df)
                  ) # Keep here the restriction for respone ~ predictor
                },
                HSD = {
                  OpenStats:::check_formula(formula@formula)
                  aov_res <- aov(formula@formula, data = df)
                  bal <- balanced_design
                  if (bal == "Balanced") {
                    bal <- TRUE
                  } else {
                    bal <- FALSE
                  }
                  fit <- agricolae::HSD.test(aov_res,
                    trt = indep,
                    alpha = p_val, group = TRUE, unbalanced = bal
                  )$groups
                },
                kruskalTest = {
                  # FIX: I think kruskal does not have a p adjustment method
                  # It is silently ignored
                  OpenStats:::check_formula(formula@formula)
                  fit <- with(df, agricolae::kruskal(df[, dep], df[, indep]),
                    alpha = p_val, p.adj = p_val_adj_method, group = TRUE
                  )$groups
                  names(fit)[1] <- dep
                },
                LSD = {
                  OpenStats:::check_formula(formula@formula)
                  aov_res <- aov(formula@formula, data = df)
                  fit <- agricolae::LSD.test(aov_res,
                    trt = indep,
                    alpha = p_val, p.adj = p_val_adj_method, group = TRUE
                  )$groups
                },
                scheffe = {
                  OpenStats:::check_formula(formula@formula)
                  aov_res <- aov(formula@formula, data = df)
                  fit <- agricolae::scheffe.test(
                    aov_res,
                    trt = indep, alpha = p_val, group = TRUE
                  )$groups
                },
                REGW = {
                  OpenStats:::check_formula(formula@formula)
                  aov_res <- aov(formula@formula, data = df)
                  fit <- agricolae::REGW.test(
                    aov_res,
                    trt = indep, alpha = p_val, group = TRUE
                  )$groups
                }
              )
              fit <- cbind(fit, row.names(fit))
              names(fit)[ncol(fit)] <- paste0(indep, collapse = ".")
              fit
            },
            args = list(
              formula = self$formula, df = self$df, dep = self$dep, indep = self$indep,
              balanced_design = self$balanced_design, p_val_adj_method = self$p_val_adj_method,
              p_val = self$p_val, method = method
            ),
            promise_result_name = new_name,
            promise_history_entry = promise_history_entry,
            in_background = TRUE, ResultsState
          )
        }
      )
    },

    eval_glm = function(method, new_name, ResultsState) {
      withCallingHandlers(
        expr = {
          promise_history_entry <- self$create_history(new_name, method)
          ResultsState$bgp$start(
            fun = function(formula, df, method) {
              fit <- NULL
              switch(method,
                aov = {
                  family <- formula@family
                  link_fct <- formula@link_fct
                  family <- str2lang(paste0("stats::", family, "(\"", link_fct, "\")"))
                  model <- glm(formula@formula, data = df, family = eval(family))
                  fit <- broom::tidy(anova(model, test = "Chisq"))
                },
                kruskal = {
                  fit <- broom::tidy(
                    kruskal.test(formula@formula, data = df)
                  ) # Keep here the restriction for respone ~ predictor
                }
              )
              if (is.null(fit)) { # This covers all the emmeans post hoc tests
                family <- formula@family
                link_fct <- formula@link_fct
                family <- str2lang(paste0("stats::", family, "(\"", link_fct, "\")"))

                f_split <- OpenStats:::env_utils_V1_2$split_formula(formula@formula)
                rhs_vars <- OpenStats:::env_utils_V1_2$vars_rhs(f_split$right_site)
                df_temp <- OpenStats:::env_utils_V1_2$num_to_factor(df, rhs_vars)
                if (any(apply(df, 2, is.numeric))) {
                  warning(paste0("Found numeric predictors and converted them to factors"))
                }
                model <- glm(formula@formula, data = df_temp, family = eval(family))
                emm <- emmeans::emmeans(model, rhs_vars)
                fit <- pairs(emm, adjust = method)
                fit <- as.data.frame(fit)
              }
              fit
            },
            args = list( formula = self$formula, df = self$df, method = method),
              promise_result_name = new_name,
              promise_history_entry = promise_history_entry,
              in_background = TRUE, ResultsState
          )
        })
    },

    eval = function(ResultsState, method) {
      e <- try({
        self$indep <- as.character(self$formula@formula)[3]
        self$dep <- as.character(self$formula@formula)[2]
      })
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        err <- paste0(err, "\n", "Could not use Formula")
        self$com$print_err(err)
        stop(err)
      }

      fit <- NULL
      history_data <- NULL
      withCallingHandlers(
        {
          new_name <- paste0( ResultsState$counter, " Test ", method)
          if (inherits(self$formula, "LinearFormula")) {
            self$eval_lm(method, new_name, ResultsState)
          } else if (inherits(self$formula, "GeneralisedLinearFormula")) {
            self$eval_glm(method, new_name, ResultsState)
          }
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
    },

    create_history_lm = function(new_name, method) {
      switch(method,
        aov = {
          history_data <- list(
            type = "ANOVA", formula = deparse(self$formula@formula), "Result name" = new_name
          )
        },
        kruskal = {
          history_data <- list(
            type = "Kruskal-Wallis Test", formula = deparse(self$formula@formula), "Result name" = new_name
          )
        },
        HSD = {
          check_formula(self$formula@formula)
          bal <- self$balanced_design
          req(bal)
          if (bal == "Balanced") {
            bal <- TRUE
          } else {
            bal <- FALSE
          }
          history_data <- list(
            type = "Tukey HSD",
            formula = deparse(self$formula@formula),
            "Balanced design" = bal,
            "P-value" = self$p_val, "Result name" = new_name
          )
        },
        kruskalTest = {
          check_formula(self$formula@formula)
          history_data <- list(
            type = "Kruskal Wallis post hoc test",
            formula = deparse(self$formula@formula),
            "Adjusted p value method" = self$p_val_adj_method,
            "P-value" = self$p_val, "Result name" = new_name
          )
        },
        LSD = {
          check_formula(self$formula@formula)
          history_data <- list(
            type = "Least significant difference test",
            formula = deparse(self$formula@formula),
            "Adjusted p value method" = self$p_val_adj_method,
            "P-value" = self$p_val, "Result name" = new_name
          )
        },
        scheffe = {
          check_formula(self$formula@formula)
          history_data <- list(type = "Scheffe post hoc test",
            formula = deparse(self$formula@formula),
            "P-value" = self$p_val, "Result name" = new_name
          )
        },
        REGW = {
          check_formula(self$formula@formula)
          history_data <- list(
            type = "REGW post hoc test",
            formula = deparse(self$formula@formula),
            "P-value" = self$p_val, "Result name" = new_name
          )
        }
      )
    },
    create_history_glm = function(new_name, method) {
      history_data <- NULL
      switch(method,
        aov = {
          history_data <- list(
            type = "ANOVA-Chisq", formula = deparse(self$formula@formula),
            family = self$formula@family, link_fct = self$formula@link_fct,
            "Result name" = new_name
          )
        },
        kruskal = {
          history_data <- list(
            type = "Kruskal-Wallis Test", formula = deparse(self$formula@formula), "Result name" = new_name
          )
        }
      )
      if (is.null(history_data)) {
        history_data <- list(type = paste0("GLM PostHoc adjusted by: ", method),
          formula = deparse(self$formula@formula), family = self$formula@family,
          link_fct = self$formula@link_fct, "Result name" = new_name
        )
      }
      history_data
    },
    create_history = function(new_name, method) {
      if (inherits(self$formula, "LinearFormula")) {
        self$create_history_lm(new_name, method)
      } else if (inherits(self$formula, "GeneralisedLinearFormula")) {
        self$create_history_glm(new_name, method)
      }
    }
  )
)

remove_result_V1_2 <- R6::R6Class(
  "remove_result_V1_2",
  public = list(
    name = NULL,
    initialize = function(name) {
      self$name <- name
    },
    validate = function() {},
    eval = function(ResultsState) {
      current_list <- ResultsState$all_data
      current_list[[self$name]] <- NULL
      ResultsState$history[[length(ResultsState$history) + 1]] <- list(
        type = "RemoveResult", "Entry removed" = self$name
      )
      ResultsState$all_data <- current_list
    }
  )
)
set_active_table_V1_2 <- R6::R6Class(
  "set_active_table_V1_2",
  public = list(
    name = NULL,
    initialize = function(name) {
      self$name <- name
    },
    validate = function() {},
    eval = function(ResultsState, DataModelState) {
      df <- ResultsState$all_data[[self$name]]
      DataModelState$df <- df
      DataModelState$active_df_name <- self$name
      ResultsState$history[[length(ResultsState$history) + 1]] <- list(
        type = "SetActiveTable",
        "NewActiveTable" = self$name
      )
    }
  )
)

replay_history_V1_2 <- R6::R6Class(
  "replay_history_V1_2",
  public = list(
    json_string = NULL,
    df = NULL,
    all_data = NULL,
    initialize = function(history_string, df, all_data) {
      self$json_string <- history_string
      self$df <- df
      self$all_data <- all_data
    },
    validate = function() {},
    eval = function(ResultsState) {
      withCallingHandlers(
        expr = {
          ResultsState$bgp$start(
            fun = function(json_string, df, all_data) {
              l <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
              version = l[[1]]
              stopifnot(version$type == "Version")
              stopifnot(version$Nr %in% OpenStats:::get_available_versions())
              l <- l[-1] # Remove version step as it is not evaluated
              eval_entry <- OpenStats:::get_correct_eval(version$Nr)[[1]]
              result_state <- OpenStats:::get_correct_result_state(version$Nr)[[1]]$new(all_data)
              result_state$bgp$in_backend <- TRUE
              data_model_state <- OpenStats:::get_correct_data_model_state(version$Nr)[[1]]$new(df)
              data_wrangling_state <- OpenStats:::get_correct_data_wrangling_state(version$Nr)[[1]]$new(data_model_state)
              get_result <- OpenStats:::get_correct_get_result_fct(version$Nr)[[1]]
              for (i in seq_along(l)) {
                inner_e <- try({
                  if (l[[i]]$type == "ModelSummary") {
                    next
                  }
                  eval_entry(l[[i]], data_model_state, data_wrangling_state, result_state, get_result)
                })
                if (inherits(inner_e, "try-error")) {
                  err <- conditionMessage(attr(inner_e, "condition"))
                  print(sprintf("Error in step: %s", i + 1))
                  stop(err)
                }
              }
              return(
                structure(
                  list(
                    ResultsState = result_state,
                    DataModelState = data_model_state,
                    DataWranglingState = data_wrangling_state
                  ),
                  class = "HistoryReplayResult"
                )
              )
            },
            args = list(json_string = self$json_string, df = self$df, all_data = self$all_data),
            promise_result_name = "History",
            promise_history_entry = "History"
          )
        },
        warning = function(warn) {
          self$com$print_warn(warn$message)
          invokeRestart("muffleWarning")
        }
      )
    }
  )
)
