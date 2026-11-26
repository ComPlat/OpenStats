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
    # The LLM tools cannot see the notifications
    last_error = NULL,

    disable = function() {
      if (self$in_backend) return()
      # Here the buttons of the current tab are disabled instantly.
      # The other disable runs have no effect here.
      # Therefore,an observe is running in the main server to disable them
      shinyjs::disable("CreatePlotBox")
      shinyjs::disable("CreatePlotScatter")
      shinyjs::disable("CreatePlotLine")
      shinyjs::disable("CreateModelBox")
      shinyjs::disable("CreateModelScatter")
      shinyjs::disable("CreateModelLine")

      shinyjs::disable("ic50")

      shinyjs::disable("replay_history")

      shinyjs::disable("DiagnosticPlot")

      shinyjs::disable("PermANOVATest")
    },
    enable = function() {
      if (self$in_backend) return()
      shinyjs::enable("VIS-CreatePlotBox")
      shinyjs::enable("VIS-CreatePlotScatter")
      shinyjs::enable("VIS-CreatePlotLine")
      shinyjs::enable("VIS-CreateModelBox")
      shinyjs::enable("VIS-CreateModelScatter")
      shinyjs::enable("VIS-CreateModelLine")

      shinyjs::enable("DOSERESPONSE-ic50")

      shinyjs::enable("HISTORY-replay_history")

      shinyjs::enable("ASS-DiagnosticPlot")

      shinyjs::enable("TESTS-PermANOVATest")
    },

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
          self$enable()
        }
        else if (inherits(res, "error") && !ResultsState$bgp$cancel_clicked) {
          ResultsState$bgp$running_status <- sprintf("Error failed with: %s", res$parent$message)
          ResultsState$bgp$com$print_err(res$parent$message)
        }
        else {
          ResultsState$bgp$warnings <- ResultsState$bgp$process$read_error()
          if (ResultsState$bgp$warnings != "") {
            ResultsState$bgp$com$print_warn(ResultsState$bgp$warnings)
          }
          e <- try(env_utils_V1_2$check_rls(ResultsState$all_data, res), silent = TRUE)
          if (inherits(e, "try-error")) {
            self$com$print_err(conditionMessage(e))
            self$enable()
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
            shiny::exportTestValues(
              result_list = ResultsState$all_data
            )
          }
        }

        ResultsState$bgp$process <- NULL
        ResultsState$bgp$is_running <- FALSE
        ResultsState$bgp$promise_result_name <- NULL
        ResultsState$bgp$promise_history_entry <- NULL
        ResultsState$bgp$result_val <- NULL
        ResultsState$bgp$warnings <- NULL
        self$enable()
      }
    },

    init = function(ResultsState, DataModelState, DataWranglingState) {
      # Polling loop
      shiny::observe({
        shiny::invalidateLater(250)
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
          shiny::exportTestValues(
            result_list = ResultsState$all_data
          )
        }
      } else {
        self$last_error <- attributes(res)$condition$message
        self$com$print_err(self$last_error)
      }
    },

    start = function(fun, args = list(), promise_result_name, promise_history_entry,
                     in_background = TRUE, ResultsState = NULL) {
      # NOTE: ResultsState is required to store the results when no background process is used.
      # Therefore, default is NULL
      background <- getOption("OpenStats.background", TRUE)
      if (!in_background || !background) {
        shiny::req(!is.null(ResultsState), "`ResultsState` must be provided when not running in background.")
        self$start_direct(fun, args, promise_result_name, promise_history_entry, ResultsState)
        return()
      }
      shiny::req(is.null(self$process) || !self$process$is_alive())
      self$disable()
      self$promise_result_name <- promise_result_name
      self$promise_history_entry <- promise_history_entry
      self$is_running <- TRUE
      r6_args <- names(args)[sapply(args, function(x) inherits(x, "R6"))]
      shiny::req(length(r6_args) == 0, paste("Cannot pass R6 objects to background process:", paste(r6_args, collapse = ", ")))
      self$result_val <- NULL
      environment(fun) <- baseenv()
      # NOTE: Formulas capture the environment in which they are created.
      # In the Shiny server that environment holds ResultsState, so callr serializes
      # the entire ResultsState into the background process on every call. Stripping to baseenv() prevents this.
      # S4 formula storage classes (LinearFormula, GeneralisedLinearFormula, etc.) carry the same risk via their formula slot.
      args <- lapply(args, function(x) {
        if (inherits(x, "formula")) {
          environment(x) <- baseenv()
        } else if (isS4(x) && "formula" %in% slotNames(x)) {
          f <- slot(x, "formula")
          environment(f) <- baseenv()
          slot(x, "formula") <- f
        }
        x
      })
      self$process <- callr::r_bg(fun, args = args)
      self$running_status <- "Running..."
    },

    cancel = function() {
      shiny::req(!is.null(self$process), self$process$is_alive())
      # Actually kill the process (and any children it spawned) now, rather
      # than just flagging cancel_clicked=TRUE for tick() to notice: tick()
      # only acts once the process is no longer alive, so the previous
      # behaviour waited for the computation to finish naturally before
      # doing anything -- the button didn't save any time.
      self$process$kill_tree()
      self$running_status <- "Canceled"
      self$is_running <- FALSE
      self$process <- NULL
      self$promise_result_name <- NULL
      self$promise_history_entry <- NULL
      self$result_val <- NULL
      self$enable()
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

    description = paste(
      "Correlation test between the response and the single numeric predictor",
      "of the linear formula currently defined in the app (response ~ predictor).",
      "Do not pass a formula; check it with get_state and use create_formula if needed."
    ),
    params = list(
      method = list(
        type = "enum",
        values = c("pearson", "spearman", "kendall"),
        default = "pearson",
        description = "pearson for linear correlation, spearman or kendall for rank based correlation."
      ),
      alternative = list(
        type = "enum",
        values = c("two.sided", "less", "greater"),
        default = "two.sided",
        description = "Alternative hypothesis."
      ),
      conflevel = list(
        type = "number",
        default = 0.95,
        description = "Confidence level of the interval, between 0 and 1."
      )
    ),

    initialize = function(df, formula, method, alternative, conflevel, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula
      self$method <- method
      self$alternative <- alternative
      self$conflevel <- conflevel
      self$com <- com$new()
    },
    validate = function() {
      stopifnot(
        "Formula is not of type linear" = inherits(self$formula, "LinearFormula"),
        "method has to be pearson, spearman or kendall" =
          isTRUE(self$method %in% c("pearson", "spearman", "kendall")),
        "alternative has to be two.sided, less or greater" =
          isTRUE(self$alternative %in% c("two.sided", "less", "greater")),
        "conflevel has to be a number between 0 and 1" =
          is.numeric(self$conflevel) && length(self$conflevel) == 1L &&
          !is.na(self$conflevel) && self$conflevel > 0 && self$conflevel < 1
      )
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

# Labels are parsed as plotmath: "Origin (Type)" is shown as "Origin(Type)"
llm_label_hint_V1_2 <- paste(
  "Empty uses the column name. Labels are parsed as R plotmath:",
  "use plain words without brackets, * or spaces before (,",
  "e.g. 'Origin' instead of 'Origin (Type)'; CO[2] shows a subscript."
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

    description = paste(
      "Plot two columns of the active dataset as boxplot, scatterplot or lineplot.",
      "Groups can be shown by colour, fill (boxplot only) or split into subplots.",
      "No formula needed."
    ),
    params = list(
      method = list(
        type = "enum",
        values = c("box", "dot", "line"),
        description = "box for a boxplot, dot for a scatterplot, line for a lineplot."
      ),
      x = list(type = "string", description = "Column shown on the x axis."),
      y = list(type = "string", description = "Column shown on the y axis."),
      type_of_x = list(
        type = "enum",
        values = c("factor", "numeric"),
        default = "factor",
        description = "Treat x as groups (factor) or as continuous axis (numeric)."
      ),
      xlabel = list(type = "string", default = "", description = paste("X axis label.", llm_label_hint_V1_2)),
      ylabel = list(type = "string", default = "", description = paste("Y axis label.", llm_label_hint_V1_2)),
      colour_var = list(type = "string", default = "", description = "Column whose groups get different colours. Empty for none."),
      colour_legend_title = list(type = "string", default = "", description = paste("Legend title for colour.", llm_label_hint_V1_2)),
      colour_theme = list(
        type = "enum",
        values = c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
        default = "Set1",
        description = "Colour palette."
      ),
      fill_var = list(type = "string", default = "", description = "Boxplot only: column whose groups get different fill colours. Empty for none."),
      fill_legend_title = list(type = "string", default = "", description = paste("Legend title for fill.", llm_label_hint_V1_2)),
      fill_theme = list(
        type = "enum",
        values = c("BuGn", "PuRd", "YlOrBr", "Greens", "GnBu", "Reds", "Oranges", "Greys"),
        default = "BuGn",
        description = "Fill palette."
      ),
      facet_var = list(type = "string", default = "", description = "Column used to split the plot into subplots. Empty for none."),
      facet_y_scaling = list(
        type = "enum",
        values = c("free", "fixed"),
        default = "free",
        description = "Scaling of the y axis in subplots."
      ),
      xrange = list(
        type = "array",
        default = character(),
        description = "Two values: start and end of the x axis (numbers or levels). Empty uses the whole range. Ignored with subplots."
      ),
      yrange = list(
        type = "array",
        default = character(),
        description = "Two numbers: start and end of the y axis. Empty uses the whole range. Ignored with subplots."
      ),
      width = list(type = "number", default = 10, description = "Width of the plot in cm."),
      height = list(type = "number", default = 10, description = "Height of the plot in cm."),
      resolution = list(type = "number", default = 300, description = "Resolution of the plot in dpi.")
    ),

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
                p <- OpenStats:::env_plotting_V1_2$lineplot_fct(
                  df, x, y, xlabel, ylabel,
                  colour_var, colour_legend_title, colour_theme,
                  facet_mode, facet_var, facet_y_scaling,
                  xrange_min, xrange_max, yrange_min, yrange_max
                )
              }
            },
            warning = function(warn) {
              self$com$print_warn(warn$message)
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
        in_background = FALSE, ResultsState
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

    description = paste(
      "Plot the raw data structured by the formula currently defined in the app:",
      "response on the y axis, predictors as x axis, colour and subplots.",
      "Random effects of mixed models are ignored."
    ),
    params = list(
      layer = list(
        type = "enum",
        values = c("box", "dot", "line"),
        default = "dot",
        description = "How the data is shown: box for boxplot, dot for points, line for lines."
      )
    ),

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
            in_background = FALSE, ResultsState
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
hist_V1_2 <- R6::R6Class(
  "hist_V1_2",
  public = list(
    com = NULL,
    df = NULL,
    y = NULL,
    ylabel = NULL,
    fill_var = NULL,
    fill_legend_title = NULL,
    fill_theme = NULL,
    facet_var = NULL,
    facet_mode = NULL,
    facet_y_scaling = NULL,
    frequency_or_density = NULL,
    bins = NULL,
    width = NULL,
    height = NULL,
    resolution = NULL,
    col_names = NULL,

    description = paste(
      "Histogram or density plot of one numeric column of the active dataset.",
      "Groups can be shown by fill colour or split into subplots. No formula needed."
    ),
    params = list(
      y = list(type = "string", description = "Numeric column to plot."),
      ylabel = list(type = "string", default = "", description = paste("Axis label.", llm_label_hint_V1_2)),
      fill_var = list(type = "string", default = "", description = "Column whose groups get different colours. Empty for none."),
      fill_legend_title = list(type = "string", default = "", description = paste("Legend title for fill.", llm_label_hint_V1_2)),
      fill_theme = list(
        type = "enum",
        values = c("BuGn", "PuRd", "YlOrBr", "Greens", "GnBu", "Reds", "Oranges", "Greys"),
        default = "BuGn",
        description = "Fill palette."
      ),
      facet_var = list(type = "string", default = "", description = "Column used to split the plot into subplots. Empty for none."),
      facet_y_scaling = list(
        type = "enum",
        values = c("free", "fixed"),
        default = "free",
        description = "Scaling of the y axis in subplots."
      ),
      frequency_or_density = list(
        type = "enum",
        values = c("frequency", "density"),
        default = "frequency",
        description = "frequency for a histogram, density for a density curve."
      ),
      bins = list(type = "integer", default = 30L, description = "Number of bins of the histogram."),
      width = list(type = "number", default = 10, description = "Width of the plot in cm."),
      height = list(type = "number", default = 10, description = "Height of the plot in cm."),
      resolution = list(type = "number", default = 300, description = "Resolution of the plot in dpi.")
    ),

    initialize = function(df, y, ylabel,
                          fill_var, fill_legend_title, fill_theme,
                          facet_var, facet_y_scaling,
                          frequency_or_density, bins,
                          width, height, resolution,
                          com = communicator_V1_2) {
      self$com <- com$new()

      # User input
      self$df <- df
      self$y <- y
      self$ylabel <- ylabel
      self$fill_var <- fill_var
      self$fill_legend_title <- fill_legend_title
      self$fill_theme <- fill_theme
      self$facet_var <- facet_var
      self$facet_y_scaling <- facet_y_scaling
      self$frequency_or_density <- frequency_or_density
      self$bins <- bins
      self$width <- width
      self$height <- height
      self$resolution <- resolution
      # Additional information based on user input
      self$facet_mode <- "none"
      if (self$facet_var != "") {
        self$facet_mode <- "facet_wrap"
      }
      self$col_names <- names(self$df)
      self$com = com$new()
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
      if (is.null(self$frequency_or_density) || !(self$frequency_or_density %in% c("frequency", "density"))) {
        self$com$print_warn("You have to set either frequency or density")
        stop("You have to set either frequency or density")
      }
    },
    eval = function(ResultsState) {
      new_result_name <- paste0(ResultsState$counter + 1, " Histogram ")
      promise_history_entry <- self$create_history(new_result_name)
      ResultsState$bgp$start(
        fun = function(df, y, ylabel,
                       fill_var, fill_legend_title, fill_theme,
                       facet_var, facet_mode, facet_y_scaling,
                       frequency_or_density, bins,
                       width, height, resolution) {
          withCallingHandlers(
            {
              p <- OpenStats:::env_plotting_V1_2$histplot_fct(
                df, y, ylabel,
                fill_var, fill_legend_title, fill_theme,
                facet_var, facet_mode, facet_y_scaling,
                frequency_or_density, bins
              )
            },
            warning = function(warn) {
              self$com$print_warn(warn$message)
              invokeRestart("muffleWarning")
            }
          )
          ggplot2::ggplot_build(p) # NOTE: invokes errors and warnings by building but not rendering plot
          new("plot", p = p, width = width, height = height, resolution = resolution)
        },
        args = list(
          self$df, self$y, self$ylabel,
          self$fill_var, self$fill_legend_title, self$fill_theme,
          self$facet_var, self$facet_mode, self$facet_y_scaling,
          self$frequency_or_density, self$bins,
          self$width, self$height, self$resolution
        ),
        promise_result_name = new_result_name,
        promise_history_entry = promise_history_entry,
        in_background = FALSE, ResultsState
      )
    },
    create_history = function(new_result_name) {
      list(
        type = "Histogram",
        y = self$y, "Y axis label" = self$ylabel,
        "Fill variable" = self$fill_var, "Legend title for fill" = self$fill_legend_title, "Fill theme" = self$fill_theme,
        "Split by" = self$facet_var, "Split in subplots" = self$facet_mode, "How to scale y in subplots" = self$facet_y_scaling,
        "Frequency or Density" = self$frequency_or_density, "Number of Bins" = self$bins,
        Width = self$width, Height = self$height, Resolution = self$resolution,
        "Result name" = new_result_name
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
    group_apply = NULL,

    description = paste(
      "Computes an intermediate variable from the active dataset, e.g. Mean(uptake)",
      "or a subset get_rows(df, Type == 'Quebec'), optionally per group.",
      "It can be used in later operations by its name; a subset can be activated",
      "with set_active_table. Only the functions listed by choose_toolbox data are allowed."
    ),
    params = list(
      operation = list(
        type = "string",
        description = "Expression, e.g. Mean(uptake), Max(conc) - Min(conc) or get_rows(df, Treatment == 'chilled')."
      ),
      # Not called `name`: it would shadow the tool name inside llm_make_tool_V1_2
      var_name = list(
        type = "string",
        description = "Name of the new intermediate variable; must not be a column name."
      ),
      group_apply = list(
        type = "array",
        default = character(),
        description = "Grouping columns; the operation is evaluated per group. Empty for no grouping."
      )
    ),

    initialize = function(df, df_name, intermediate_vars, operation, name, group_apply, com = communicator_V1_2) {
      self$df <- df
      self$df_name <- df_name
      self$intermediate_vars <- intermediate_vars
      self$operation <- operation
      self$name <- name
      self$group_apply <- group_apply
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
      }, silent = TRUE)
      if (inherits(e, "try-error")) {
        self$com$print_err(e)
        stop("Invalid ast")
      }
    },

    eval = function(ResultsState, DataWranglingState) {
      e <- try({
        if (!is.null(self$group_apply)) {
          eval_env <- create_run_env()
          list2env(self$intermediate_vars, envir = eval_env)
          ints <- interaction(DataWranglingState$df[, self$group_apply])
          groups_col_names <- paste0(self$group_apply, collapse = "_")
          new <- lapply(unique(ints), function(g) {
            block <- DataWranglingState$df[ints == g, ]
            list2env(block, envir = eval_env) # NOTE: this adds each column as own variable
            eval_env[[DataWranglingState$df_name]] <- block
            temp <- withCallingHandlers(
              eval(parse(text = self$operation), envir = eval_env),
              warning = function(w) { self$com$print_warn(w$message); invokeRestart("muffleWarning") }
            )
            check_type_res(temp)
            env_utils_V1_2$check_rls(ResultsState$all_data, temp)
            res <- data.frame(name = g)
            names(res)[[1L]] <- groups_col_names
            if (length(temp) > 1L) {
              if (is.data.frame(temp)) {
                res <- cbind(res, temp)
              } else {
                if (length(names(temp)) == length(temp)) {
                  res <- cbind(res, as.data.frame(t(temp)))
                } else {
                  for (i in seq_len(length(temp))) {
                    res[[paste0("value", i)]] <- temp[[i]]
                  }
                }
              }
            } else {
              if (is.data.frame(temp)) {
                res <- cbind(res, temp)
              } else {
                if (length(names(temp)) == length(temp)) {
                  res <- cbind(res, as.data.frame(t(temp)))
                } else {
                  res <- data.frame(name = g, value = temp)
                  names(res)[[1L]] <- groups_col_names
                }
              }
            }
            res
          })
          new <- Reduce(rbind, new)
          new <- new("summaryDataFrame", summary = new)
          new
        } else {
          eval_env <- create_run_env()
          list2env(self$intermediate_vars, envir = eval_env)
          list2env(DataWranglingState$df, envir = eval_env) # NOTE: this adds each column as own variable
          eval_env[[DataWranglingState$df_name]] <- self$df
          new <- withCallingHandlers(
            eval(parse(text = self$operation), envir = eval_env),
            warning = function(w) { self$com$print_warn(w$message); invokeRestart("muffleWarning") }
          )
          check_type_res(new)
          env_utils_V1_2$check_rls(ResultsState$all_data, new)
        }
      }, silent = TRUE)
      if (inherits(e, "try-error")) {
        err <- conditionMessage(attr(e, "condition"))
        self$com$print_err(err)
        stop("Error in create intermediate variable")
      } else {
        DataWranglingState$intermediate_vars[[self$var_name]] <- new

        ResultsState$counter <- ResultsState$counter + 1
        # NOTE: for better saving the results
        if (inherits(new, "summaryDataFrame")) {
          new_df <- new
        } else if (!inherits(new, "data.frame")) {
          new_df <- setNames(data.frame(new), self$name)
        } else {
          new_df <- new
        }
        ResultsState$all_data[[paste0(ResultsState$counter, " ", self$var_name)]] <- new_df
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "CreateIntermediateVariable",
          operation = self$operation,
          groups = paste(self$group_apply, collapse = ", "),
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

    description = paste(
      "Adds a new column to the active dataset, e.g. log(conc) or uptake / Mean(uptake).",
      "Columns and intermediate variables are available by name.",
      "Only the functions listed by choose_toolbox data are allowed."
    ),
    params = list(
      operation = list(
        type = "string",
        description = "R expression that returns one value or one value per row."
      ),
      column_name = list(
        type = "string",
        description = "Name of the new column; must not exist yet."
      )
    ),

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
      }, silent = TRUE)
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
        new <- withCallingHandlers(
          eval(parse(text = self$operation), envir = eval_env),
          warning = function(w) { self$com$print_warn(w$message); invokeRestart("muffleWarning") }
        )
        check_type_res(new)
        env_utils_V1_2$check_rls(ResultsState$all_data, new)
        self$df[, self$name] <- new
        DataWranglingState$df <- self$df
        if (!is.null(DataModelState$backup_df)) {
          eval_env <- create_run_env()
          list2env(self$intermediate_vars, envir = eval_env)
          list2env(DataModelState$backup_df, envir = eval_env)  # NOTE: this adds each column as own variable
          new <- withCallingHandlers(
            eval(parse(text = self$operation), envir = eval_env),
            warning = function(w) { self$com$print_warn(w$message); invokeRestart("muffleWarning") }
          )
          check_type_res(new)
          DataModelState$backup_df[, self$name] <- new
          self$com$print_warn("Conducted operation also for entire dataset and not only for the subset")
        }
      }, silent = TRUE)
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

create_summary_V1_2 <- R6::R6Class(
  "create_summary_V1_2",
  public = list(
    df = NULL,
    column_by = NULL,
    for_which = NULL,
    what_to_do = NULL,
    com = NULL,

    description = paste(
      "Summarises columns of the active dataset per group,",
      "e.g. the mean and SD of uptake for each Treatment.",
      "Does not need a formula."
    ),
    params = list(
      column_by = list(
        type = "array",
        description = "Grouping columns; the data is summarised for each combination of their levels."
      ),
      for_which = list(
        type = "array",
        description = "Numeric columns that are summarised."
      ),
      what_to_do = list(
        type = "array",
        values = c("Mean", "Median", "SD"),
        description = "Summary functions applied to each column."
      )
    ),

    initialize = function(df, column_by, for_which, what_to_do, com = communicator_V1_2) {
      self$df <- df
      self$column_by <- column_by
      self$for_which <- for_which
      self$what_to_do <- what_to_do
      self$com <- com$new()
    },

    validate = function() {
      check_is_invalid <- function(a) {
        if (length(a) == 0 || (length(a) == 1L && a == "")) return(TRUE)
        return(FALSE)
      }
      if (check_is_invalid(self$column_by)) {
        stop("You have to define at least one column across which to summarize the data")
      }
      if (check_is_invalid(self$for_which)) {
        stop("You have to define at least one column across which should be summarized")
      }
      if (check_is_invalid(self$what_to_do)) {
        stop("You have to set at least one operation")
      }
      # what_to_do is pasted into code in eval
      if (!all(self$what_to_do %in% c("Mean", "Median", "SD"))) {
        stop("Operations have to be Mean, Median or SD")
      }
      cols <- c(self$column_by, self$for_which)
      if (!all(cols %in% names(self$df))) {
        stop("Unknown columns: ", paste(setdiff(cols, names(self$df)), collapse = ", "))
      }
    },

    eval = function(ResultsState) {
      new_name <- paste0(ResultsState$counter + 1, " Dataset summary")
      promise_history_entry <- self$create_history(new_name)
      withCallingHandlers(
        {
          expr = {
            ResultsState$bgp$start(
              fun = function(df, column_by, for_which, what_to_do) {
                ints <- interaction(df[, column_by])
                res <- lapply(unique(ints), function(i) {
                  block <- df[ints == i, for_which, drop = FALSE]
                  m <- matrix(NA, nrow = length(what_to_do), ncol = ncol(block))
                  for (col in seq_len(ncol(block))) {
                    for (op in seq_len(length(what_to_do))) {
                      eval_env <- create_run_env()
                      eval_env$col <- block[, col]
                      operation <- paste0(what_to_do[[op]], "(col)")
                      m[op, col] <- eval(parse(text = operation), eval_env)
                    }
                  }
                  m <- as.data.frame(m)
                  names(m) <- for_which
                  m$operations <- what_to_do
                  m[paste0(column_by, collapse = "_")] <- as.character(levels(i)[i])
                  m
                })
                res <- Reduce(rbind, res)
                new("summaryDataFrame", summary = res)
              },
              args = list(df = self$df, column_by = self$column_by, for_which = self$for_which, what_to_do = self$what_to_do),
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

    create_history = function(new_name) {
      list(
        type = "DatasetSummary",
        column_by = paste0(self$column_by, collapse = ","),
        for_which = paste0(self$for_which, collapse = ","),
        what_to_do = paste0(self$what_to_do, collapse = ","),
        "Result name" = new_name
      )
    }
  )
)

create_summary_plot_V1_2 <- R6::R6Class(
  "create_summary_plot_V1_2",
  public = list(
    df = NULL,
    column_by = NULL,
    for_which = NULL,
    com = NULL,

    description = paste(
      "Boxplots of columns of the active dataset per group.",
      "The plot is shown in the app; you only get its name.",
      "Does not need a formula."
    ),
    params = list(
      column_by = list(
        type = "array",
        description = "Grouping columns shown on the x axis."
      ),
      for_which = list(
        type = "array",
        description = "Numeric columns; one boxplot per column."
      )
    ),

    initialize = function(df, column_by, for_which, com = communicator_V1_2) {
      self$df <- df
      self$column_by <- column_by
      self$for_which <- for_which
      self$com <- com$new()
    },

    validate = function() {
      check_is_invalid <- function(a) {
        if (length(a) == 0 || (length(a) == 1L && a == "")) return(TRUE)
        return(FALSE)
      }
      if (check_is_invalid(self$column_by)) {
        stop("You have to define at least one column across which to summarize the data")
      }
      if (check_is_invalid(self$for_which)) {
        stop("You have to define at least one column across which should be summarized")
      }
      cols <- c(self$column_by, self$for_which)
      if (!all(cols %in% names(self$df))) {
        stop("Unknown columns: ", paste(setdiff(cols, names(self$df)), collapse = ", "))
      }
    },

    eval = function(ResultsState) {
      new_name <- paste0(ResultsState$counter + 1, " Dataset summary plot")
      promise_history_entry <- self$create_history(new_name)
      withCallingHandlers(
        {
          expr = {
            ResultsState$bgp$start(
              fun = function(df, column_by, for_which) {

                summary_plot <- function(df, for_which, column_by) {
                  df$INTERACTION_INTERNALLY_ <- interaction(df[, column_by], sep = " + ")
                  xlab <- paste0(column_by, collapse = " + ")
                  ggplot2::ggplot(
                    df, ggplot2::aes(y = .data[[for_which]], x = INTERACTION_INTERNALLY_)
                  ) +
                    ggplot2::geom_boxplot() +
                    ggplot2::xlab(xlab) +
                    ggplot2::theme(text = ggplot2::element_text(size = 12),
                      axis.text.x = ggplot2::element_text(angle = 90))
                }
                res <- lapply(for_which, function(i) {
                  summary_plot(df, i, column_by)
                })
                p <- cowplot::plot_grid(plotlist = res, nrow = length(res))
                new("summaryPlotDataFrame", p = p)
              },
              args = list(df = self$df, column_by = self$column_by, for_which = self$for_which),
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

    create_history = function(new_name) {
      list(
        type = "DatasetSummaryPlot",
        column_by = paste0(self$column_by, collapse = ","),
        for_which = paste0(self$for_which, collapse = ","),
        "Result name" = new_name
      )
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
                model <- glm(self$formula@formula, data = self$df, family = OpenStats:::env_utils_V1_2$build_glm_family(family, link_fct))
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

    eval_linear_mixed = function(ResultsState, new_name, promise_history_entry) {
      withCallingHandlers(
        {
          expr = {
            ResultsState$bgp$start(
              fun = function(df, formula) {
                p <- OpenStats:::env_summarising_model_V1_2$plot_pred_linear_mixed(df, formula)
                ggplot2::ggplot_build(p)
                p <- new("plot", p = p, width = 15, height = 15, resolution = 600)
                model <- lmerTest::lmer(formula@formula, data = df)
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

    eval = function(ResultsState) {
      new_name <- paste0(ResultsState$counter + 1, " Model summary")
      promise_history_entry <- self$create_history(new_name)
      if (inherits(self$formula, "LinearFormula")) {
        res <- self$eval_lm(ResultsState, new_name, promise_history_entry)
      } else if (inherits(self$formula, "GeneralisedLinearFormula")) {
        res <- self$eval_glm(ResultsState, new_name, promise_history_entry)
      } else if (inherits(self$formula, "OptimFormula")) {
        res <- self$eval_optim(ResultsState, new_name, promise_history_entry)
      } else if (inherits(self$formula, "LinearMixedFormula")) {
        res <- self$eval_linear_mixed(ResultsState, new_name, promise_history_entry)
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

    description = paste(
      "Defines the model (formula) that translates the scientific question",
      "into a statistical model. All later analyses (e.g. t_test) use this formula.",
      "Use only column names shown by get_state.",
      "Returns the model summary."
    ),
    params = list(
      response = list(
        type = "string",
        description = "Name of the column with the measured outcome, e.g. uptake."
      ),
      predictors = list(
        type = "string",
        description = paste(
          "Right-hand side of the formula without ~, e.g. Treatment or Treatment * Type.",
          "Allowed: column names, + for main effects, * for main effects with interaction,",
          ": for an interaction only, and functions like log or sqrt.",
          "Linear Mixed Model: random effects like (1 | Plant).",
          "Optimization Model: an expression with unknown parameters, e.g. a * exp(b * conc)."
        )
      ),
      model_type = list(
        type = "enum",
        values = c("Linear", "Generalised Linear Model", "Linear Mixed Model", "Optimization Model"),
        default = "Linear",
        description = "Type of the model."
      ),
      family = list(
        type = "enum",
        values = c("binomial", "gaussian", "Gamma", "inverse.gaussian", "poisson", "quasi", "quasibinomial", "quasipoisson"),
        default = "gaussian",
        description = "Only for Generalised Linear Model: distribution of the response."
      ),
      link_fct = list(
        type = "string",
        default = "identity",
        description = "Only for Generalised Linear Model: link function, e.g. identity, log, logit, probit, inverse."
      ),
      optim_method = list(
        type = "enum",
        values = c("general purpose optimization", "nonlinear least squares"),
        default = "general purpose optimization",
        description = "Only for Optimization Model."
      ),
      lower = list(
        type = "number",
        default = 0,
        description = "Only for Optimization Model: lower boundary of the parameters."
      ),
      upper = list(
        type = "number",
        default = 100,
        description = "Only for Optimization Model: upper boundary of the parameters."
      ),
      seed = list(
        type = "integer",
        default = 1234L,
        description = "Only for Optimization Model: seed for the random start values."
      )
    ),

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
        env_check_ast_V1_2$check_formula_rhs(formula[[3]])
        DataModelState$formula <- new("LinearFormula", formula = formula)
        model <- lm(formula, data = self$df)
        eq <- equatiomatic::extract_eq(model, wrap = TRUE)
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
        env_check_ast_V1_2$check_formula_rhs(formula[[3]])
        DataModelState$formula <- new("GeneralisedLinearFormula",
          formula = formula, family = family, link_fct = link_fct
        )
        model <- glm(formula, data = self$df, family = env_utils_V1_2$build_glm_family(family, link_fct))
        eq <- equatiomatic::extract_eq(model, wrap = TRUE)
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "CreateFormula",
          formula = deparse(formula),
          "Model Type" = "Generalised Linear Model",
          family = details[[1]],
          "Link function" = link_fct
        )
      } else if (model_type == "Optimization Model") {
        details <- c(...)
        method <- details[[1]]
        lower <- details[[2]]
        upper <- details[[3]]
        seed <- details[[4]]
        formula <- paste0(self$response_var, " ~ ", self$right_site)
        formula <- as.formula(formula)
        DataModelState$formula <- env_optim_V1_2$create_formula_optim(formula, self$df, method, lower, upper, seed)
        eq <- paste0(self$response_var, " = ", self$right_site)
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "CreateFormula",
          formula = deparse(formula),
          method = method,
          lower = lower, upper = upper, seed = seed,
          "Model Type" = "Optimization Model"
        )
      } else if (model_type == "Linear Mixed Model") {
        formula <- paste(self$response_var, " ~ ", self$right_site)
        formula <- as.formula(formula)
        env_check_ast_V1_2$check_ast(formula, colnames(self$df))
        env_check_ast_V1_2$check_formula_rhs(formula[[3]])
        DataModelState$formula <- new("LinearMixedFormula", formula = formula)
        model <- lmerTest::lmer(formula, data = self$df)
        eq <- try(equatiomatic::extract_eq(model, wrap = TRUE), silent = TRUE)
        if (inherits(eq, "try-error")) {
          eq <- sprintf(
            "Failed to create the equation from the formula (due to %s)",
            attributes(eq)$condition$message)
        }
        ResultsState$history[[length(ResultsState$history) + 1]] <- list(
          type = "CreateFormula",
          formula = deparse(formula),
          "Model Type" = "Linear Mixed Model",
          details = ""
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
      }, silent = TRUE)
      if (inherits(e, "try-error")) {
        self$com$print_err(conditionMessage(attr(e, "condition")))
        return(NULL)
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

    description = paste(
      "Shapiro-Wilk test of normality of the response, separately for each group",
      "of the formula currently defined in the app.",
      "Usually the residuals (shapiro_on_residuals) are the better check."
    ),
    # Ollama: ellmer drops the properties field of tools without arguments
    params = list(
      alpha = list(
        type = "number",
        default = 0.05,
        description = "Not used; the column 'Normal distributed' always uses 0.05."
      )
    ),

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

    description = paste(
      "Shapiro-Wilk test of normality of the residuals of the formula currently",
      "defined in the app (linear or linear mixed model)."
    ),
    params = list(
      alpha = list(
        type = "number",
        default = 0.05,
        description = "Not used; the column 'Residuals normal distributed' always uses 0.05."
      )
    ),

    initialize = function(df, formula, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula
      self$com <- com$new()
    },

    validate = function() {
      stopifnot("Formula is not of type linear or linear mixed" = (inherits(self$formula, "LinearFormula") || inherits(self$formula, "LinearMixedFormula")))
    },

    eval = function(ResultsState) {
      withCallingHandlers(
        expr = {
          new_name <- paste0(ResultsState$counter + 1, " Shapiro on residuals")
          promise_history_entry <- self$create_history(new_name)
          ResultsState$bgp$start(
            fun = function(df, formula) {
              if (inherits(formula, "LinearFormula")) {
                fit <- lm(formula@formula, data = df)
                r <- resid(fit)
              } else if (inherits(formula, "LinearMixedFormula")) {
                fit <- lmerTest::lmer(formula@formula, data = df)
                r <- residuals(fit)
              }
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
# car::leveneTest compares groups, numeric predictors have none
levene_possible_V1_2 <- function(df, formula) {
  predictors <- all.vars(formula@formula[[3]])
  all(vapply(predictors, function(v) !is.numeric(df[[v]]), logical(1)))
}

levene_V1_2 <- R6::R6Class(
  "levene_V1_2",
  public = list(
    df = NULL,
    formula = NULL,
    center = NULL,
    com = NULL,

    description = paste(
      "Levene test for equal variances between the groups of the linear formula",
      "currently defined in the app."
    ),
    params = list(
      center = list(
        type = "enum",
        values = c("median", "mean"),
        default = "median",
        description = "median gives the robust Brown-Forsythe variant, mean the original Levene test."
      )
    ),

    initialize = function(df, formula, center, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula
      self$center <- center
      self$com <- com$new()
    },

    validate = function() {
      stopifnot(
        "Formula is not of type linear" = inherits(self$formula, "LinearFormula"),
        "center has to be median or mean" = isTRUE(self$center %in% c("median", "mean")),
        "Levene test requires that all predictors are categorical (factors)" =
          levene_possible_V1_2(self$df, self$formula)
      )
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

    description = paste(
      "Creates diagnostic plots of the residuals for the formula currently defined in the app:",
      "residuals vs fitted (linearity), QQ plot (normality), scale-location (equal variance),",
      "residuals vs leverage and Cook's distance (influential points).",
      "Returns the patterns of the plots as numbers.",
      "Use it after create_formula to check the model assumptions."
    ),
    params = list(
      max_points = list(
        type = "integer",
        default = 5L,
        description = "Maximum number of influential rows listed."
      )
    ),

    initialize = function(df, formula, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula
      self$com <- com$new()
    },

    validate = function() {
      stopifnot(
        "Diagnostic plots require a linear, generalised linear or linear mixed model" =
          inherits(self$formula, c("LinearFormula", "GeneralisedLinearFormula", "LinearMixedFormula"))
      )
    },

    eval = function(ResultsState) {
      withCallingHandlers(
        expr = {
          new_name <- paste0(ResultsState$counter + 1, " Diagnostic plot")
          promise_history_entry <- self$create_history(new_name)
          ResultsState$bgp$start(
            fun = function(df, formula) {
              plots <- OpenStats:::env_diagnostic_plots_V1_2$diagnostic_plots(df, formula)
              new("diagnosticPlots", p = plots, current_page = 1L)
            },
            args = list(df = self$df, formula = self$formula),
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
    unit_names = NULL,
    formula = NULL,
    com = NULL,
    res_df = NULL,
    res_p = NULL,
    ic_percentage = NULL,
    type = NULL,

    description = paste(
      "Fits dose response curves per substance and returns the IC values,",
      "using the formula currently defined in the app (response ~ concentration).",
      "Runs in the background and can take a while."
    ),
    params = list(
      ic_percentage = list(
        type = "number",
        default = 50,
        description = "Which inhibitory concentration is calculated, e.g. 50 for IC50."
      ),
      is_xlog = list(
        type = "boolean",
        default = FALSE,
        description = "Log transform the x axis (concentration)."
      ),
      is_ylog = list(
        type = "boolean",
        default = FALSE,
        description = "Log transform the y axis (response)."
      ),
      substance_names = list(
        type = "string",
        description = "Column containing the names of the substances."
      ),
      unit_names = list(
        type = "string",
        description = "Column containing the units of the concentration."
      ),
      type = list(
        type = "enum",
        values = c("continuous", "binomial", "Poisson"),
        default = "continuous",
        description = "Distribution of the response."
      )
    ),

    initialize = function(df, ic_percentage,
                          is_xlog, is_ylog,
                          substance_names, unit_names,
                          formula, type,
                          com = communicator_V1_2) {
      self$df <- df
      self$ic_percentage <- ic_percentage
      self$df[, substance_names] <- self$df[, substance_names] |> as.character()
      self$is_xlog <- is_xlog
      self$is_ylog <- is_ylog
      self$substance_names <- substance_names
      self$unit_names <- unit_names
      self$formula <- formula@formula
      self$type <- type
      self$com <- com$new()
    },

    validate = function() {},

    eval = function(ResultsState, new_name) {
      withCallingHandlers(
        expr = {
          promise_history_entry <- self$create_history(new_name)
          ResultsState$bgp$start(
            fun = function(df, ic_percentage, formula, substance_names, unit_names, is_xlog, is_ylog, type) {
              f <- as.character(formula)
              dep <- f[2]
              indep <- f[3]
              res_p <- NULL
              res_df <- NULL
              err <- try({
                OpenStats:::check_formula(formula)
                OpenStats:::env_check_ast_V1_2$check_ast(str2lang(indep), colnames(df))
                OpenStats:::env_check_ast_V1_2$check_ast(str2lang(dep), colnames(df))

                res <- OpenStats:::env_lc_V1_2$ic(
                  df, ic_percentage, dep, indep, substance_names, unit_names,
                  is_xlog, is_ylog, type
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
              new("doseResponse", input_df = df, df = res_df, p = res_p,
                name_col = substance_names, unit_col = unit_names, formula = formula,
                xTransform = is_xlog, yTransform = is_ylog,
                current_page = 1L)
            },
            args = list(
              df = self$df, ic_percentage = self$ic_percentage, formula = self$formula, substance_names = self$substance_names,
              unit_names = self$unit_names, is_xlog = self$is_xlog, is_ylog = self$is_ylog, type = self$type
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
        "IC [%]" = self$ic_percentage,
        "Column containing the names" = self$substance_names,
        "Column containing the units" = self$unit_names,
        "Log transform x-axis" = self$is_xlog,
        "Log transform y-axis" = self$is_ylog,
        "formula" = deparse(self$formula),
        "Response type" = self$type,
        "Result name" = new_name
      )
    }

  )
)
primary_assay_V1_2 <- R6::R6Class(
  "primary_assay_V1_2",
  public = list(
    df = NULL,
    formula = NULL,
    name_column = NULL,
    neg_control_name = NULL,
    pos_control_name = NULL,
    pval_adj_method = NULL,
    fold_or_percentage = NULL,
    com = NULL,

    description = paste(
      "Primary assay: compares every substance against the negative control",
      "using the formula currently defined in the app (response ~ substance column).",
      "Linear models are normalised to the controls first."
    ),
    params = list(
      neg_control_name = list(
        type = "string",
        description = "Level of the predictor that is the negative control."
      ),
      pos_control_name = list(
        type = "string",
        default = "",
        description = "Level of the predictor that is the positive control. Empty for none."
      ),
      pval_adj_method = list(
        type = "enum",
        values = c("holm", "hochberg", "hommel", "bonferroni", "sidak"),
        default = "holm",
        description = "p value adjustment for multiple testing."
      ),
      fold_or_percentage = list(
        type = "enum",
        values = c("Percentage", "Fold change"),
        default = "Percentage",
        description = "Normalisation for linear models."
      )
    ),

    initialize = function(df, formula, neg_control_name, pos_control_name, pval_adj_method, fold_or_percentage, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula
      indep <- try({
        f <- as.character(formula@formula)
        f[[3L]]
      }, silent = TRUE)
      if (inherits(indep, "try-error")) stop("The independent variable is not usable")
      self$name_column <- indep
      self$neg_control_name <- neg_control_name
      self$pos_control_name <- pos_control_name
      self$pval_adj_method <- pval_adj_method
      self$fold_or_percentage <- fold_or_percentage
      self$com <- com$new()
    },

    validate = function() {},

    eval = function(ResultsState) {
      withCallingHandlers(
        expr = {
          new_name <- paste0(ResultsState$counter + 1, " Primary Assay")
          promise_history_entry <- self$create_history(new_name)

          ResultsState$bgp$start(
            fun = function(df, formula, name_column, neg_control_name, pos_control_name, fold_or_percentage, pval_adj_method) {
              indep <- as.character(formula@formula)[3]
              dep <- as.character(formula@formula)[2]

              if (inherits(formula, "LinearFormula")) {
                if (pos_control_name != "") {
                  mean_pos <- mean(df[df[, indep] == pos_control_name, dep], na.rm = TRUE)
                  df[, dep] <- df[, dep] - mean_pos
                  df <- df[df[, indep] != pos_control_name, , drop = FALSE] # Remove pos control
                } else {
                  warning("No positive control is specified the respective normalization will not be conducted")
                }
              }
              unit <- "%"
              if (inherits(formula, "LinearFormula")) {
                if (neg_control_name == "") {
                  stop("You have to define a name for the negative control")
                }
                mean_neg <- mean(df[df[, indep] == neg_control_name, dep], na.rm = TRUE)
                if (fold_or_percentage == "Percentage") {
                  df[, dep] <- (df[, dep] / mean_neg) * 100
                } else if (fold_or_percentage == "Fold change") {
                  df[, dep] <- df[, dep] / mean_neg
                  unit <- ""
                } else {
                  stop(sprintf("Invalid mode '%s' for a linear model. Use 'Percentage' or 'Fold change'.", fold_or_percentage))
                }
              } else {
                unit <- ""
              }

              if (inherits(formula, "LinearFormula")) {
                fit <- lm(formula@formula, data = df)
              } else if (inherits(formula, "GeneralisedLinearFormula")) {
                f <- formula@family
                if (f != "binomial") {
                  warning(sprintf("Cannot handle family %s. Therefore the family binomial is used here!", f))
                  f <- "binomial"
                }
                fit <- glm(formula@formula, data = df, family = f)
              } else {
                stop(sprintf("Cannot use model of type %s", class(formula)))
              }

              emm <- emmeans::emmeans(fit, name_column)
              res <- emmeans::contrast(emm, method = "trt.vs.ctrl", ref = neg_control_name, adjust = pval_adj_method)
              res <- broom::tidy(res) |> as.data.frame()
              res <- res[, c("contrast", "estimate", "adj.p.value")]
              names(res) <- c("name", "Standard Value", "adj. p value")
              res[["Standard Units"]] <- unit
              res <- res[, c(1, 2, 4, 3)]
              pattern <- paste0("\\s*-\\s*\\Q", neg_control_name, "\\E$")
              res$name <- gsub(pattern, "", res$name, perl = TRUE)
              res
            },
            args = list(
              df = self$df, formula = self$formula,
              name_column = self$name_column,
              neg_control_name = self$neg_control_name, pos_control_name = self$pos_control_name,
              fold_or_percentage = self$fold_or_percentage,
              pval_adj_method = self$pval_adj_method
            ),
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
        type = "PrimaryAssay",
        formula = deparse(self$formula@formula),
        "Negative control name" = self$neg_control_name,
        "Positive control name" = self$pos_control_name,
        "P val adjustment method" = self$pval_adj_method,
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

    description = paste(
      "Conducts a t-test using the formula currently defined in the app.",
      "Do not pass a formula; check it with get_state and use create_formula if needed.",
      "Requires a formula with one grouping variable that has exactly two levels."
    ),
    params = list(
      variances_equal = list(
        type = "enum",
        values = c("eq", "noeq"),
        default = "eq",
        description = "Input which indicates whether the variances of the groups are equal. noeq results in a Welch's test."
      ),
      conf_level = list(
        type = "number",
        default = 0.95,
        description = "A numeric value between 0 and 1 which indicates the desired confidence value."
      ),
      alternative_hyp = list(
        type = "enum",
        values = c("two.sided", "less", "greater"),
        default = "two.sided",
        description = "Input value which indicates which alternative hypothesis should be used."
      )
    ),

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

    validate = function() {
      stopifnot(
        "conf_level has to be a number between 0 and 1" =
          is.numeric(self$conf_level) && length(self$conf_level) == 1L &&
          !is.na(self$conf_level) && self$conf_level > 0 && self$conf_level < 1,
        "variances_equal has to be eq or noeq" =
          isTRUE(self$variances_equal %in% c("eq", "noeq")),
        "alternative_hyp has to be two.sided, less or greater" =
          isTRUE(self$alternative_hyp %in% c("two.sided", "less", "greater"))
      )
    },

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

wilcox_rank_sum_V1_2 <- R6::R6Class(
  "wilcox_rank_sum_V1_2",
  public = list(
    df = NULL,
    formula = NULL,
    alternative_hyp = NULL,
    com = NULL,

    description = paste(
      "Wilcoxon rank sum test (Mann-Whitney U), non-parametric alternative to the t-test.",
      "Uses the formula currently defined in the app;",
      "requires one grouping variable with exactly two levels."
    ),
    params = list(
      alternative_hyp = list(
        type = "enum",
        values = c("two.sided", "less", "greater"),
        default = "two.sided",
        description = "Alternative hypothesis."
      )
    ),

    initialize = function(df, formula, alternative_hyp, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula@formula
      self$alternative_hyp <- alternative_hyp
      self$com = com$new()
    },

    validate = function() {
      stopifnot(
        "alternative_hyp has to be two.sided, less or greater" =
          isTRUE(self$alternative_hyp %in% c("two.sided", "less", "greater"))
      )
    },

    eval = function(ResultsState) {
      withCallingHandlers(
        {
          new_name <- paste0( ResultsState$counter + 1, " Wilcox-Rank-Sum-Test")
          promise_history_entry <- self$create_history(new_name)
          ResultsState$bgp$start(
            fun = function(formula, df, alternative_hyp) {
              broom::tidy(wilcox.test(formula, data = df, alternative = alternative_hyp))
            },
            args = list(
              formula = self$formula, df = self$df, alternative_hyp = self$alternative_hyp
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
        type = "WilcoxRankSumTest",
        formula = deparse(self$formula),
        "alternative hypothesis" = self$alternative_hyp,
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

    description = paste(
      "ANOVA type tests and post hoc tests for the formula currently defined in the app.",
      "Linear model: aov (ANOVA), welch_aov, kruskal (Kruskal-Wallis),",
      "post hoc HSD, LSD, scheffe, REGW, kruskalTest (these need response ~ one factor).",
      "Generalised linear model: aov (Chisq ANOVA) or a p value adjustment",
      "(tukey, sidak, bonferroni, scheffe, none, fdr, holm, hochberg, hommel) for pairwise comparisons.",
      "Linear mixed model: only aov."
    ),
    params = list(
      method = list(
        type = "enum",
        values = c(
          "aov", "welch_aov", "kruskal", "HSD", "LSD", "scheffe", "REGW", "kruskalTest",
          "tukey", "sidak", "bonferroni", "none", "fdr", "holm", "hochberg", "hommel"
        ),
        description = "Test to run; which ones are allowed depends on the model type."
      ),
      balanced_design = list(
        type = "enum",
        values = c("Balanced", "Unbalanced"),
        default = "Balanced",
        description = "Only used by HSD."
      ),
      p_val = list(
        type = "number",
        default = 0.05,
        description = "Significance level of the post hoc tests."
      ),
      p_val_adj_method = list(
        type = "enum",
        values = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
        default = "holm",
        description = "p value adjustment, only used by LSD and kruskalTest."
      )
    ),

    initialize = function(df, formula, balanced_design, p_val, p_val_adj_method, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula
      self$balanced_design <- balanced_design
      self$p_val <- p_val
      self$p_val_adj_method <- p_val_adj_method
      self$com <- com$new()
    },

    validate = function() {},

    # Separate from validate(): the method is only known in eval()
    validate_method = function(method) {
      allowed <- if (inherits(self$formula, "LinearFormula")) {
        c("aov", "welch_aov", "kruskal", "HSD", "LSD", "scheffe", "REGW", "kruskalTest")
      } else if (inherits(self$formula, "GeneralisedLinearFormula")) {
        c("aov", "tukey", "sidak", "bonferroni", "scheffe", "none", "fdr", "holm", "hochberg", "hommel")
      } else if (inherits(self$formula, "LinearMixedFormula")) {
        "aov"
      } else {
        character()
      }
      if (!isTRUE(method %in% allowed)) {
        stop("method has to be one of: ", paste(allowed, collapse = ", "), " for this model type")
      }
    },

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
                welch_aov = {
                  fit <- broom::tidy(oneway.test(
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
                  fit <- agricolae::HSD.test(aov_res,
                    trt = indep,
                    alpha = p_val, group = TRUE,
                    unbalanced = balanced_design != "Balanced"
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
            in_background = FALSE, ResultsState
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
                  model <- glm(formula@formula, data = df, family = OpenStats:::env_utils_V1_2$build_glm_family(family, link_fct))
                  fit <- broom::tidy(anova(model, test = "Chisq"))
                }
              )
              if (is.null(fit)) { # This covers all the emmeans post hoc tests
                family <- formula@family
                link_fct <- formula@link_fct

                f_split <- OpenStats:::env_utils_V1_2$split_formula(formula@formula)
                rhs_vars <- OpenStats:::env_utils_V1_2$vars_rhs(f_split$right_site)
                df_temp <- OpenStats:::env_utils_V1_2$num_to_factor(df, rhs_vars)
                if (any(apply(df, 2, is.numeric))) {
                  warning(paste0("Found numeric predictors and converted them to factors"))
                }
                model <- glm(formula@formula, data = df_temp, family = OpenStats:::env_utils_V1_2$build_glm_family(family, link_fct))
                emm <- emmeans::emmeans(model, rhs_vars)
                fit <- pairs(emm, adjust = method)
                fit <- as.data.frame(fit)
              }
              fit
            },
            args = list( formula = self$formula, df = self$df, method = method),
              promise_result_name = new_name,
              promise_history_entry = promise_history_entry,
              in_background = FALSE, ResultsState
          )
        })
    },

    eval_linear_mixed = function(method, new_name, ResultsState) {
      withCallingHandlers(
        expr = {
          promise_history_entry <- self$create_history(new_name, method)
          ResultsState$bgp$start(
            fun = function(formula, df, method) {
              fit <- NULL
              switch(method,
                aov = {
                  m <- lmerTest::lmer(formula@formula, data = df)
                  fit <- broom::tidy(anova(m)) |> as.data.frame()
                }
              )
              fit
            },
            args = list(
              formula = self$formula, df = self$df, method = method),
            promise_result_name = new_name,
            promise_history_entry = promise_history_entry,
            in_background = FALSE, ResultsState
          )
        }
      )
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
          new_name <- paste0(ResultsState$counter + 1, " Test ", method)
          if (inherits(self$formula, "LinearFormula")) {
            self$eval_lm(method, new_name, ResultsState)
          } else if (inherits(self$formula, "GeneralisedLinearFormula")) {
            self$eval_glm(method, new_name, ResultsState)
          } else if (inherits(self$formula, "LinearMixedFormula")) {
            self$eval_linear_mixed(method, new_name, ResultsState)
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
        welch_aov = {
          history_data <- list(
            type = "WELCH_ANOVA", formula = deparse(self$formula@formula), "Result name" = new_name
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
          shiny::req(bal)
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
    create_history_linear_mixed = function(new_name, method) {
      history_data <- NULL
      switch(method,
        aov = {
          history_data <- list(
            type = "ANOVA Linear Mixed",
            formula = deparse(self$formula@formula),
            "Result name" = new_name
          )
        }
      )
    },
    create_history = function(new_name, method) {
      if (inherits(self$formula, "LinearFormula")) {
        self$create_history_lm(new_name, method)
      } else if (inherits(self$formula, "GeneralisedLinearFormula")) {
        self$create_history_glm(new_name, method)
      } else if (inherits(self$formula, "LinearMixedFormula")) {
        self$create_history_linear_mixed(new_name, method)
      }
    }
  )
)

perm_ANOVA_V1_2 <- R6::R6Class(
  "perm_ANOVA_V1_2",
  public = list(
    df = NULL,
    formula = NULL,
    perm = NULL,
    seed = 1234,
    model_type = NULL,
    com = NULL,

    description = paste(
      "Permutation ANOVA for the formula currently defined in the app",
      "(linear or linear mixed model). Robust alternative to the classical ANOVA",
      "when residuals are not normally distributed.",
      "Runs in the background and can take a while.",
      "Do not pass a formula; check it with get_state and use create_formula if needed."
    ),
    params = list(
      perm = list(
        type = "integer",
        default = 5000L,
        description = "Number of permutations, typically 1000 to 20000. More permutations give more precise p-values but take longer."
      ),
      seed = list(
        type = "integer",
        default = 1234L,
        description = "Seed for the random number generator, makes the result reproducible."
      )
    ),

    initialize = function(df, formula, perm, seed, model_type, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula
      self$perm <- perm
      self$seed <- seed
      self$model_type <- model_type
      self$com = com$new()
    },

    validate = function() {
      stopifnot(
        "perm has to be a positive whole number" =
          is.numeric(self$perm) && length(self$perm) == 1L &&
          !is.na(self$perm) && self$perm >= 1 && self$perm == round(self$perm),
        "seed has to be a whole number" =
          is.numeric(self$seed) && length(self$seed) == 1L &&
          !is.na(self$seed) && self$seed == round(self$seed),
        "Permutation ANOVA requires a linear or linear mixed model" =
          isTRUE(self$model_type[1] %in% c("LinearFormula", "LinearMixedFormula"))
      )
    },

    eval = function(ResultsState) {
      withCallingHandlers(
        {
          new_name <- paste0( ResultsState$counter + 1, " Permutation ANOVA")
          promise_history_entry <- self$create_history(new_name)
          ResultsState$bgp$start(
            fun = function(formula, df, perm, seed, model_type) {
              set.seed(seed)
              environment(formula) <- globalenv()
              if (model_type == "LinearFormula") {
                P <- permuco::Pmat(np = perm, n = nrow(df))
                fit <- permuco::aovperm(formula, data = df, P = P)
                as.data.frame(summary(fit))
              } else if (model_type == "LinearMixedFormula") {
                fit <- permutes::perm.lmer(formula, data = df, np = perm, type = 'anova')
                fit
              }
            },
            args = list(
              formula = self$formula@formula, df = self$df, perm = self$perm, seed = self$seed, model_type = self$model_type
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
        type = "Permutation ANOVA",
        formula = deparse(self$formula@formula),
        "number of permutations" = self$perm,
        "seed" = self$seed,
        "Model type" = self$model_type,
        "Result name" = new_name
      )
    }
  )
)

pairwise_comparisons_V1_2 <- R6::R6Class(
  "pairwise_comparisons_V1_2",
  public = list(
    df = NULL,
    formula = NULL,
    p_val = NULL,
    alternative_hyp = NULL,
    p_val_adj_method = NULL,
    parametric = NULL,
    method = NULL,
    com = NULL,

    description = paste(
      "Pairwise t-tests or Wilcoxon tests between all groups (combinations of the",
      "predictors) of the formula currently defined in the app.",
      "Only comparisons with p <= p_val are returned."
    ),
    params = list(
      p_val = list(
        type = "number",
        default = 0.05,
        description = "Only comparisons with an adjusted p value below this are listed."
      ),
      alternative_hyp = list(
        type = "enum",
        values = c("two.sided", "less", "greater"),
        default = "two.sided",
        description = "Alternative hypothesis."
      ),
      p_val_adj_method = list(
        type = "enum",
        values = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
        default = "holm",
        description = "p value adjustment for multiple testing."
      ),
      parametric_or_non_parametric = list(
        type = "enum",
        values = c("parametric", "non_parametric"),
        default = "parametric",
        description = "parametric: t-tests, non_parametric: Wilcoxon tests."
      )
    ),

    initialize = function(df, formula,
                          p_val, alternative_hyp,
                          p_val_adj_method, parametric_or_non_parametric, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula@formula
      self$p_val <- p_val
      self$alternative_hyp <- alternative_hyp
      self$p_val_adj_method <- p_val_adj_method
      self$parametric <- TRUE
      self$method <- "T-Test"
      if (parametric_or_non_parametric == "non_parametric") {
        self$parametric <- FALSE
        self$method <- "Wilcox"
      }
      self$com <- com$new()
    },

    validate = function() {
      stopifnot(
        "p_val has to be a number between 0 and 1" =
          is.numeric(self$p_val) && length(self$p_val) == 1L &&
          !is.na(self$p_val) && self$p_val >= 0 && self$p_val <= 1,
        "alternative_hyp has to be two.sided, less or greater" =
          isTRUE(self$alternative_hyp %in% c("two.sided", "less", "greater")),
        "p_val_adj_method is unknown" = isTRUE(self$p_val_adj_method %in% stats::p.adjust.methods)
      )
    },

    eval = function(ResultsState) {
      withCallingHandlers(
        {
          if (self$parametric) {
            new_name <- paste0( ResultsState$counter + 1, " Pairwise T-Test")
          } else {
            new_name <- paste0( ResultsState$counter + 1, " Pairwise Wilcox-Test")
          }
          promise_history_entry <- self$create_history(new_name)
          ResultsState$bgp$start(
            fun = function(formula, df, p_val, alternative_hyp, p_val_adj_method, parametric) {
              dep <- as.character(formula)[2]
              predictors <- all.vars(formula[[3]])
              if (parametric) {
                res <- pairwise.t.test(df[[dep]], interaction(df[predictors]), alternative = alternative_hyp, p.adjust.method = p_val_adj_method)
              } else {
                res <- pairwise.wilcox.test(df[[dep]], interaction(df[predictors]), alternative = alternative_hyp, p.adjust.method = p_val_adj_method)
              }
              res <- broom::tidy(res)
              res <- res[res$p.value <= p_val, ]
              return(res)
            },
            args = list(
              formula = self$formula, df = self$df,
              p_val = self$p_val, alternative_hyp = self$alternative_hyp,
              p_val_adj_method = self$p_val_adj_method, parametric = self$parametric
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
        type = "PairwiseComparison",
        formula = deparse(self$formula),
        "P-value" = self$p_val,
        "alternative hypothesis" = self$alternative_hyp,
        "Adjusted p value method" = self$p_val_adj_method,
        "Method" = self$method,
        "Result name" = new_name
      )
    }
  )
)

pairwise_comparisons_linear_mixed_V1_2 <- R6::R6Class(
  "pairwise_comparisons_linear_mixed_V1_2",
  public = list(
    df = NULL,
    formula = NULL,
    method = NULL,
    p_val = NULL,
    com = NULL,

    description = paste(
      "Post hoc comparisons for the linear mixed model currently defined in the app",
      "(emmeans pairs for factors, emtrends for numeric predictors).",
      "With factors and numeric predictors both the group levels and, if the",
      "formula contains an interaction, the slopes are compared at several values",
      "of the numeric predictor."
    ),
    params = list(
      method = list(
        type = "enum",
        values = c("tukey", "sidak", "bonferroni", "scheffe", "none", "fdr", "holm", "hochberg", "hommel"),
        default = "tukey",
        description = "p value adjustment for multiple testing."
      ),
      p_val = list(
        type = "number",
        default = 0.05,
        description = "Only comparisons with an adjusted p value below this are listed."
      )
    ),

    initialize = function(df, formula, method, p_val, com = communicator_V1_2) {
      self$df <- df
      self$formula <- formula
      self$method <- method
      self$p_val<- p_val
      self$com <- com$new()
    },

    validate = function() {
      stopifnot(
        "Pairwise comparisons for linear mixed models require a linear mixed model" =
          inherits(self$formula, "LinearMixedFormula")
      )
    },

    eval = function(ResultsState) {
      withCallingHandlers(
        {
          new_name <- paste0( ResultsState$counter + 1, " Pairwise Comparisons Linear Mixed Model")
          promise_history_entry <- self$create_history(new_name)
          ResultsState$bgp$start(
            fun = function(formula, df, method, p_val) {
              model <- lmerTest::lmer(formula@formula, data = df)
              fixed_formula <- reformulas::nobars(formula@formula)
              f_split <- OpenStats:::env_utils_V1_2$split_formula(fixed_formula)
              rhs_vars <- OpenStats:::env_utils_V1_2$vars_rhs(f_split$right_site)
              factor_vars <- rhs_vars[vapply(df[rhs_vars], is.factor, logical(1))]
              numeric_vars <- rhs_vars[vapply(df[rhs_vars], is.numeric, logical(1))]
              # Predictors are all of type factor
              if (length(factor_vars) == length(rhs_vars)) {
                emm <- emmeans::emmeans(
                  model,
                  specs = stats::as.formula(
                    paste("~", paste(factor_vars, collapse = " * "))
                  ),
                  data = df
                )
                fit <- pairs(emm, adjust = method)
                fit <- as.data.frame(fit)
                res <- fit[fit$p.value <= p_val, ]
              }
              # Predictors are all of type numeric
              else if (length(numeric_vars) == length(rhs_vars)) {
                out <- list()
                for (num_var in numeric_vars) {
                  trend <- emmeans::emtrends(model, specs = ~ 1, var = num_var, data = df)
                  trend <- as.data.frame(trend)
                  trend$trend_variable <- num_var
                  out[[num_var]] <- trend
                }
                res <- do.call(rbind, out)
                rownames(res) <- NULL
              }
              # Mixed
              else {
                # Same columns for every block, otherwise rbind fails
                as_rows <- function(fit, comparison, at_variable) {
                  fit <- as.data.frame(fit)
                  at_value <- NA_real_
                  if (!is.na(at_variable)) {
                    at_value <- fit[[at_variable]]
                    fit[[at_variable]] <- NULL
                  }
                  cbind(
                    data.frame(comparison = comparison, at_variable = at_variable, at_value = at_value),
                    fit
                  )
                }
                levels_spec <- paste("~", paste(factor_vars, collapse = " * "))
                out <- list()
                for (num_var in numeric_vars) {
                  by_factors <- OpenStats:::env_utils_V1_2$interacting_factors(
                    fixed_formula, num_var, factor_vars
                  )
                  # Without interaction the slopes are equal by model definition:
                  # contrasts with zero variance break the Kenward-Roger df
                  if (length(by_factors) == 0L) next
                  at <- stats::setNames(list(OpenStats:::env_utils_V1_2$at_values(df[, num_var])), num_var)
                  emm <- emmeans::emmeans(
                    model,
                    specs = stats::as.formula(paste(levels_spec, "|", num_var)),
                    data = df,
                    at = at
                  )
                  trend <- emmeans::emtrends(
                    model,
                    specs = stats::as.formula(
                      paste("~", paste(by_factors, collapse = " * "), "|", num_var)
                    ),
                    var = num_var,
                    data = df,
                    at = at
                  )
                  # The adjustment runs within each value of num_var (emmeans default for
                  # by variables), not across all values and both blocks; by = NULL would
                  # adjust over everything but is very strict with many values
                  out[[paste("level", num_var)]] <- as_rows(pairs(emm, adjust = method), "level", num_var)
                  out[[paste("slope", num_var)]] <- as_rows(
                    pairs(trend, adjust = method), paste("slope of", num_var), num_var
                  )
                }
                # No interaction: the group differences are equal for every value
                # of the numeric predictors, so one comparison at their mean suffices
                if (length(out) == 0L) {
                  emm <- emmeans::emmeans(model, specs = stats::as.formula(levels_spec), data = df)
                  out$level <- as_rows(pairs(emm, adjust = method), "level", NA_character_)
                }
                res <- do.call(rbind, out)
                rownames(res) <- NULL
                res <- res[res$p.value <= p_val, ]
              }
              return(res)
            },
            args = list(
              formula = self$formula, df = self$df, method = self$method, p_val = self$p_val
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
        type = "PairwiseComparisonLinearMixedModel",
        formula = deparse(self$formula@formula),
        "P-value" = self$p_val,
        "Adjusted p value method" = self$method,
        "Result name" = new_name
      )
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

    description = paste(
      "Makes another dataset from the results the active dataset; all later",
      "analyses use it. E.g. create a subset with create_intermediate_var",
      "(get_rows(df, Treatment == 'chilled')) and activate it here with its result name.",
      "get_results lists the datasets."
    ),
    # Not called `name`: it would shadow the tool name inside llm_make_tool_V1_2
    params = list(
      table_name = list(
        type = "string",
        description = "Name of a dataset exactly as listed by get_results."
      )
    ),

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
            promise_history_entry = NULL, #"History"
            in_background = TRUE
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
