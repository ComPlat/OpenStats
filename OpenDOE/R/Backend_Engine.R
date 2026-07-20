communicator <- R6::R6Class("communicator",
  public = list(
    print_warn = NULL,
    print_err = NULL,
    initialize = function() {
      self$print_warn <- print_warn
      self$print_err <- print_err
    }
  )
)

backend_communicator <- R6::R6Class("backend_communicator",
  public = list(
    print_warn = function(msg) warning(msg, call. = FALSE),
    print_err = function(msg) stop(msg, call. = FALSE),
    initialize = function() {}
  )
)

bg_process <- R6::R6Class("bg_process",
  public = list(
    process = NULL,
    running = FALSE,
    error = NULL,
    on_success = NULL,
    on_finally = NULL,
    com = NULL,

    initialize = function(com = communicator) {
      self$com <- com$new()
    },

    start = function(fun, args, on_success, on_finally = NULL, in_background = TRUE) {
      background <- getOption("OpenDOE.background", TRUE)

      if (!in_background || !background) {
        self$error <- NULL
        res <- tryCatch(do.call(fun, args), error = function(e) e)
        if (inherits(res, "error")) {
          self$error <- conditionMessage(res)
          self$com$print_err(self$error)
        } else {
          on_success(res)
        }
        if (!is.null(on_finally)) on_finally()
        return(invisible(NULL))
      }

      shiny::req(is.null(self$process) || !self$process$is_alive())

      # fun is defined inside a moduleServer closure that chains up to State;
      # strip it (and any formula args, which capture the same environment)
      # to baseenv() so callr does not serialize State into the child process.
      environment(fun) <- baseenv()
      args <- lapply(args, function(x) {
        if (inherits(x, "formula")) environment(x) <- baseenv()
        x
      })

      self$error <- NULL
      self$on_success <- on_success
      self$on_finally <- on_finally
      self$running <- TRUE
      self$process <- callr::r_bg(fun, args = args)
      invisible(NULL)
    },

    cancel = function() {
      if (is.null(self$process) || !self$process$is_alive()) return(invisible(NULL))

      self$process$kill_tree()
      on_finally <- self$on_finally
      self$process <- NULL
      self$running <- FALSE
      self$on_success <- NULL
      self$on_finally <- NULL
      self$com$print_warn("Process cancelled")
      if (!is.null(on_finally)) on_finally()
    },

    tick = function() {
      if (is.null(self$process) || self$process$is_alive()) return(invisible(NULL))

      on_success <- self$on_success
      on_finally <- self$on_finally

      tryCatch({
        res <- tryCatch(self$process$get_result(), error = function(e) e)
        err_output <- self$process$read_error()
        if (nzchar(err_output)) self$com$print_warn(err_output)

        if (inherits(res, "condition")) {
          self$error <- conditionMessage(res)
          self$com$print_err(self$error)
        } else {
          on_success(res)
        }
      }, finally = {
        self$process <- NULL
        self$running <- FALSE
        self$on_success <- NULL
        self$on_finally <- NULL
        if (!is.null(on_finally)) on_finally()
      })
    },

    init = function() {
      shiny::observe({
        shiny::invalidateLater(250)
        self$tick()
      })
    }
  )
)
