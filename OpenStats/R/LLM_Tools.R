# Build the LLM tools
# ----------------------------------------------------------
# params and description of the tools come from these classes
llm_tool_classes_V1_2 <- list(
  create_formula = create_formula_V1_2,
  t_test = t_test_V1_2,
  perm_ANOVA = perm_ANOVA_V1_2,
  diagnostic_plots = diagnostic_plots_V1_2,
  get_state = get_state_V1_2,
  get_results = get_results_V1_2
)

# These tools create no result
llm_read_tools_V1_2 <- c("get_state", "get_results")

# run(name, args) executes the tool: in the app directly, in the MCP server
# process it forwards the call to the app
llm_make_tool_V1_2 <- function(name, cls, run) {
  param_types <- function(p) {
    required <- is.null(p$default)
    description <- p$description
    if (!required) {
      description <- paste0(description, " Default: ", format(p$default), ".")
    }
    switch(p$type,
      enum = ellmer::type_enum(values = p$values, description = description, required = required),
      number = ellmer::type_number(description = description, required = required),
      integer = ellmer::type_integer(description = description, required = required),
      boolean = ellmer::type_boolean(description = description, required = required),
      string = ellmer::type_string(description = description, required = required),
      stop("Unknown param type: ", p$type)
    )
  }
  params <- cls$public_fields$params
  fun <- function() {
    run(name, as.list(environment()))
  }
  # NULL instead of empty defaults: optional args the LLM omits
  # would otherwise be missing arguments inside as.list(environment())
  formals(fun) <- stats::setNames(
    rep(list(NULL), length(params)), names(params)
  )
  ellmer::tool(
    fun, cls$public_fields$description,
    arguments = lapply(params, param_types), name = name
  )
}

llm_tools_V1_2 <- function(DataModelState, ResultsState) {
  run <- function(name, args) {
    llm_run_tool_V1_2(name, args, DataModelState, ResultsState)
  }
  Map(
    function(n, cls) llm_make_tool_V1_2(n, cls, run),
    names(llm_tool_classes_V1_2),
    llm_tool_classes_V1_2
  )
}

# Run a tool
# ----------------------------------------------------------
# The polling observer of bg_process stores the result;
# this only waits until it is done
llm_wait_for_bgp_V1_2 <- function(ResultsState, interval = 0.25) {
  promises::promise(function(resolve, reject) {
    check <- function() {
      if (isTRUE(shiny::isolate(ResultsState$bgp$is_running))) {
        later::later(check, interval)
      } else {
        resolve(NULL)
      }
    }
    check()
  })
}

llm_tool_result_text_V1_2 <- function(name, args, DataModelState, ResultsState,
                                      counter_before, background, warnings) {
  text <- shiny::isolate({
    # start_direct and tick report errors via notification instead of stopping
    if (ResultsState$counter == counter_before) {
      status <- ResultsState$bgp$running_status
      if (background && is.character(status) && startsWith(status, "Error")) {
        stop(status)
      }
      if (background && is.character(status) && startsWith(status, "Cancel")) {
        stop("The analysis was cancelled by the user.")
      }
      stop("The analysis failed. The error is shown as notification in the app.")
    }
    result_name <- utils::tail(names(ResultsState$all_data), 1L)
    res <- ResultsState$all_data[[result_name]]
    body <- tool_text_V1_2(name, res, DataModelState, args)
    paste0("Result name: ", result_name, "\n", body)
  })
  if (length(warnings) > 0L) {
    text <- paste0(text, "\nWarnings:\n", paste(warnings, collapse = "\n"))
  }
  text
}
# ellmer passes omitted arguments as NA or NULL
llm_fill_defaults_V1_2 <- function(args, params) {
  for (n in names(params)) {
    x <- args[[n]]
    if (length(x) == 0L || (length(x) == 1L && is.na(x))) {
      args[n] <- list(params[[n]]$default)
    }
  }
  args
}

llm_run_tool_V1_2 <- function(name, args, DataModelState, ResultsState) {
  warnings <- character()
  args <- llm_fill_defaults_V1_2(args, llm_tool_classes_V1_2[[name]]$public_fields$params)
  # Tools without a result have to work while an analysis is running
  if (name %in% llm_read_tools_V1_2) {
    return(shiny::isolate(eval_tool_V1_2(name, args, DataModelState, ResultsState)))
  }
  # Tool calls run in promise callbacks, outside of any reactive context
  started <- shiny::isolate({
    if (isTRUE(ResultsState$bgp$is_running)) {
      stop("Another analysis is still running. Wait until it has finished.")
    }
    counter_before <- ResultsState$counter
    withCallingHandlers(
      eval_tool_V1_2(name, args, DataModelState, ResultsState),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    list(
      counter_before = counter_before,
      background = isTRUE(ResultsState$bgp$is_running)
    )
  })
  finish <- function(...) {
    llm_tool_result_text_V1_2(
      name, args, DataModelState, ResultsState,
      started$counter_before, started$background, warnings
    )
  }
  if (started$background) {
    return(promises::then(llm_wait_for_bgp_V1_2(ResultsState), finish))
  }
  finish()
}
