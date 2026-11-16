llm_param_type_V1_2 <- function(p) {
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

llm_format_V1_2 <- function(x) {
  format(signif(x, 4))
}

llm_result_to_text_V1_2 <- function(res) {
  if (is.data.frame(res)) {
    return(paste(utils::capture.output(print(res)), collapse = "\n"))
  }
  if (isS4(res)) {
    tables <- Filter(
      function(s) is.data.frame(methods::slot(res, s)),
      methods::slotNames(res)
    )
    if (length(tables) > 0L) {
      parts <- vapply(tables, function(s) {
        paste0(s, ":\n", llm_result_to_text_V1_2(methods::slot(res, s)))
      }, character(1))
      return(paste(parts, collapse = "\n\n"))
    }
  }
  paste("Result of class", paste(class(res), collapse = ", "), "created.")
}

llm_run_tool_V1_2 <- function(cls, args, DataModelState, ResultsState) {
  warnings <- character()
  args <- llm_fill_defaults_V1_2(args, cls$public_fields$params)
  # Tool calls run in promise callbacks, outside of any reactive context
  shiny::isolate({
    counter_before <- ResultsState$counter
    withCallingHandlers(
      cls$run_tool(args, DataModelState, ResultsState),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    # start_direct reports errors via notification instead of stopping
    if (ResultsState$counter == counter_before) {
      stop("The analysis failed. The error is shown as notification in the app.")
    }
    name <- utils::tail(names(ResultsState$all_data), 1L)
    res <- ResultsState$all_data[[name]]
    body <- if (is.function(cls$tool_text)) {
      cls$tool_text(res, DataModelState)
    } else {
      llm_result_to_text_V1_2(res)
    }
    text <- paste0("Result name: ", name, "\n", body)
  })
  if (length(warnings) > 0L) {
    text <- paste0(text, "\nWarnings:\n", paste(warnings, collapse = "\n"))
  }
  text
}

llm_column_info_V1_2 <- function(name, x, max_levels = 20L) {
  if (is.numeric(x)) {
    return(sprintf("%s: numeric, range %s to %s", name,
      format(min(x, na.rm = TRUE)), format(max(x, na.rm = TRUE))))
  }
  lvls <- if (is.factor(x)) levels(x) else sort(unique(as.character(x)))
  shown <- utils::head(lvls, max_levels)
  more <- if (length(lvls) > max_levels) sprintf(", ... (%d levels)", length(lvls)) else ""
  sprintf("%s: %s, levels: %s%s", name, class(x)[1],
    paste(shown, collapse = ", "), more)
}

llm_get_state_V1_2 <- function(DataModelState, max_levels = 20L) {
  if (length(max_levels) != 1L || is.na(max_levels) || max_levels < 1) {
    max_levels <- 20L
  }
  shiny::isolate({
    df <- DataModelState$df
    if (is.null(df)) {
      return("No dataset loaded.")
    }
    formula <- if (is.null(DataModelState$formula)) {
      "none (define one with create_formula)"
    } else {
      paste(deparse(DataModelState$formula@formula), collapse = " ")
    }
    paste(
      c(
        paste("Active dataset:", DataModelState$active_df_name),
        sprintf("Rows: %d", nrow(df)),
        paste("Formula:", formula),
        "Columns:",
        mapply(llm_column_info_V1_2, names(df), df,
          MoreArgs = list(max_levels = max_levels))
      ),
      collapse = "\n"
    )
  })
}

llm_make_tool_V1_2 <- function(name, cls, DataModelState, ResultsState) {
  params <- cls$public_fields$params
  fun <- function() {
    llm_run_tool_V1_2(cls, as.list(environment()), DataModelState, ResultsState)
  }
  # NULL instead of empty defaults: optional args the LLM omits
  # would otherwise be missing arguments inside as.list(environment())
  formals(fun) <- stats::setNames(
    rep(list(NULL), length(params)),
    names(params)
  )
  ellmer::tool(
    fun,
    cls$public_fields$description,
    arguments = lapply(params, llm_param_type_V1_2),
    name = name
  )
}

llm_tools_V1_2 <- function(DataModelState, ResultsState) {
  ns <- environment(llm_tools_V1_2)
  classes <- mget(ls(ns, pattern = "_V1_2$"), envir = ns)
  has_tool <- vapply(classes, function(x) {
    inherits(x, "R6ClassGenerator") && is.function(x$run_tool)
  }, logical(1))
  engine_tools <- Map(
    function(n, cls) {
      llm_make_tool_V1_2(sub("_V1_2$", "", n), cls, DataModelState, ResultsState)
    },
    names(classes)[has_tool],
    classes[has_tool]
  )
  # ellmer drops the properties field of tools without arguments
  # for Ollama, which some model templates reject
  get_state <- ellmer::tool(
    function(max_levels = NULL) llm_get_state_V1_2(DataModelState, max_levels),
    paste(
      "Show the active dataset, its columns with types and levels,",
      "and the formula currently defined in the app."
    ),
    arguments = list(
      max_levels = ellmer::type_integer(
        "Maximum number of levels shown per column. Default: 20.",
        required = FALSE
      )
    ),
    name = "get_state"
  )
  c(engine_tools, list(get_state = get_state))
}
