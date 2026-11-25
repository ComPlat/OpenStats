# Build the LLM tools
# ----------------------------------------------------------
# params and description of the tools come from these classes
llm_tool_classes_V1_2 <- list(
  get_state = get_state_V1_2,
  get_results = get_results_V1_2,
  create_formula = create_formula_V1_2,
  create_summary = create_summary_V1_2,
  create_summary_plot = create_summary_plot_V1_2,
  create_intermediate_var = create_intermediate_var_V1_2,
  create_new_col = create_new_col_V1_2,
  diagnostic_plots = diagnostic_plots_V1_2,
  shapiro_on_data = shapiro_on_data_V1_2,
  shapiro_on_residuals = shapiro_on_residuals_V1_2,
  levene = levene_V1_2,
  t_test = t_test_V1_2,
  wilcox_rank_sum = wilcox_rank_sum_V1_2,
  correlation = correlation_V1_2,
  statistical_tests = statistical_tests_V1_2,
  perm_ANOVA = perm_ANOVA_V1_2,
  pairwise_comparisons = pairwise_comparisons_V1_2,
  pairwise_comparisons_linear_mixed = pairwise_comparisons_linear_mixed_V1_2,
  dose_response = dose_response_V1_2,
  primary_assay = primary_assay_V1_2,
  set_active_table = set_active_table_V1_2,
  choose_toolbox = choose_toolbox_V1_2
)

# Toolboxes
# ----------------------------------------------------------
# Small models have a small context window: besides the tools that are
# always offered, only the tools of the chosen toolbox are offered,
# filtered by the formula and the number of groups in the dataset
llm_always_offered_tools_V1_2 <- c("get_state", "get_results", "create_formula", "choose_toolbox")

llm_toolbox_tools_V1_2 <- function(toolbox, DataModelState) {
  formula <- DataModelState$formula
  is_lm <- inherits(formula, "LinearFormula")
  is_glm <- inherits(formula, "GeneralisedLinearFormula")
  is_lmm <- inherits(formula, "LinearMixedFormula")
  switch(
    toolbox,
    data = c("create_summary", "create_summary_plot", "create_intermediate_var", "create_new_col", "set_active_table"),
    assumptions = {
      if (is_lm) {
        c(
          "diagnostic_plots", "shapiro_on_data", "shapiro_on_residuals",
          if (levene_possible_V1_2(DataModelState$df, formula)) "levene"
        )
      } else if (is_glm) {
        "diagnostic_plots"
      } else if (is_lmm) {
        c("diagnostic_plots", "shapiro_on_residuals")
      }
    },
    tests = {
      if (is_lm && calc_n_covariates(DataModelState) == 2L) {
        c("t_test", "wilcox_rank_sum")
      } else if (is_lm || is_lmm) {
        c("statistical_tests", "perm_ANOVA")
      } else if (is_glm) {
        "statistical_tests"
      }
    },
    posthoc = {
      if (is_lm) {
        c("statistical_tests", "pairwise_comparisons")
      } else if (is_glm) {
        "statistical_tests"
      } else if (is_lmm) {
        "pairwise_comparisons_linear_mixed"
      }
    },
    correlation = if (is_lm) "correlation",
    dose_response = if (is_lm || is_glm) c("dose_response", "primary_assay"),
    character()
  )
}

# statistical_tests mixes global and post hoc tests;
# each toolbox offers only its own methods
llm_statistical_tests_methods_V1_2 <- function(toolbox, formula) {
  if (toolbox == "tests") {
    if (inherits(formula, "LinearFormula")) c("aov", "welch_aov", "kruskal") else "aov"
  } else if (inherits(formula, "LinearFormula")) {
    c("HSD", "LSD", "scheffe", "REGW", "kruskalTest")
  } else {
    c("tukey", "sidak", "bonferroni", "scheffe", "none", "fdr", "holm", "hochberg", "hommel")
  }
}

llm_tool_params_V1_2 <- function(name, toolbox, formula) {
  params <- llm_tool_classes_V1_2[[name]]$public_fields$params
  if (name == "statistical_tests") {
    params$method$values <- llm_statistical_tests_methods_V1_2(toolbox, formula)
  }
  params
}


# These tools create no result
llm_read_tools_V1_2 <- c("get_state", "get_results")

# These tools change the state but create no result, so the
# counter does not change and they return their own text
llm_action_tools_V1_2 <- c("set_active_table")

llm_no_formula_tools_V1_2 <- c(
  "create_formula", "create_summary", "create_summary_plot",
  "create_intermediate_var", "create_new_col", llm_read_tools_V1_2,
  llm_action_tools_V1_2
)

llm_make_tool_V1_2 <- function(name, cls, params, run) {
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
      array = ellmer::type_array(
        if (is.null(p$values)) ellmer::type_string() else ellmer::type_enum(values = p$values),
        description = description, required = required
      ),
      stop("Unknown param type: ", p$type)
    )
  }
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

# on_done is called after every tool call: e.g. create_formula or
# choose_toolbox change which tools are offered, already within the same turn
llm_tools_V1_2 <- function(DataModelState, ResultsState, DataWranglingState,
                           ChatState, on_done = function() NULL) {
  run <- function(name, args) {
    if (name == "choose_toolbox") {
      res <- choose_toolbox_V1_2$new(args[["toolbox"]])
      res$validate()
      text <- shiny::isolate(res$eval(ChatState, DataModelState, DataWranglingState))
      on_done()
      return(text)
    }
    res <- llm_run_tool_V1_2(name, args, DataModelState, ResultsState, DataWranglingState)
    if (promises::is.promise(res)) {
      return(promises::then(res, function(x) {
        on_done()
        x
      }))
    }
    on_done()
    res
  }
  shiny::isolate({
    toolbox <- ChatState$toolbox
    offered <- c(llm_always_offered_tools_V1_2, llm_toolbox_tools_V1_2(toolbox, DataModelState))
    formula <- DataModelState$formula
  })
  Map(
    function(n) {
      llm_make_tool_V1_2(n, llm_tool_classes_V1_2[[n]], llm_tool_params_V1_2(n, toolbox, formula), run)
    },
    offered
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
      if (!background && !is.null(ResultsState$bgp$last_error)) {
        stop(ResultsState$bgp$last_error)
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

llm_run_tool_V1_2 <- function(name, args, DataModelState, ResultsState, DataWranglingState) {
  warnings <- character()
  args <- llm_fill_defaults_V1_2(args, llm_tool_classes_V1_2[[name]]$public_fields$params)
  # Tools without a result have to work while an analysis is running
  if (name %in% llm_read_tools_V1_2) {
    return(shiny::isolate(eval_tool_V1_2(name, args, DataModelState, ResultsState, DataWranglingState)))
  }
  if (name %in% llm_action_tools_V1_2) {
    return(shiny::isolate({
      if (isTRUE(ResultsState$bgp$is_running)) {
        stop("Another analysis is still running. Wait until it has finished.")
      }
      eval_tool_V1_2(name, args, DataModelState, ResultsState, DataWranglingState)
    }))
  }
  # Tool calls run in promise callbacks, outside of any reactive context
  started <- shiny::isolate({
    if (isTRUE(ResultsState$bgp$is_running)) {
      stop("Another analysis is still running. Wait until it has finished.")
    }
    counter_before <- ResultsState$counter
    ResultsState$bgp$last_error <- NULL
    withCallingHandlers(
      eval_tool_V1_2(name, args, DataModelState, ResultsState, DataWranglingState),
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
