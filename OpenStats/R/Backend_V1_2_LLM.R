llm_round_df_V1_2 <- function(df) {
  df <- as.data.frame(df)
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], signif, digits = 4)
  df
}

# Effects the LLM can read directly: estimated means and slopes per group
# (emmeans/emtrends), instead of coefficients that depend on the coding
create_formula_effects_text_V1_2 <- function(model, f, df) {
  predictors <- all.vars(f[[3]])
  is_num <- vapply(predictors, function(v) is.numeric(df[[v]]), logical(1))
  factors <- predictors[!is_num]
  numerics <- predictors[is_num]
  term_vars <- lapply(attr(stats::terms(f), "term.labels"), function(t) all.vars(str2lang(t)))
  partners <- function(v) {
    setdiff(unique(unlist(Filter(function(t) v %in% t && length(t) > 1L, term_vars))), v)
  }
  table <- function(x) llm_table_text_V1_2(llm_round_df_V1_2(summary(x)))
  at_mean <- if (length(numerics) > 0L) {
    paste0(" (numeric predictors at their mean: ",
      paste0(numerics, " = ", llm_format_V1_2(colMeans(df[numerics], na.rm = TRUE)), collapse = ", "), ")")
  } else {
    ""
  }
  parts <- character()
  for (v in factors) {
    by <- intersect(partners(v), factors)
    spec <- stats::as.formula(paste("~", v, if (length(by) > 0L) paste("|", paste(by, collapse = "*"))))
    # data: emmeans cannot rebuild it from the call lm(f, data = df) inside this function
    em <- suppressMessages(emmeans::emmeans(model, spec, data = df))
    parts <- c(parts, paste0(
      "Estimated mean per level of ", v, if (length(by) > 0L) paste0(" for each ", paste(by, collapse = "/")), at_mean, ":\n",
      table(em), "\n",
      "Differences between the levels of ", v, " (Tukey adjusted):\n",
      table(graphics::pairs(em))
    ))
  }
  for (v in numerics) {
    by <- intersect(partners(v), factors)
    spec <- stats::as.formula(paste("~", if (length(by) > 0L) paste(by, collapse = "*") else "1"))
    et <- suppressMessages(emmeans::emtrends(model, spec, var = v, data = df))
    parts <- c(parts, paste0(
      "Slope of ", v, " (change of the response per unit of ", v, ")",
      if (length(by) > 0L) paste0(" for each level of ", paste(by, collapse = "/")), ":\n",
      table(et),
      if (length(by) > 0L) paste0("\nDifferences of the slopes:\n", table(graphics::pairs(et)))
    ))
  }
  paste(parts, collapse = "\n\n")
}

create_formula_tool_text_V1_2 <- function(res, DataModelState, args) {
  f <- DataModelState$formula@formula
  df <- DataModelState$df
  if (!inherits(DataModelState$formula, "LinearFormula")) {
    return(paste0("Formula: ", paste(deparse(f), collapse = " "), "\n\n", llm_text(res)))
  }
  model <- lm(f, data = df)
  paste0(
    "Formula: ", paste(deparse(f), collapse = " "), "\n\n",
    "Effects (use these for the interpretation):\n\n",
    create_formula_effects_text_V1_2(model, f, df), "\n\n",
    "Raw coefficients (depend on the reference levels; prefer the effects above).\n",
    llm_text(res)
  )
}

# Names the groups behind estimate1/estimate2, small models mix them up
t_test_tool_text_V1_2 <- function(res, DataModelState, args) {
  f <- DataModelState$formula@formula
  groups <- levels(factor(stats::model.frame(f, DataModelState$df)[[2]]))
  if (length(groups) != 2L) {
    return(llm_text(res))
  }
  r4 <- llm_format_V1_2
  paste0(
    res$method, " (alternative: ", res$alternative, ")\n",
    "Groups of ", deparse(f[[3]]), ":\n",
    "  ", groups[1], ": mean ", r4(res$estimate1), "\n",
    "  ", groups[2], ": mean ", r4(res$estimate2), "\n",
    "Difference ", groups[1], " - ", groups[2], ": ", r4(res$estimate), "\n",
    "Confidence interval of the difference: ",
    r4(res$conf.low), " to ", r4(res$conf.high), "\n",
    "t = ", r4(res$statistic), ", df = ", r4(res$parameter),
    ", p = ", r4(res$p.value)
  )
}

diagnostic_plots_fit_model_V1_2 <- function(df, formula) {
  if (inherits(formula, "LinearMixedFormula")) {
    return(lmerTest::lmer(formula@formula, data = df))
  }
  if (inherits(formula, "GeneralisedLinearFormula")) {
    family <- env_utils_V1_2$build_glm_family(formula@family, formula@link_fct)
    return(glm(formula@formula, data = df, family = family))
  }
  lm(formula@formula, data = df)
}

# The LLM cannot see the plots, so their patterns are condensed into numbers.
# Thresholds follow the ones used to highlight points in diagnostic_plots()
diagnostic_plots_tool_text_V1_2 <- function(res, DataModelState, args) {
  model <- diagnostic_plots_fit_model_V1_2(DataModelState$df, DataModelState$formula)
  resids <- residuals(model)
  fitted_values <- as.numeric(fitted(model))
  n <- length(resids)
  r4 <- llm_format_V1_2
  resid_sd <- stats::sd(resids)

  k <- max(2L, min(5L, n %/% 10L))
  breaks <- unique(stats::quantile(fitted_values, seq(0, 1, length.out = k + 1L)))
  bins <- cut(fitted_values, breaks, include.lowest = TRUE)
  bin_mean <- tapply(resids, bins, mean)
  bin_sd <- tapply(resids, bins, stats::sd)
  bin_n <- tapply(resids, bins, length)
  bin_t <- bin_mean / (resid_sd / sqrt(bin_n))

  # Binning by fitted values mixes groups (e.g. Treatment shifts the fitted
  # values) and can hide curvature; residuals per numeric predictor reveal it
  # From the dataset, not model.frame(): poly(conc, 2) is a matrix column there
  df <- DataModelState$df
  pred_vars <- intersect(all.vars(reformulas::nobars(DataModelState$formula@formula)[[3]]), names(df))
  numeric_preds <- Filter(function(v) is.numeric(df[[v]]), pred_vars)
  per_predictor <- lapply(numeric_preds, function(v) {
    x <- df[names(resids), v]
    groups <- if (length(unique(x)) <= 10L) {
      factor(x)
    } else {
      cut(x, unique(stats::quantile(x, seq(0, 1, length.out = k + 1L))), include.lowest = TRUE)
    }
    g_mean <- tapply(resids, groups, mean)
    list(mean = g_mean, t = g_mean / (resid_sd / sqrt(tapply(resids, groups, length))))
  })
  names(per_predictor) <- numeric_preds
  per_predictor_text <- vapply(numeric_preds, function(v) {
    g <- per_predictor[[v]]
    paste0(
      "  by ", v, ":\n",
      paste0("    ", names(g$mean), ": ", r4(g$mean), " (", r4(g$t), " standard errors)", collapse = "\n")
    )
  }, character(1))

  z <- (resids - mean(resids)) / resid_sd
  qq_cor <- stats::cor(sort(z), stats::qnorm(stats::ppoints(n)))

  n_coef <- length(if (inherits(model, "merMod")) lme4::fixef(model) else stats::coef(model))
  leverage <- stats::hatvalues(model)
  cooks <- stats::cooks.distance(model)
  high_leverage <- names(resids)[leverage > 2 * (n_coef + 1) / n]
  influential <- utils::head(sort(cooks[cooks > 4 / n], decreasing = TRUE), args[["max_points"]])
  sd_ratio <- max(bin_sd) / min(bin_sd)

  # Small models overlook single lines in long texts, so every
  # violated rule of thumb is repeated at the top
  findings <- character()
  for (b in names(bin_t)[abs(bin_t) > 2]) {
    findings <- c(findings, sprintf("fitted values %s: mean residual %s standard errors from 0, possible systematic pattern", b, r4(bin_t[[b]])))
  }
  for (v in numeric_preds) {
    g_t <- per_predictor[[v]]$t
    for (b in names(g_t)[abs(g_t) > 2]) {
      findings <- c(findings, sprintf("%s = %s: mean residual %s standard errors from 0, possible curvature in %s", v, b, r4(g_t[[b]]), v))
    }
  }
  if (sd_ratio > 2) {
    findings <- c(findings, sprintf("ratio largest/smallest residual SD = %s, possible unequal variance", r4(sd_ratio)))
  }
  if (sum(abs(z) > 3) > 0L) {
    findings <- c(findings, sprintf("%d standardised residuals beyond 3, possible outliers or heavy tails", sum(abs(z) > 3)))
  }
  # 4/n flags points in almost every dataset; as finding only the classic cutoff
  very_influential <- names(cooks)[cooks > 1]
  if (length(very_influential) > 0L) {
    findings <- c(findings, sprintf("very influential rows (Cook's distance > 1): %s", paste(very_influential, collapse = ", ")))
  }
  findings_text <- if (length(findings) == 0L) {
    "  No rule of thumb is violated.\n"
  } else {
    paste0(paste0("  - ", findings, collapse = "\n"), "\n")
  }

  paste0(
    "Diagnostics for ", paste(deparse(DataModelState$formula@formula), collapse = " "),
    " (n = ", n, ", residual SD = ", r4(resid_sd), ")\n\n",
    "Findings (rules of thumb violated, details below):\n", findings_text, "\n",
    "Linearity (residuals vs fitted): mean residual per range of fitted values\n",
    paste0("  ", names(bin_mean), ": ", r4(bin_mean),
      " (", r4(bin_t), " standard errors)", collapse = "\n"), "\n",
    if (length(per_predictor_text) > 0L) {
      paste0("Mean residual per value of each numeric predictor\n", paste(per_predictor_text, collapse = "\n"), "\n")
    },
    "  Rule of thumb: more than 2 standard errors away from 0 in some ranges",
    " suggests a systematic pattern such as curvature.\n\n",
    "Equal variance (scale-location): residual SD per range of fitted values\n",
    paste0("  ", names(bin_sd), ": ", r4(bin_sd), collapse = "\n"), "\n",
    "  Ratio largest/smallest SD: ", r4(sd_ratio),
    " (rule of thumb: above 2 suggests unequal variance)\n\n",
    "Normality (QQ plot):\n",
    "  Correlation of sample and normal quantiles: ", r4(qq_cor), "\n",
    "  Skewness: ", r4(mean(z^3)), ", excess kurtosis: ", r4(mean(z^4) - 3), "\n",
    "  |standardised residual| > 2: ", sum(abs(z) > 2), " of ", n,
    " (about ", round(0.05 * n), " expected), > 3: ", sum(abs(z) > 3), "\n\n",
    "Influential points (row names):\n",
    "  Cook's distance > 4/n: ",
    if (length(influential) == 0L) "none" else paste0(names(influential), " (", r4(influential), ")", collapse = ", "),
    "\n",
    "  High leverage (> 2(p+1)/n): ",
    if (length(high_leverage) == 0L) "none" else paste(utils::head(high_leverage, args[["max_points"]]), collapse = ", ")
  )
}

get_state_V1_2 <- R6::R6Class(
  "get_state_V1_2",
  public = list(
    DataModelState = NULL,
    max_levels = NULL,

    description = paste(
      "Show the active dataset, its columns with types and levels,",
      "and the formula currently defined in the app."
    ),
    # Ollama: ellmer drops the properties field of tools without
    # arguments, which some model templates reject
    params = list(
      max_levels = list(
        type = "integer",
        default = 20L,
        description = "Maximum number of levels shown per column."
      )
    ),

    initialize = function(DataModelState, max_levels) {
      self$DataModelState <- DataModelState
      self$max_levels <- max_levels
    },

    validate = function() {
      stopifnot(
        "max_levels has to be a positive number" =
          length(self$max_levels) == 1L && !is.na(self$max_levels) && self$max_levels >= 1
      )
    },

    eval = function() {
      shiny::isolate({
        df <- self$DataModelState$df
        if (is.null(df)) {
          return("No dataset loaded.")
        }
        formula <- if (is.null(self$DataModelState$formula)) {
          "none"
        } else {
          paste(deparse(self$DataModelState$formula@formula), collapse = " ")
        }
        paste(
          c(
            paste("Active dataset:", self$DataModelState$active_df_name),
            sprintf("Rows: %d", nrow(df)),
            paste("Formula:", formula),
            "Columns:",
            mapply(private$column_info, names(df), df)
          ),
          collapse = "\n"
        )
      })
    }
  ),
  private = list(
    column_info = function(name, x) {
      if (is.numeric(x)) {
        return(sprintf("%s: numeric, range %s to %s", name,
          format(min(x, na.rm = TRUE)), format(max(x, na.rm = TRUE))))
      }
      lvls <- if (is.factor(x)) levels(x) else sort(unique(as.character(x)))
      shown <- utils::head(lvls, self$max_levels)
      more <- if (length(lvls) > self$max_levels) sprintf(", ... (%d levels)", length(lvls)) else ""
      sprintf("%s: %s, levels: %s%s", name, class(x)[1],
        paste(shown, collapse = ", "), more)
    }
  )
)

get_results_V1_2 <- R6::R6Class(
  "get_results_V1_2",
  public = list(
    all_data = NULL,
    name = NULL,

    description = paste(
      "Read the results the user created in the app.",
      "Without a name, list all results with their type;",
      "with a name, show the content of that result."
    ),
    # Not called `name`: it would shadow the tool name inside llm_make_tool_V1_2
    params = list(
      result_name = list(
        type = "string",
        default = "",
        description = "Name of a result exactly as listed. Leave empty to list all results."
      )
    ),

    initialize = function(all_data, name) {
      self$all_data <- all_data
      self$name <- name
    },

    validate = function() {
      stopifnot(
        "name has to be a single string" =
          is.character(self$name) && length(self$name) == 1L && !is.na(self$name)
      )
      if (nzchar(self$name) && !self$name %in% names(self$all_data)) {
        stop("Unknown result: ", self$name, ". Available: ", paste(names(self$all_data), collapse = ", "))
      }
    },

    eval = function() {
      if (length(self$all_data) == 0L) {
        return("The user has not created any results yet.")
      }
      if (!nzchar(self$name)) {
        types <- vapply(self$all_data, function(x) class(x)[1], character(1))
        return(paste(c("Results:", paste0(names(self$all_data), ": ", types)), collapse = "\n"))
      }
      llm_text(self$all_data[[self$name]])
    }
  )
)

# Here and not in LLM_Tools.R: the class below needs it at load time
llm_toolboxes_V1_2 <- c(
  data = "Summaries per group, intermediate variables (e.g. subsets), new columns and switching the active dataset. No formula needed.",
  visualisation = "Boxplots, scatterplots, lineplots, histograms of the data (no formula needed) and plots of the model predictions.",
  assumptions = "Check the model assumptions: diagnostic plots, normality, equal variances.",
  tests = "Global tests: t-test/Wilcoxon for two groups, ANOVA/Kruskal-Wallis/permutation ANOVA for more groups.",
  posthoc = "Post hoc and pairwise comparisons after a significant global test.",
  correlation = "Correlation between the response and a numeric predictor.",
  dose_response = "Dose response curves (IC values) and primary assays."
)

# Hand-picked from env_check_ast_V1_2$allowed_fcts(): the whole list
# would not fit into the small context windows of local models
llm_wrangling_help_V1_2 <- function(df_name) {
  paste0(
    "Data wrangling rules for create_intermediate_var and create_new_col:\n",
    "- The active dataset is called ", df_name, ", its columns are available by name.\n",
    "- Operators: + - * / ^ ( ), comparisons == != < > <= >=, & for and, | for or.\n",
    "- Functions: Mean, SD, Median, Sum, Min, Max (capitalised!), quantile, range, length,",
    " log, log10, sqrt, exp, abs, round, floor, ceiling, scale,",
    " as.numeric, as.character, as.factor, as.logical, paste, paste0, C (combine), Seq.\n",
    "- Subsets: get_rows(", df_name, ", condition) keeps the rows where condition is TRUE,",
    " e.g. get_rows(", df_name, ", Type == 'Quebec'); get_cols(", df_name, ", col1, col2) keeps columns.\n",
    "- Not allowed: [ ], $, ifelse, mean/sd (lowercase) and any other function."
  )
}

choose_toolbox_V1_2 <- R6::R6Class(
  "choose_toolbox_V1_2",
  public = list(
    toolbox = NULL,

    description = paste(
      "Choose the toolbox for the next step. Only the tools of the chosen toolbox",
      "are available afterwards; choosing another toolbox replaces them.",
      "Toolboxes:",
      paste0(names(llm_toolboxes_V1_2), ": ", llm_toolboxes_V1_2, collapse = " ")
    ),
    params = list(
      toolbox = list(
        type = "enum",
        values = names(llm_toolboxes_V1_2),
        description = "Toolbox to use."
      )
    ),

    initialize = function(toolbox) {
      self$toolbox <- toolbox
    },

    validate = function() {
      if (!isTRUE(self$toolbox %in% names(llm_toolboxes_V1_2))) {
        stop("toolbox has to be one of: ", paste(names(llm_toolboxes_V1_2), collapse = ", "))
      }
    },

    eval = function(ChatState, DataModelState, DataWranglingState) {
      tools <- llm_toolbox_tools_V1_2(self$toolbox, DataModelState)
      if (length(tools) == 0L) {
        return(paste0(
          "The toolbox ", self$toolbox, " has no tools for the current formula.",
          " Define a suitable formula with create_formula first."
        ))
      }
      ChatState$toolbox <- self$toolbox
      text <- paste0("Toolbox ", self$toolbox, " chosen. Available tools now: ", paste(tools, collapse = ", "))
      if (self$toolbox == "data") {
        text <- paste0(text, "\n\n", llm_wrangling_help_V1_2(DataWranglingState$df_name))
      }
      text
    }
  )
)

llm_check_column_V1_2 <- function(value, arg_name, df, optional = TRUE) {
  ok <- is.character(value) && length(value) == 1L &&
    ((optional && value == "") || value %in% names(df))
  if (!ok) {
    stop(
      arg_name, " has to be one of the columns", if (optional) " or empty", ": ",
      paste(names(df), collapse = ", ")
    )
  }
  value
}

llm_check_line_V1_2 <- function(df, columns) {
  columns <- unique(columns[nzchar(columns)])
  if (anyDuplicated(df[columns]) > 0L) {
    stop(
      "A line plot needs exactly one value per combination of ",
      paste(columns, collapse = ", "),
      ", otherwise the lines zigzag. Use dot instead."
    )
  }
}

llm_label_V1_2 <- function(label, column) {
  if (length(label) == 1L && nzchar(label)) label else column
}

# Padding as in the sliders of the visualisation UI
llm_vis_range_V1_2 <- function(range, df, column, padding, arg_name) {
  values <- df[[column]]
  if (length(range) == 0L) {
    if (is.numeric(values)) {
      return(list(min(values, na.rm = TRUE) * padding[1], max(values, na.rm = TRUE) * padding[2]))
    }
    choices <- as.character(unique(values))
    return(list(choices[1], choices[length(choices)]))
  }
  if (length(range) != 2L) {
    stop(arg_name, " has to contain exactly two values")
  }
  if (is.numeric(values)) {
    range <- suppressWarnings(as.numeric(range))
    if (anyNA(range)) stop(arg_name, " has to contain two numbers")
    return(as.list(range))
  }
  if (!all(range %in% values)) {
    stop(arg_name, " has to contain two levels of ", column, ": ", paste(unique(values), collapse = ", "))
  }
  as.list(range)
}

eval_tool_V1_2 <- function(name, args, DataModelState, ResultsState, DataWranglingState) {
  formula <- DataModelState$formula
  if (!name %in% llm_no_formula_tools_V1_2 && is.null(formula)) {
    stop("No formula defined. Define a formula first.")
  }
  if (!name %in% llm_read_tools_V1_2 && is.null(DataModelState$df)) {
    stop("No dataset loaded.")
  }
  switch(
    name,
    get_state = {
      res <- get_state_V1_2$new(DataModelState, args[["max_levels"]])
      res$validate()
      res$eval()
    },
    get_results = {
      res <- get_results_V1_2$new(ResultsState$all_data, args[["result_name"]])
      res$validate()
      res$eval()
    },
    # Checks live here and not in validate(): the history passes
    # response_var as a call object (f[2]) instead of a string
    create_formula = {
      df <- DataModelState$df
      response <- args[["response"]]
      predictors <- args[["predictors"]]
      if (!is.character(response) || length(response) != 1L || !response %in% names(df)) {
        stop("response has to be one of the columns: ", paste(names(df), collapse = ", "))
      }
      if (!is.character(predictors) || length(predictors) != 1L || grepl("~", predictors, fixed = TRUE)) {
        stop("predictors has to be the right-hand side of the formula without ~")
      }
      res <- create_formula_V1_2$new(response, predictors, df, backend_communicator_V1_2)
      res$validate()
      switch(
        args[["model_type"]],
        "Generalised Linear Model" = res$eval(
          ResultsState, DataModelState, args[["model_type"]],
          args[["family"]], args[["link_fct"]]
        ),
        "Optimization Model" = res$eval(
          ResultsState, DataModelState, args[["model_type"]],
          args[["optim_method"]], args[["lower"]], args[["upper"]], args[["seed"]]
        ),
        res$eval(ResultsState, DataModelState, args[["model_type"]])
      )
    },
    wilcox_rank_sum = {
      res <- wilcox_rank_sum_V1_2$new(
        DataModelState$df, formula, args[["alternative_hyp"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    correlation = {
      res <- correlation_V1_2$new(
        DataModelState$df, formula,
        args[["method"]], args[["alternative"]], args[["conflevel"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    statistical_tests = {
      res <- statistical_tests_V1_2$new(
        DataModelState$df, formula,
        args[["balanced_design"]], args[["p_val"]], args[["p_val_adj_method"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$validate_method(args[["method"]])
      res$eval(ResultsState, args[["method"]])
    },
    pairwise_comparisons = {
      res <- pairwise_comparisons_V1_2$new(
        DataModelState$df, formula,
        args[["p_val"]], args[["alternative_hyp"]], args[["p_val_adj_method"]],
        args[["parametric_or_non_parametric"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    pairwise_comparisons_linear_mixed = {
      res <- pairwise_comparisons_linear_mixed_V1_2$new(
        DataModelState$df, formula, args[["method"]], args[["p_val"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    shapiro_on_data = {
      res <- shapiro_on_data_V1_2$new(DataModelState$df, formula, backend_communicator_V1_2)
      res$validate()
      res$eval(ResultsState)
    },
    shapiro_on_residuals = {
      res <- shapiro_on_residuals_V1_2$new(DataModelState$df, formula, backend_communicator_V1_2)
      res$validate()
      res$eval(ResultsState)
    },
    levene = {
      res <- levene_V1_2$new(DataModelState$df, formula, args[["center"]], backend_communicator_V1_2)
      res$validate()
      res$eval(ResultsState)
    },
    dose_response = {
      res <- dose_response_V1_2$new(
        DataModelState$df, args[["ic_percentage"]],
        args[["is_xlog"]], args[["is_ylog"]],
        args[["substance_names"]], args[["unit_names"]],
        formula, args[["type"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, paste0(ResultsState$counter + 1, " DoseResponse"))
    },
    primary_assay = {
      res <- primary_assay_V1_2$new(
        DataModelState$df, formula,
        args[["neg_control_name"]], args[["pos_control_name"]],
        args[["pval_adj_method"]], args[["fold_or_percentage"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    create_summary = {
      res <- create_summary_V1_2$new(
        DataModelState$df, args[["column_by"]], args[["for_which"]], args[["what_to_do"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    create_summary_plot = {
      res <- create_summary_plot_V1_2$new(
        DataModelState$df, args[["column_by"]], args[["for_which"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    # eval() takes the grouped path for any non NULL group_apply
    create_intermediate_var = {
      groups <- args[["group_apply"]]
      if (length(groups) == 0L) {
        groups <- NULL
      }
      res <- create_intermediate_var_V1_2$new(
        DataWranglingState$df, DataWranglingState$df_name,
        DataWranglingState$intermediate_vars,
        args[["operation"]], args[["var_name"]], groups,
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, DataWranglingState)
    },
    # The class does not know all_data, so the name is checked here
    set_active_table = {
      table_name <- args[["table_name"]]
      tables <- names(Filter(is.data.frame, ResultsState$all_data))
      if (!isTRUE(table_name %in% tables)) {
        stop("table_name has to be one of the datasets: ", paste(tables, collapse = ", "))
      }
      res <- set_active_table_V1_2$new(table_name)
      res$validate()
      res$eval(ResultsState, DataModelState)
      paste0(
        "The active dataset is now ", table_name, ".\n",
        get_state_V1_2$new(DataModelState, 20L)$eval()
      )
    },
    create_new_col = {
      res <- create_new_col_V1_2$new(
        DataWranglingState$df, DataWranglingState$df_name,
        DataWranglingState$intermediate_vars,
        args[["operation"]], args[["column_name"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState, DataWranglingState, DataModelState)
    },
    t_test = {
      res <- t_test_V1_2$new(
        DataModelState$df, formula,
        args[["variances_equal"]],
        args[["conf_level"]],
        args[["alternative_hyp"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    perm_ANOVA = {
      res <- perm_ANOVA_V1_2$new(
        DataModelState$df, formula,
        args[["perm"]],
        args[["seed"]],
        class(formula),
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    diagnostic_plots = {
      res <- diagnostic_plots_V1_2$new(
        DataModelState$df, formula,
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    visualisation = {
      df <- DataModelState$df
      x <- llm_check_column_V1_2(args[["x"]], "x", df, optional = FALSE)
      y <- llm_check_column_V1_2(args[["y"]], "y", df, optional = FALSE)
      colour_var <- llm_check_column_V1_2(args[["colour_var"]], "colour_var", df)
      fill_var <- llm_check_column_V1_2(args[["fill_var"]], "fill_var", df)
      facet_var <- llm_check_column_V1_2(args[["facet_var"]], "facet_var", df)
      if (args[["type_of_x"]] == "numeric" && !is.numeric(df[[x]])) {
        stop("type_of_x numeric needs a numeric x column; ", x, " is not numeric")
      }
      if (args[["method"]] != "box" && fill_var != "") {
        stop("fill_var is only available for boxplots; use colour_var instead")
      }
      if (args[["method"]] == "line") {
        llm_check_line_V1_2(df, c(x, colour_var, facet_var))
      }
      res <- visualisation_V1_2$new(
        df = df, x = x, y = y, method = args[["method"]],
        xlabel = llm_label_V1_2(args[["xlabel"]], x), type_of_x = args[["type_of_x"]],
        ylabel = llm_label_V1_2(args[["ylabel"]], y),
        colour_var = colour_var,
        colour_legend_title = llm_label_V1_2(args[["colour_legend_title"]], colour_var),
        colour_theme = args[["colour_theme"]],
        fill_var = fill_var,
        fill_legend_title = llm_label_V1_2(args[["fill_legend_title"]], fill_var),
        fill_theme = args[["fill_theme"]],
        facet_var = facet_var, facet_y_scaling = args[["facet_y_scaling"]],
        xrange = llm_vis_range_V1_2(args[["xrange"]], df, x, c(0.5, 1.25), "xrange"),
        yrange = llm_vis_range_V1_2(args[["yrange"]], df, y, c(0.95, 1.05), "yrange"),
        width = args[["width"]], height = args[["height"]], resolution = args[["resolution"]],
        com = backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    histogram = {
      df <- DataModelState$df
      y <- llm_check_column_V1_2(args[["y"]], "y", df, optional = FALSE)
      if (!is.numeric(df[[y]])) {
        stop("y has to be a numeric column; ", y, " is not numeric")
      }
      fill_var <- llm_check_column_V1_2(args[["fill_var"]], "fill_var", df)
      facet_var <- llm_check_column_V1_2(args[["facet_var"]], "facet_var", df)
      res <- hist_V1_2$new(
        df = df, y = y, ylabel = llm_label_V1_2(args[["ylabel"]], y),
        fill_var = fill_var,
        fill_legend_title = llm_label_V1_2(args[["fill_legend_title"]], fill_var),
        fill_theme = args[["fill_theme"]],
        facet_var = facet_var, facet_y_scaling = args[["facet_y_scaling"]],
        frequency_or_density = args[["frequency_or_density"]], bins = args[["bins"]],
        width = args[["width"]], height = args[["height"]], resolution = args[["resolution"]],
        com = backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    visualisation_model = {
      message <- check_visualization2(DataModelState)
      if (!is.null(message)) stop(message)
      if (args[["layer"]] == "line") {
        preds <- utils::head(all.vars(reformulas::nobars(formula@formula)[[3]]), 4L)
        # ggplot groups by a discrete x, so each line would be a single point
        if (!is.numeric(DataModelState$df[[preds[1]]])) {
          stop("A line plot needs a numeric first predictor; ", preds[1], " is not numeric. Use dot or box instead.")
        }
        llm_check_line_V1_2(DataModelState$df, preds)
      }
      res <- visualisation_model_V1_2$new(
        DataModelState$df, formula, args[["layer"]],
        backend_communicator_V1_2
      )
      res$validate()
      res$eval(ResultsState)
    },
    stop("Unknown tool: ", name)
  )
}

# Long vectors and subsets would fill the small context windows of local models
# Vectors are stored as one column data.frame by create_intermediate_var
create_intermediate_var_tool_text_V1_2 <- function(res) {
  if (!is.data.frame(res) || nrow(res) <= 10L) {
    return(llm_text(res))
  }
  if (ncol(res) == 1L) {
    return(paste0(
      "Vector of length ", nrow(res), ", first values: ",
      paste(utils::head(res[[1L]], 10L), collapse = ", ")
    ))
  }
  paste0(
    "Dataset with ", nrow(res), " rows. Activate it with set_active_table to analyse it.\n",
    "Columns: ", paste(names(res), collapse = ", ")
  )
}

llm_group_stats_V1_2 <- function(df, y, groups) {
  groups <- unique(groups[nzchar(groups)])
  key <- if (length(groups) == 0L) {
    factor(rep("all", nrow(df)))
  } else {
    interaction(df[groups], drop = TRUE, lex.order = TRUE)
  }
  rows <- lapply(split(seq_len(nrow(df)), key), function(i) {
    v <- df[[y]][i]
    v <- v[!is.na(v)]
    q <- stats::quantile(v, c(0.25, 0.5, 0.75), names = FALSE)
    iqr <- q[3] - q[1]
    stats_row <- data.frame(
      n = length(v), mean = mean(v), sd = stats::sd(v), min = min(v),
      q1 = q[1], median = q[2], q3 = q[3], max = max(v),
      outliers = sum(v < q[1] - 1.5 * iqr | v > q[3] + 1.5 * iqr)
    )
    if (length(groups) == 0L) stats_row else cbind(df[i[1], groups, drop = FALSE], stats_row)
  })
  res <- do.call(rbind, rows)
  rownames(res) <- NULL
  num <- vapply(res, is.double, logical(1))
  res[num] <- lapply(res[num], signif, 4)
  res
}

# The LLM cannot see the plots; without numbers it invents their content
llm_vis_data_text_V1_2 <- function(name, res, DataModelState, args) {
  df <- DataModelState$df
  if (name == "histogram") {
    y <- args[["y"]]
    groups <- c(args[["fill_var"]], args[["facet_var"]])
  } else if (name == "visualisation_model") {
    f <- reformulas::nobars(DataModelState$formula@formula)
    y <- all.vars(f[[2]])[1]
    groups <- utils::head(all.vars(f[[3]]), 4L)
    for (g in groups) {
      if (is.numeric(df[[g]]) && length(unique(df[[g]])) > 10L) {
        breaks <- unique(stats::quantile(df[[g]], seq(0, 1, length.out = 6L), na.rm = TRUE))
        df[[g]] <- cut(df[[g]], breaks, include.lowest = TRUE)
      }
    }
  } else {
    y <- args[["y"]]
    x <- args[["x"]]
    # Numeric x with many values: bins instead of one row per value
    if (is.numeric(df[[x]]) && length(unique(df[[x]])) > 10L) {
      breaks <- unique(stats::quantile(df[[x]], seq(0, 1, length.out = 6L), na.rm = TRUE))
      df[[x]] <- cut(df[[x]], breaks, include.lowest = TRUE)
    }
    groups <- c(x, args[["colour_var"]], args[["fill_var"]], args[["facet_var"]])
  }
  if (!is.numeric(df[[y]])) {
    return(llm_text(res))
  }
  paste0(
    llm_text(res), "\n\n",
    "Data shown in the plot: ", y, " per group",
    " (outliers: points beyond 1.5 IQR, drawn as dots in boxplots).",
    " Describe the plot only with these numbers.\n",
    llm_table_text_V1_2(llm_group_stats_V1_2(df, y, groups), max_rows = 60L)
  )
}

# An empty table made the LLM retry with other settings as if it were an error
llm_pairwise_text_V1_2 <- function(res, args) {
  p_val <- args[["p_val"]]
  if (is.data.frame(res) && nrow(res) == 0L) {
    return(paste0(
      "No comparison has an adjusted p value <= ", p_val, ". ",
      "This is a result, not an error: no pair differs significantly. ",
      "Do not retry; use p_val = 1 only if the user wants all comparisons listed."
    ))
  }
  text <- paste0("Only comparisons with an adjusted p value <= ", p_val, " are listed.\n")
  if (is.data.frame(res) && "comparison" %in% names(res)) {
    text <- paste0(
      text,
      "Columns: comparison 'level' = difference of the response between the groups, ",
      "'slope of x' = difference of the slopes of x between the groups; ",
      "at_value = value of at_variable at which the groups are compared ",
      "(NA: numeric predictors at their mean, the difference is the same for every value).\n"
    )
  }
  # Levels and slopes at every value of a numeric predictor exceed the default limit
  body <- if (is.data.frame(res)) llm_table_text_V1_2(res, max_rows = 200L) else llm_text(res)
  paste0(text, body)
}

tool_text_V1_2 <- function(name, res, DataModelState, args) {
  switch(
    name,
    create_formula = create_formula_tool_text_V1_2(res, DataModelState, args),
    t_test = t_test_tool_text_V1_2(res, DataModelState, args),
    diagnostic_plots = diagnostic_plots_tool_text_V1_2(res, DataModelState, args),
    create_intermediate_var = create_intermediate_var_tool_text_V1_2(res),
    pairwise_comparisons = ,
    pairwise_comparisons_linear_mixed = llm_pairwise_text_V1_2(res, args),
    visualisation = ,
    visualisation_model = ,
    histogram = llm_vis_data_text_V1_2(name, res, DataModelState, args),
    # The result is the whole dataset
    create_new_col =paste0("Added the column ", make.names(args[["column_name"]]), " to the active dataset."),
    llm_text(res)
  )
}
