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
    em <- suppressMessages(emmeans::emmeans(model, spec))
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
    et <- suppressMessages(emmeans::emtrends(model, spec, var = v))
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
  mf <- stats::model.frame(model)
  numeric_preds <- Filter(function(v) is.numeric(mf[[v]]), names(mf)[-1])
  per_predictor <- lapply(numeric_preds, function(v) {
    x <- mf[[v]]
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

# Click paths for the workflow guide.
# A step is skipped when its `done` selector matches, e.g. the formula editor is already open
click_path_titles_V1_2 <- c(
  "Create a linear model" = "create_linear_model",
  "Check the assumptions" = "check_assumptions",
  "t test (two groups)" = "t_test",
  "ANOVA (more than two groups)" = "anova",
  "Scatterplot" = "scatterplot"
)

click_path_tab_V1_2 <- function(tab, label) {
  list(
    label = label,
    selector = sprintf("a[data-value='%s']", tab),
    done = sprintf("li.active > a[data-value='%s'], a.active[data-value='%s']", tab, tab)
  )
}

# skip_if_model: as first part of another path, skipped when a model is shown in the sidebar
click_path_linear_model_V1_2 <- function(skip_if_model) {
  model_exists <- if (skip_if_model) ", #OPENFORMULA-formulaUI .var-box-output" else ""
  # "" instead of NULL: jsonlite turns NULL into {}, which is truthy in JS
  only_model_exists <- if (skip_if_model) "#OPENFORMULA-formulaUI .var-box-output" else ""
  list(
    list(
      label = "'Open formula editor' in the sidebar",
      selector = "#OPENFORMULA-open_formula_editor",
      done = paste0("#formula_modal.in, #formula_modal.show", model_exists)
    ),
    list(
      label = "'Change the model type (optional)': choose 'Linear'",
      selector = ".form-group:has(#FO-model_type)",
      done = paste0("#FO-model_type option[value='Linear']:checked", model_exists)
    ),
    list(
      label = "'Dependent Variable': choose the outcome",
      selector = "#FO-colnames_dropdown",
      done = only_model_exists
    ),
    list(
      label = "Predictors: the 'Variables' and 'Functions' buttons or the text field",
      selector = "#FO-buttons",
      done = only_model_exists
    ),
    list(
      label = "'Create statistical model'",
      selector = "#FO-create_formula",
      done = only_model_exists
    ),
    list(
      label = "'Close' the formula editor",
      selector = "#formula_modal [data-dismiss='modal']",
      done = "#formula_modal:not(.in):not(.show)"
    )
  )
}

click_paths_V1_2 <- list(
  create_linear_model = click_path_linear_model_V1_2(skip_if_model = FALSE),
  check_assumptions = c(
    click_path_linear_model_V1_2(skip_if_model = TRUE),
    list(
      click_path_tab_V1_2("Assumption", "Tab 'Assumption'"),
      list(label = "'diagnostic plots' in the sidebar", selector = "#ASS-DiagnosticPlot")
    )
  ),
  t_test = c(
    click_path_linear_model_V1_2(skip_if_model = TRUE),
    list(
      click_path_tab_V1_2("Tests", "Tab 'Tests'"),
      click_path_tab_V1_2("Two groups", "Sub-tab 'Two groups'"),
      list(
        label = "'Test family': 'Parametric'",
        selector = "#TESTS-ParametricOrNonParametric",
        done = "#TESTS-ParametricOrNonParametric input[value='parametric']:checked"
      ),
      list(label = "'t test' in the sidebar", selector = "#TESTS-tTest")
    )
  ),
  anova = c(
    click_path_linear_model_V1_2(skip_if_model = TRUE),
    list(
      click_path_tab_V1_2("Tests", "Tab 'Tests'"),
      click_path_tab_V1_2("More than two groups", "Sub-tab 'More than two groups'"),
      list(
        label = "'Test family': 'Parametric'",
        selector = "#TESTS-ParametricOrNonParametric",
        done = "#TESTS-ParametricOrNonParametric input[value='parametric']:checked"
      ),
      list(label = "'ANOVA' in the sidebar", selector = "#TESTS-aovTest")
    )
  ),
  scatterplot = list(
    click_path_tab_V1_2("Visualisation", "Tab 'Visualisation'"),
    click_path_tab_V1_2("Scatterplot", "Sub-tab 'Scatterplot'"),
    list(label = "'Y Variable' in the sidebar", selector = ".form-group:has(#VIS-yVar)"),
    list(label = "'X Variable' in the sidebar", selector = ".form-group:has(#VIS-xVar)"),
    list(label = "'Create plot'", selector = "#VIS-CreatePlotScatter")
  )
)

eval_tool_V1_2 <- function(name, args, DataModelState, ResultsState) {
  formula <- DataModelState$formula
  if (!name %in% c("create_formula", llm_read_tools_V1_2) && is.null(formula)) {
    stop("No formula defined. Define a formula first.")
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
      if (is.null(df)) {
        stop("No dataset loaded.")
      }
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
      res$eval(ResultsState, DataModelState, "Linear")
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
    stop("Unknown tool: ", name)
  )
}

tool_text_V1_2 <- function(name, res, DataModelState, args) {
  switch(
    name,
    create_formula = create_formula_tool_text_V1_2(res, DataModelState, args),
    t_test = t_test_tool_text_V1_2(res, DataModelState, args),
    diagnostic_plots = diagnostic_plots_tool_text_V1_2(res, DataModelState, args),
    llm_text(res)
  )
}
