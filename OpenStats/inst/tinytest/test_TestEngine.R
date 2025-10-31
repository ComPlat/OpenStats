coverage_test <- nzchar(Sys.getenv("R_COVR"))
run_test <- function(f) {
  if (coverage_test) f(FALSE) else f(TRUE)
}
library(OpenStats)
library(tinytest)

# Test create formula
# =======================================================================================
test_create_formula <- function() {
  options(OpenStats.background = FALSE)
  df <- CO2
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)

  cf <- OpenStats:::create_formula_V1_2$new(
    response_var = "uptake",
    right_site = "conc",
    df = df,
    com = OpenStats:::backend_communicator_V1_2
  )
  eq <- cf$eval(ResultsState, DataModelState, "Linear")
  check1 <- expect_equal(DataModelState$formula@formula, as.formula("uptake ~ conc"))
  check2 <- expect_true(is.character(eq)) # extract_eq() returns a LaTeX-like string
  checks <- c(check1, check2)
  expect_true(all(checks))
}
test_create_formula()

test_summary_model <- function() {
  options(OpenStats.background = FALSE)
  df <- CO2
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
  formula <- as.formula("uptake ~ conc")
  formula <- new("LinearFormula", formula = formula)

  sum_model <- OpenStats:::summarise_model_V1_2$new(
    df, formula, com = OpenStats:::backend_communicator_V1_2
  )
  sum_model$validate()
  sum_model$eval(ResultsState)
  res <- ResultsState$all_data[[length(ResultsState$all_data)]]
  check1 <- expect_true(inherits(res, "summaryModel"))
  model <- lm(uptake ~ conc, data = CO2)
  check2 <- expect_equal(
    res@summary, broom::tidy(model)
  )
  check3 <- expect_equal(
    res@information_criterions, data.frame(AIC = AIC(model), BIC = BIC(model))
  )
  check4 <- expect_true(inherits(res@p, "plot"))
  checks <- c(check1, check2, check3, check4)
  expect_true(all(checks))
}
test_summary_model()

# Test correlation_V1_2
# =======================================================================================
test_corr <- function() {
  options(OpenStats.background = FALSE)
  df <- CO2
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)

  formula <- as.formula("uptake ~ conc")
  formula <- new("LinearFormula", formula = formula)

  corr <- OpenStats:::correlation_V1_2$new(CO2, formula, "pearson", "two.sided",
    0.95, OpenStats:::backend_communicator_V1_2)
  trash <- corr$validate()
  corr$eval(ResultsState)

  check1 <- expect_equal(
    ResultsState$all_data[[2]],
    broom::tidy(
      cor.test(
        df$uptake,
        df$conc,
        data = df,
        method = "pearson",
        alternative = "two.sided",
        conf.level = 0.95
      )
    )
  )

  corr$method <- "kendall"
  corr$eval(ResultsState)
  check2 <- expect_equal(
    ResultsState$all_data[[3]],
    broom::tidy(
      cor.test(
        CO2$uptake,
        CO2$conc,
        data = CO2,
        method = "kendall",
        alternative = "two.sided",
        conf.level = 0.95
      )
    )
  )

  corr$method <- "spearman"
  corr$eval(ResultsState)
  check3 <- expect_equal(
    ResultsState$all_data[[4]],
    broom::tidy(
      cor.test(
        CO2$uptake,
        CO2$conc,
        data = CO2,
        method = "spearman",
        alternative = "two.sided",
        conf.level = 0.95
      )
    )
  )

  corr$method <- "spearman"
  corr$conflevel <- 0.5
  corr$eval(ResultsState)
  check4 <- expect_equal(
    ResultsState$all_data[[5]],
    broom::tidy(
      cor.test(
        CO2$uptake,
        CO2$conc,
        data = CO2,
        method = "spearman",
        alternative = "two.sided",
        conf.level = 0.5
      )
    )
  )

  corr$method <- "pearson"
  corr$conflevel <- 0.5
  corr$alternative <- "less"
  corr$eval(ResultsState)
  check5 <- expect_equal(
    ResultsState$all_data[[6]],
    broom::tidy(
      cor.test(
        CO2$uptake,
        CO2$conc,
        data = CO2,
        method = "pearson",
        alternative = "less",
        conf.level = 0.5
      )
    )
  )
  checks <- c(check1, check2, check3, check4, check5)
  expect_true(all(checks))
}
test_corr()

# Test visualisation_V1_2
# =======================================================================================
test_vis_all <- function(in_background) {
  options(OpenStats.background = in_background)
  ib <- getOption("OpenStats.background", TRUE)
  df <- CO2
  outer_checks <- c()
  for (method in c("box", "dot", "line")) {
    ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
    ResultsState$bgp$in_backend <- TRUE
    DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
    checks <- c()
    vis <- OpenStats:::visualisation_V1_2$new(
      df = df,
      x = "conc",
      y = "uptake",
      method = method,
      xlabel = paste("xlabel", method),
      type_of_x = if (method == "box") "factor" else "numeric",
      ylabel = paste("ylabel", method),
      colour_var = "",
      colour_legend_title = "Legend colour",
      colour_theme = "Dark2",
      fill_var = "",
      fill_legend_title = "Legend fill",
      fill_theme = "YlOrRd",
      facet_var = if (method == "dot") "Treatment" else "",
      facet_y_scaling = "fixed",
      xrange = c(0, 1000),
      yrange = c(0, 60),
      width = 20,
      height = 20,
      resolution = 150,
      com = OpenStats:::backend_communicator_V1_2
    )

    p <- vis$eval(ResultsState)
    if(ib) OpenStats:::backend_get_result_V1_2(ResultsState)
    p <- ResultsState$all_data[[length(ResultsState$all_data)]]
    # Basic checks
    checks <- c(checks, expect_true(inherits(p, "plot")))
    checks <- c(checks, expect_equal(length(ResultsState$all_data), 2))
    checks <- c(checks, expect_equal(length(ResultsState$history), 1))

    # History correctness
    h <- ResultsState$history[[1]]
    checks <- c(checks, expect_equal(h$type, "Visualisation"))
    checks <- c(checks, expect_equal(h$`Plot-type`, method))
    checks <- c(checks, expect_equal(h$`X axis label`, paste("xlabel", method)))
    checks <- c(checks, expect_equal(h$`Y axis label`, paste("ylabel", method)))

    # Result name check
    checks <- c(checks, expect_match(names(ResultsState$all_data)[[2]], paste0("1 Visualization ", 
      c(box = "Boxplot", dot = "Scatterplot", line = "Lineplot")[method])))

    outer_checks <- c(outer_checks, all(checks))
  }
  expect_true(all(outer_checks))
}
run_test(test_vis_all)

test_vis_warn_size <- function(in_background) {
  options(OpenStats.background = in_background)
  ib <- getOption("OpenStats.background", TRUE)
  df <- CO2
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
  vis <- OpenStats:::visualisation_V1_2$new(
    df = CO2, x = "conc", y = "uptake", method = "box",
    xlabel = "x", type_of_x = "factor", ylabel = "y",
    colour_var = "", colour_legend_title = "", colour_theme = "Accent",
    fill_var = "", fill_legend_title = "", fill_theme = "BuGn",
    facet_var = "", facet_y_scaling = "free",
    xrange = c(0, 100), yrange = c(0, 100),
    width = -5, height = 200, resolution = 72,
    com = OpenStats:::backend_communicator_V1_2
  )
  vis$validate()
  checks1 <- expect_equal(vis$width, 10)
  checks2 <- expect_equal(vis$height, 100)
  expect_true(all(c(checks1, checks2)))
}
run_test(test_vis_warn_size)

# Plot model
# =======================================================================================
test_plot_model <- function(in_background) {
  options(OpenStats.background = in_background)
  ib <- getOption("OpenStats.background", TRUE)
  df <- CO2
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
  formula <- uptake ~ conc*Treatment
  formula <- new("LinearFormula", formula = formula)
  plot_model <- OpenStats:::visualisation_model_V1_2$new(df, formula, "box")
  plot_model$validate()
  plot_model$eval(ResultsState)
  if(ib) OpenStats:::backend_get_result_V1_2(ResultsState)
  p <- ResultsState$all_data[[length(ResultsState$all_data)]]
  check <- expect_true(inherits(p, "plot"))
  expect_true(check)
}
run_test(test_plot_model)

# Filter data
# =======================================================================================
test_apply_filter <- function() {
  options(OpenStats.background = FALSE)
  df <- CO2
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)

  af <- OpenStats:::apply_filter_V1_2$new("Plant", c("Qn1", "Qn2"), OpenStats:::backend_communicator_V1_2)
  af$validate()
  af$eval(DataModelState, ResultsState)

  check1 <- expect_true("Qn1" %in% unique(DataModelState$df$Plant))
  check2 <- expect_true("Qn2" %in% unique(DataModelState$df$Plant))
  check3 <- expect_equal(DataModelState$filter_col, "Plant")
  check4 <- expect_equal(DataModelState$filter_group, c("Qn1", "Qn2"))
  check5 <- expect_equal(ResultsState$history[[1]]$type, "ApplyFilter")
  checks <- c(check1, check2, check3, check4, check5)
  expect_true(all(checks))
}
test_apply_filter()

test_remove_filter <- function() {
  options(OpenStats.background = FALSE)
  df <- CO2
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
  DataModelState$df <- df[CO2$Plant %in% c("Qn1", "Qn2"), ]
  DataModelState$backup_df <- df
  DataModelState$filter_col <- "Plant"
  DataModelState$filter_group <- c("Qn1", "Qn2")

  rf <- OpenStats:::remove_filter_V1_2$new()
  rf$eval(ResultsState, DataModelState)

  check1 <- expect_equal(nrow(DataModelState$df), nrow(CO2))
  check2 <- expect_null(DataModelState$backup_df)
  check3 <- expect_equal(ResultsState$history[[1]]$type, "RemoveFilter")
  checks <- c(check1, check2, check3)
  expect_true(all(checks))
}
test_remove_filter()

# Data wrangling
# =======================================================================================
test_create_intermediate_var <- function() {
  options(OpenStats.background = FALSE)
  df <- CO2
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
  DataWranglingState <- OpenStats:::backend_data_wrangling_state_V1_2$new(DataModelState)

  ci <- OpenStats:::create_intermediate_var_V1_2$new(
    df = CO2,
    df_name = "CO2",
    intermediate_vars = list(),
    operation = "uptake + conc",
    name = "myVar",
    com = OpenStats:::backend_communicator_V1_2
  )

  ci$validate()
  ci$eval(ResultsState, DataWranglingState)

  check1 <- expect_true("myVar" %in% names(DataWranglingState$intermediate_vars))
  check2 <- expect_equal(ResultsState$counter, 1)
  check3 <- expect_equal(ResultsState$history[[1]]$type, "CreateIntermediateVariable")
  checks <- c(check1, check2, check3)
  expect_true(all(checks))
}
test_create_intermediate_var()

test_create_new_col <- function() {
  options(OpenStats.background = FALSE)
  df <- CO2
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list("df0" = df))
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
  DataModelState$active_df_name = "df0"
  DataWranglingState <- OpenStats:::backend_data_wrangling_state_V1_2$new(DataModelState)

  cc <- OpenStats:::create_new_col_V1_2$new(
    df = df,
    df_name = "CO2",
    intermediate_vars = list(),
    operation = "uptake / conc",
    name = "ratio",
    com = OpenStats:::backend_communicator_V1_2
  )

  cc$validate()
  cc$eval(ResultsState, DataWranglingState, DataModelState)

  check1 <- expect_true("ratio" %in% colnames(DataModelState$df))
  check2 <- expect_equal(ResultsState$counter, 1)
  check3 <- expect_equal(ResultsState$history[[1]]$type, "CreateNewColumn")
  checks <- c(check1, check2, check3)
  expect_true(all(checks))
}
test_create_new_col()

test_remove_intermediate_var <- function() {
  options(OpenStats.background = FALSE)
  df <- CO2
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
  DataWranglingState <- OpenStats:::backend_data_wrangling_state_V1_2$new(DataModelState)
  DataWranglingState$intermediate_vars <- list(foo = 1:3)

  riv <- OpenStats:::remove_intermediate_var_V1_2$new("foo", com = OpenStats:::backend_communicator_V1_2)
  riv$eval(ResultsState, DataWranglingState)

  check1 <- expect_null(DataWranglingState$intermediate_vars$foo)
  check2 <- expect_equal(ResultsState$history[[1]]$type, "RemoveIntermediateVariable")
  checks <- c(check1, check2)
  expect_true(all(checks))
}
test_remove_intermediate_var()

# Statistical tests
# =======================================================================================
test_t_test <- function(in_background) {
  options(OpenStats.background = in_background)
  ib <- getOption("OpenStats.background", TRUE)
  df <- CO2
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
  formula <- as.formula("uptake ~ Treatment")
  formula <- new("LinearFormula", formula = formula)

  tt <- OpenStats:::t_test_V1_2$new(
    df = df,
    formula = formula,
    variances_equal = "eq",
    conf_level = 0.95,
    alternative_hyp = "two.sided",
    com = OpenStats:::backend_communicator_V1_2
  )

  result <- tt$eval(ResultsState)
  if(ib) OpenStats:::backend_get_result_V1_2(ResultsState)
  result <- ResultsState$all_data[[length(ResultsState$all_data)]]

  check1 <- expect_true("estimate" %in% colnames(result))
  check2 <- expect_equal(ResultsState$counter, 1)
  check3 <- expect_equal(ResultsState$history[[1]]$type, "TTest")
  checks <- c(check1, check2, check3)
  expect_true(all(checks))
}
run_test(test_t_test)

test_statistical_methods <- function(in_background) {
  options(OpenStats.background = in_background)
  ib <- getOption("OpenStats.background", TRUE)
  df <- CO2
  df$group <- rep(c("A", "B", "C", "D"), length.out = nrow(df))
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
  formula <- as.formula("uptake ~ group")
  formula <- new("LinearFormula", formula = formula)

  outer_checks <- c()
  methods <- c("aov", "kruskal", "HSD", "kruskalTest", "LSD", "scheffe", "REGW")
  for (method in methods) {
    ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
    ResultsState$bgp$in_backend <- TRUE
    st <- OpenStats:::statistical_tests_V1_2$new(
      df = df,
      formula = formula,
      balanced_design = "Balanced",
      p_val = 0.05,
      p_val_adj_method = "bonferroni",
      com = OpenStats:::backend_communicator_V1_2
    )

    result <- st$eval(ResultsState, method = method)
    if(ib) OpenStats:::backend_get_result_V1_2(ResultsState)
    result <- ResultsState$all_data[[length(ResultsState$all_data)]]
    check1 <- expect_true(is.data.frame(result) || is.matrix(result))
    check2 <- expect_equal(ResultsState$counter, 1)
    check3 <- expect_match(ResultsState$history[[1]]$type, "ANOVA|Test|Tukey|post hoc|Least|Scheffe|REGW")
    check4 <- expect_true(length(ResultsState$all_data) == 2)
    checks <- c(check1, check2, check3, check4)
    outer_checks <- c(outer_checks, all(checks))
  }
  expect_true(all(outer_checks))
}
run_test(test_statistical_methods)

test_statistical_methods_glm <- function(in_background) {
  options(OpenStats.background = in_background)
  ib <- getOption("OpenStats.background", TRUE)
  df <- CO2
  df$group <- rep(c("A", "B", "C", "D"), length.out = nrow(df))
  df$Treatment <- factor(df$Treatment)

  formula <- as.formula("Treatment ~ group")
  formula <- methods::new(
    "GeneralisedLinearFormula",
    formula = formula,
    family = "binomial",
    link_fct = "logit"
  )

  outer_checks <- c()
  methods <- c(
    "aov", "kruskal",  # native GLM tests
    "tukey", "sidak", "bonferroni", "scheffe", "none",
    "fdr", "holm", "hochberg", "hommel"  # emmeans
  )

  for (method in methods) {
    ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
    ResultsState$bgp$in_backend <- TRUE
    st <- OpenStats:::statistical_tests_V1_2$new(
      df = df,
      formula = formula,
      balanced_design = "Balanced",
      p_val = 0.05,
      p_val_adj_method = method,
      com = OpenStats:::backend_communicator_V1_2
    )

    result <- st$eval(ResultsState, method = method)

    if(ib) OpenStats:::backend_get_result_V1_2(ResultsState)
    result <- ResultsState$all_data[[length(ResultsState$all_data)]]
    check1 <- expect_true(is.data.frame(result) || is.matrix(result))
    check2 <- expect_equal(ResultsState$counter, 1)
    check3 <- expect_match(ResultsState$history[[1]]$type, "ANOVA|Chisq|PostHoc|Test")
    check4 <- expect_true(length(ResultsState$all_data) == 2)
    checks <- c(check1, check2, check3, check4)
    outer_checks <- c(outer_checks, all(checks))
  }

  expect_true(all(outer_checks))
}
run_test(test_statistical_methods_glm)


# Assumptions
# =======================================================================================
test_shapiro_on_data <- function() {
  options(OpenStats.background = FALSE)
  df <- CO2
  df$group <- rep(c("A", "B"), length.out = nrow(df))
  formula <- as.formula("uptake ~ group")
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
  formula <- new("LinearFormula", formula = formula)
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE

  sh <- OpenStats:::shapiro_on_data_V1_2$new(df = df, formula = formula, com = OpenStats:::backend_communicator_V1_2)
  sh$eval(ResultsState)
  result <- ResultsState$all_data[[2]]
  check1 <- expect_true(is.data.frame(result))
  check2 <- expect_true("Normal distributed" %in% colnames(result))
  check3 <- expect_equal(ResultsState$counter, 1)
  check4 <- expect_equal(ResultsState$history[[1]]$type, "ShapiroOnData")
  checks <- c(check1, check2, check3, check4)
  expect_true(all(checks))
}
test_shapiro_on_data()

test_shapiro_on_residuals <- function() {
  options(OpenStats.background = FALSE)
  df <- CO2
  df$group <- rep(c("A", "B"), length.out = nrow(df))
  formula <- as.formula("uptake ~ group")
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
  formula <- new("LinearFormula", formula = formula)
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE

  sh <- OpenStats:::shapiro_on_residuals_V1_2$new(df = df, formula = formula, com = OpenStats:::backend_communicator_V1_2)
  sh$eval(ResultsState)
  result <- ResultsState$all_data[[2]]

  check1 <- expect_true(is.data.frame(result))
  check2 <- expect_true("Residuals normal distributed" %in% colnames(result))
  check3 <- expect_equal(ResultsState$counter, 1)
  check4 <- expect_equal(ResultsState$history[[1]]$type, "ShapiroOnResiduals")
  checks <- c(check1, check2, check3, check4)
  expect_true(all(checks))
}
test_shapiro_on_residuals()

test_levene <- function() {
  options(OpenStats.background = FALSE)
  df <- CO2
  df$group <- rep(c("A", "B"), length.out = nrow(df))
  formula <- as.formula("uptake ~ group")
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
  formula <- new("LinearFormula", formula = formula)
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE

  lv <- OpenStats:::levene_V1_2$new(df = df, formula = formula, center = "mean", com = OpenStats:::backend_communicator_V1_2)
  lv$eval(ResultsState)
  result <- ResultsState$all_data[[2]]

  check1 <- expect_true(is.data.frame(result))
  check2 <- expect_true("Variance homogenity" %in% colnames(result))
  check3 <- expect_equal(ResultsState$counter, 1)
  check4 <- expect_equal(ResultsState$history[[1]]$type, "LeveneTest")
  checks <- c(check1, check2, check3, check4)
  expect_true(all(checks))
}
test_levene()

test_diagnostic_plots <- function(in_background) {
  options(OpenStats.background = in_background)
  ib <- getOption("OpenStats.background", TRUE)
  df <- CO2
  df$group <- rep(c("A", "B"), length.out = nrow(df))
  formula <- as.formula("uptake ~ group")
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
  formula <- new("LinearFormula", formula = formula)
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE

  dp <- OpenStats:::diagnostic_plots_V1_2$new(df = df, formula = formula, com = OpenStats:::backend_communicator_V1_2)
  dp$eval(ResultsState)
  if(ib) OpenStats:::backend_get_result_V1_2(ResultsState)
  p <- ResultsState$all_data[[length(ResultsState$all_data)]]

  check1 <- expect_true(inherits(p, "plot"))
  check2 <- expect_match(names(ResultsState$all_data[[2]]), "1 Diagnostic plot")
  check3 <- expect_equal(ResultsState$history[[1]]$type, "DiagnosticPlots")
  checks <- c(check1, check2, check3)
  expect_true(all(checks))
}
run_test(test_diagnostic_plots)

# Dose response
# =======================================================================================
test_dose_response <- function(in_background) {
  options(OpenStats.background = in_background)
  ib <- getOption("OpenStats.background", TRUE)
  df <- data.frame(
    dose = rep(c(0.1, 1, 10, 100), each = 5),
    response = c(100, 90, 80, 60, 50, 98, 85, 75, 58, 48, 95, 80, 70, 55, 45, 92, 78, 65, 50, 40),
    name = rep(c("A", "B"), length.out = 20)
  )
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
  formula <- response ~ dose
  formula <- new("LinearFormula", formula = formula)
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE

  dr <- OpenStats:::dose_response_V1_2$new(
    df = df,
    is_xlog = FALSE,
    is_ylog = FALSE,
    substance_names = "name",
    formula = formula,
    com = OpenStats:::backend_communicator_V1_2
  )

  new_name <- "Mock"
  dr$eval(ResultsState, new_name)
  if(ib) OpenStats:::backend_get_result_V1_2(ResultsState)
  res <- ResultsState$all_data[[length(ResultsState$all_data)]]

  check1 <- expect_true(inherits(res, "doseResponse"))
  check2 <- expect_equal(ResultsState$counter, 1)
  check3 <- expect_equal(ResultsState$history[[1]]$type, "DoseResponse")
  checks <- c(check1, check2, check3)
  expect_true(all(checks))
}
run_test(test_dose_response)

# Remove result from ResultList
# =======================================================================================
test_remove_result <- function() {
  options(OpenStats.background = FALSE)
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE
  ResultsState$all_data <- list("MyResult" = 42)
  ResultsState$history <- list()

  rr <- OpenStats:::remove_result_V1_2$new("MyResult")
  rr$eval(ResultsState)

  check1 <- expect_null(ResultsState$all_data$MyResult)
  check2 <- expect_equal(ResultsState$history[[1]]$type, "RemoveResult")

  checks <- c(check1, check2)
  expect_true(all(checks))
}
test_remove_result()
