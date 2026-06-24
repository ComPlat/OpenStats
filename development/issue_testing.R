# install.packages("OpenStats", type = "source", repos = NULL)

library(OpenStats)
library(tinytest)

coverage_test <- nzchar(Sys.getenv("R_COVR"))
run_test <- function(f) {
  if (coverage_test) f(FALSE) else f(TRUE)
}

test_statistical_methods_linear_mixed_models <- function(in_background) {
  options(OpenStats.background = in_background)
  ib <- getOption("OpenStats.background", TRUE)
  df <- lme4::sleepstudy
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
  formula <- as.formula("Reaction ~ Days + (1 | Subject)")
  # environment(formula) <- baseenv()
  formula <- new("LinearMixedFormula", formula = formula)

  outer_checks <- c()

  # ANOVA on the linear mixed model
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE
  st <- OpenStats:::statistical_tests_V1_2$new(
    df = df,
    formula = formula,
    balanced_design = NULL,
    p_val = NULL,
    p_val_adj_method = NULL,
    com = OpenStats:::backend_communicator_V1_2
  )
  st$eval(ResultsState, method = "aov")
  if(ib) OpenStats:::backend_get_result_V1_2(ResultsState)
  result <- ResultsState$all_data[[length(ResultsState$all_data)]]

  expected <- broom::tidy(
    anova(lmerTest::lmer(Reaction ~ Days + (1 | Subject), data = df))
  ) |> as.data.frame()
  check1 <- expect_equal(result, expected)
  check2 <- expect_equal(ResultsState$counter, 1)
  check3 <- expect_equal(ResultsState$history[[1]]$type, "ANOVA Linear Mixed")
  check4 <- expect_true(length(ResultsState$all_data) == 2)
  outer_checks <- c(outer_checks, all(c(check1, check2, check3, check4)))

  # Permutation ANOVA on the linear mixed model
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
  ResultsState$bgp$in_backend <- TRUE
  perm <- 50L
  pa <- OpenStats:::perm_ANOVA_V1_2$new(
    df = df, formula = formula, perm = perm, seed = 1234,
    model_type = "LinearMixedFormula",
    com = OpenStats:::backend_communicator_V1_2
  )
  pa$eval(ResultsState)
  if(ib) OpenStats:::backend_get_result_V1_2(ResultsState)
  result <- ResultsState$all_data[[length(ResultsState$all_data)]]

  set.seed(1234)
  expected <- permutes::perm.lmer(
    Reaction ~ Days + (1 | Subject), data = df, np = perm, type = "anova"
  )
  print(result)
  print(expected)

}
run_test(test_statistical_methods_linear_mixed_models)
