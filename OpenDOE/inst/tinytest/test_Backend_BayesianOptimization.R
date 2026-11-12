for (p in c("mlr3mbo", "mlr3learners", "mlr3pipelines", "DiceKriging", "rgenoud", "ranger")) {
  if (!requireNamespace(p, quietly = TRUE)) exit_file(paste("needs", p))
}
library(tinytest)

bo_suggest <- OpenDOE:::bo_suggest

test_continuous_suggestion_within_bounds <- function() {
  set.seed(1)
  data <- data.frame(x = c(0.5, 2.5, 4.5))
  data$y <- sin(data$x)
  res <- bo_suggest(
    data,
    predictors = list(x = as.character(0:5)),
    predictor_types = c(x = "continuous"),
    objectives = c(y = "maximize")
  )
  expect_equal(nrow(res), 1L)
  expect_equal(names(res), "x")
  expect_true(res$x >= 0 && res$x <= 5)
}
test_continuous_suggestion_within_bounds()

test_mixed_suggestion <- function() {
  set.seed(1)
  data <- data.frame(
    temp = c(20, 40, 60, 30, 50),
    solvent = c("A", "B", "C", "A", "B")
  )
  data$yield <- data$temp / 10 + (data$solvent == "B")
  res <- bo_suggest(
    data,
    predictors = list(temp = c("20", "40", "60"), solvent = c("A", "B", "C")),
    predictor_types = c(temp = "continuous", solvent = "categorical"),
    objectives = c(yield = "maximize")
  )
  expect_equal(nrow(res), 1L)
  expect_true(res$temp >= 20 && res$temp <= 60)
  expect_true(res$solvent %in% c("A", "B", "C"))
}
test_mixed_suggestion()

test_unmeasured_rows_dropped <- function() {
  data <- data.frame(x = c(0.5, 2.5, 4.5, 3), y = c(sin(c(0.5, 2.5, 4.5)), NA))
  res <- bo_suggest(
    data,
    predictors = list(x = as.character(0:5)),
    predictor_types = c(x = "continuous"),
    objectives = c(y = "maximize")
  )
  expect_equal(nrow(res), 1L)
}
test_unmeasured_rows_dropped()

test_pending_rows_are_avoided <- function() {
  data <- data.frame(x = c(0.5, 2.5, 4.5, 1.5))
  data$y <- sin(data$x)
  args <- list(predictors = list(x = as.character(0:5)), predictor_types = c(x = "continuous"), objectives = c(y = "maximize"))
  set.seed(1)
  first <- do.call(bo_suggest, c(list(data), args))$x
  set.seed(1)
  again <- do.call(bo_suggest, c(list(rbind(data, data.frame(x = first, y = NA))), args))$x
  expect_true(abs(again - first) > 0.05)
}
test_pending_rows_are_avoided()

test_non_syntactic_names <- function() {
  set.seed(1)
  data <- data.frame(
    `Base loading` = c(1, 2, 3, 4, 5), `Catalyst` = c("Pd(OAc)2", "B", "Pd(OAc)2", "B", "Pd(OAc)2"),
    check.names = FALSE
  )
  data$`P-1 yield` <- data$`Base loading` * 10
  res <- bo_suggest(
    data,
    predictors = list(`Base loading` = c("1", "5"), Catalyst = c("Pd(OAc)2", "B")),
    predictor_types = c(`Base loading` = "continuous", Catalyst = "categorical"),
    objectives = c(`P-1 yield` = "maximize")
  )
  expect_equal(names(res), c("Base loading", "Catalyst"))
  expect_true(res$Catalyst %in% c("Pd(OAc)2", "B"))
}
test_non_syntactic_names()

test_bo_predictors_fixed_bounds <- function() {
  data <- data.frame(temp = c(60, 70, 80), solvent = c("A", "A", "B"))
  conv <- OpenDOE:::bo_predictors(
    data, c(temp = "continuous", solvent = "categorical"),
    bounds = list(temp = c("40", "90"), other = c("1", "2"))
  )
  expect_equal(names(conv$predictors), c("temp", "solvent"))
  expect_equal(conv$predictors$temp, c("40", "90"))
  expect_equal(conv$predictors$solvent, c("A", "B"))
  expect_equal(OpenDOE:::bo_predictors(data, c(temp = "continuous"))$predictors$temp, c("60", "80"))
}
test_bo_predictors_fixed_bounds()

test_batch_suggestions_are_distinct <- function() {
  set.seed(1)
  data <- data.frame(x = c(0.5, 2.5, 4.5, 1.5))
  data$y <- sin(data$x)
  res <- bo_suggest(
    data,
    predictors = list(x = c("0", "5")),
    predictor_types = c(x = "continuous"),
    objectives = c(y = "maximize"),
    n_points = 3L
  )
  expect_equal(nrow(res), 3L)
  expect_true(all(res$x >= 0 & res$x <= 5))
  expect_true(min(diff(sort(res$x))) > 0.05)
}
test_batch_suggestions_are_distinct()

test_batch_mixed_suggestions <- function() {
  data <- data.frame(
    temp = c(20, 40, 60, 30, 50, 45),
    solvent = c("A", "B", "C", "A", "B", "C")
  )
  data$yield <- data$temp / 10 + (data$solvent == "B")
  res <- bo_suggest(
    data,
    predictors = list(temp = c("20", "60"), solvent = c("A", "B", "C")),
    predictor_types = c(temp = "continuous", solvent = "categorical"),
    objectives = c(yield = "maximize"),
    n_points = 2L
  )
  expect_equal(nrow(res), 2L)
  expect_true(all(res$solvent %in% c("A", "B", "C")))
  expect_true(all(res$temp >= 20 & res$temp <= 60))
}
test_batch_mixed_suggestions()

test_exhausted_categorical_space_suggests_replicate <- function() {
  data <- data.frame(
    solvent = c("A", "B", "C", "A", "B", "C", "A", "B"),
    yield = c(0.42, 0.71, 0.53, 0.39, 0.68, 0.55, 0.44, 0.73)
  )
  res <- bo_suggest(
    data,
    predictors = list(solvent = c("A", "B", "C")),
    predictor_types = c(solvent = "categorical"),
    objectives = c(yield = "maximize")
  )
  expect_equal(nrow(res), 1L)
  expect_true(res$solvent %in% c("A", "B", "C"))
}
test_exhausted_categorical_space_suggests_replicate()

test_pareto_optimal <- function() {
  # example from the explanation: exp 3 is beaten by 2, exp 5 by 1
  data <- data.frame(
    yield = c(0.90, 0.70, 0.60, 0.40, 0.85, NA),
    cost = c(80, 30, 50, 10, 90, NA)
  )
  res <- OpenDOE:::pareto_optimal(data, c(yield = "maximize", cost = "minimize"))
  expect_equal(res, c(TRUE, TRUE, FALSE, TRUE, FALSE, NA))
}
test_pareto_optimal()

mock_two_responses <- function() {
  data.frame(
    temp = c(20, 30, 40, 50, 60, 70, 80, 90),
    yield = c(0.21, 0.38, 0.55, 0.68, 0.74, 0.70, 0.58, 0.41),
    cost = c(12, 15, 19, 24, 30, 37, 45, 54),
    purity = c(0.95, 0.93, 0.90, 0.88, 0.85, 0.80, 0.74, 0.70)
  )
}

test_two_objectives_ehvi <- function() {
  res <- bo_suggest(
    mock_two_responses(),
    predictors = list(temp = c("20", "90")),
    predictor_types = c(temp = "continuous"),
    objectives = c(yield = "maximize", cost = "minimize"),
    n_points = 2L
  )
  expect_equal(nrow(res), 2L)
  expect_true(all(res$temp >= 20 & res$temp <= 90))
}
test_two_objectives_ehvi()

test_three_objectives_smsego <- function() {
  res <- bo_suggest(
    mock_two_responses(),
    predictors = list(temp = c("20", "90")),
    predictor_types = c(temp = "continuous"),
    objectives = c(yield = "maximize", cost = "minimize", purity = "maximize")
  )
  expect_equal(nrow(res), 1L)
  expect_true(res$temp >= 20 && res$temp <= 90)
}
test_three_objectives_smsego()

test_two_objectives_mixed <- function() {
  data <- data.frame(
    temp = c(20, 35, 50, 65, 80, 95, 45, 70),
    solvent = c("A", "B", "C", "A", "B", "C", "B", "C"),
    yield = c(0.20, 0.52, 0.49, 0.61, 0.83, 0.38, 0.66, 0.57),
    cost = c(14, 26, 22, 33, 47, 45, 30, 36)
  )
  res <- bo_suggest(
    data,
    predictors = list(temp = c("20", "95"), solvent = c("A", "B", "C")),
    predictor_types = c(temp = "continuous", solvent = "categorical"),
    objectives = c(yield = "maximize", cost = "minimize")
  )
  expect_equal(nrow(res), 1L)
  expect_true(res$solvent %in% c("A", "B", "C"))
}
test_two_objectives_mixed()

test_run_bo_suggest_adds_pareto_column <- function() {
  data <- mock_two_responses()[c("temp", "yield", "cost")]
  data$pareto_optimal <- TRUE # stale column from a previous round
  res <- OpenDOE:::run_bo_suggest(
    data, c(temp = "continuous"), c("yield", "cost"), c("maximize", "minimize"), seed = 1, n_points = 2L
  )
  expect_equal(nrow(res), 10L)
  expect_true(all(is.na(res$pareto_optimal[9:10])))
  expect_equal(
    res$pareto_optimal[1:8],
    OpenDOE:::pareto_optimal(data[1:8, ], c(yield = "maximize", cost = "minimize"))
  )
  expect_true(any(!res$pareto_optimal[1:8]))
}
test_run_bo_suggest_adds_pareto_column()

test_invalid_input <- function() {
  predictors <- list(x = as.character(0:5))
  types <- c(x = "continuous")
  expect_error(
    bo_suggest(data.frame(x = 1:3), predictors, types, c(y = "maximize")),
    "Missing columns"
  )
  expect_error(
    bo_suggest(data.frame(x = 1:2, y = 1:2), predictors, types, c(y = "maximize")),
    "At least 3"
  )
  # mock_data2.csv case: conc + bla (2 levels) = 3 dimensions -> 4 experiments needed
  expect_error(
    bo_suggest(
      data.frame(y = c(0.1, 3.4, 5), conc = c(1, 100, 50), bla = c("A", "A", "B")),
      list(conc = c("1", "100"), bla = c("A", "B")),
      c(conc = "continuous", bla = "categorical"), c(y = "maximize")
    ),
    "At least 4"
  )
  expect_equal(OpenDOE:::bo_min_experiments(list(x = c("0", "1")), c(x = "continuous")), 3L)
  expect_equal(
    OpenDOE:::bo_min_experiments(
      list(x = c("0", "1"), z = c("0", "1"), s = c("A", "B", "C")),
      c(x = "continuous", z = "continuous", s = "categorical")
    ),
    6L
  )
  expect_error(
    bo_suggest(data.frame(x = 1:3, y = 1:3), predictors, types, character(0)),
    "at least one measured result"
  )
  expect_error(
    bo_suggest(
      data.frame(s = c("A", "B", "C", "D"), y = 1:4),
      list(s = c("A", "B", "C")), c(s = "categorical"), c(y = "maximize")
    ),
    "Unknown levels"
  )
}
test_invalid_input()

# Regression test from development/benchmark_bo.R: on Branin (budget 20) BO had a median
# regret of ~0.19 vs ~1.7 for pure LHS. Threshold sits in between, so a broken BO fails.
branin <- function(x1, x2) {
  (x2 - 5.1 / (4 * pi^2) * x1^2 + 5 / pi * x1 - 6)^2 + 10 * (1 - 1 / (8 * pi)) * cos(x1) + 10
}

bo_regret_branin <- function(seed, mixed) {
  predictors <- list(x1 = c("-5", "10"), x2 = c("0", "15"))
  types <- c(x1 = "continuous", x2 = "continuous")
  offset <- c(A = 0, B = 5, C = 10)
  if (mixed) {
    predictors$cat <- c("A", "B", "C")
    types <- c(types, cat = "categorical")
  }
  f <- function(df) branin(df$x1, df$x2) + if (mixed) unname(offset[df$cat]) else 0
  budget <- if (mixed) 24L else 20L
  set.seed(seed)
  noise <- stats::rnorm(budget)
  df <- OpenDOE:::lhs_design(predictors, types, 4L * length(predictors), seed)
  df$y <- f(df) + noise[seq_len(nrow(df))]
  while (nrow(df) < budget) {
    df <- OpenDOE:::run_bo_suggest(df, types, "y", "minimize", seed * 1000L + nrow(df))
    i <- nrow(df)
    df$y[i] <- f(df[i, , drop = FALSE]) + noise[i]
  }
  best <- df[which.min(df$y), , drop = FALSE]
  f(best) - 0.397887
}

# Median over seeds, since a single unlucky seed can miss the optimum even when BO works
seeds <- 1:3

test_bo_beats_lhs_on_branin <- function() {
  regrets <- vapply(seeds, bo_regret_branin, numeric(1), mixed = FALSE)
  expect_true(stats::median(regrets) < 1)
}
test_bo_beats_lhs_on_branin()

test_bo_beats_lhs_on_mixed_branin <- function() {
  regrets <- vapply(seeds, bo_regret_branin, numeric(1), mixed = TRUE)
  expect_true(stats::median(regrets) < 2)
}
test_bo_beats_lhs_on_mixed_branin()
