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

test_invalid_input <- function() {
  predictors <- list(x = as.character(0:5))
  types <- c(x = "continuous")
  expect_error(
    bo_suggest(data.frame(x = 1:3), predictors, types, c(y = "maximize")),
    "Missing columns"
  )
  expect_error(
    bo_suggest(data.frame(x = 1:2, y = 1:2), predictors, types, c(y = "maximize")),
    "At least three"
  )
  expect_error(
    bo_suggest(data.frame(x = 1:3, y = 1:3, z = 1:3), predictors, types, c(y = "maximize", z = "minimize")),
    "single objective"
  )
  expect_error(
    bo_suggest(
      data.frame(s = c("A", "B", "D"), y = 1:3),
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
