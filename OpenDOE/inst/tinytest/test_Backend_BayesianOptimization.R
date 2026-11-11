for (p in c("mlr3mbo", "mlr3learners", "DiceKriging", "rgenoud", "ranger")) {
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
