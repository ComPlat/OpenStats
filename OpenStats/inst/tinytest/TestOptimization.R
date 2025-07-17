library(tinytest)
set.seed(1234)

df <- data.frame(
  x = seq(1, 10, by = 1),
  y = 2 * seq(1, 10, by = 1) + 5 + rnorm(10, sd = 0.1)
)

test_create_formula <- function() {
  f <- y ~ a * x + b
  lower <- c(0, 0)
  upper <- c(10, 10)
  seed <- 42

  formula_obj <- OpenStats:::create_formula_optim(f, df, lower, upper, seed)
  checks <- c()

  checks[[1]] <- expect_inherits(formula_obj, "OptimFormula")
  checks[[2]] <- expect_equal(formula_obj@parameter, c("a", "b"))
  checks[[3]] <- expect_equal(formula_obj@lhs, "y")
  checks[[4]] <- expect_equal(formula_obj@lower, lower)
  checks[[5]] <- expect_equal(formula_obj@upper, upper)
  expect_true(all(unlist(checks)))
}
test_create_formula()

test_loss_fn <- function() {
  f <- y ~ a * x + b
  formula_obj <- OpenStats:::create_formula_optim(f, df, c(0, 0), c(10, 10), 42)
  loss_fn <- OpenStats:::make_loss_fn_optim(formula_obj, df)
  params <- c(a = 2, b = 5)
  checks <- c()
  loss <- loss_fn(params)
  checks[[1]] <- expect_true(is.numeric(loss))
  checks[[2]] <- expect_true(loss >= 0)
  preds <- loss_fn(params, error_calc = FALSE)
  checks[[3]] <- expect_equal(length(preds), nrow(df))
  expect_true(all(unlist(checks)))
}
test_loss_fn()

test_optimize <- function() {
  f <- y ~ a * x + b
  formula_obj <- OpenStats:::create_formula_optim(f, df, c(0, 0), c(10, 10), 42)
  result <- OpenStats:::optimize(formula_obj, df)
  checks <- c()
  checks[[1]] <- expect_inherits(result, "optimResult")
  checks[[2]] <- expect_true(result@convergence)
  checks[[3]] <- expect_true(is.numeric(result@parameter))
  checks[[4]] <- expect_equal(length(result@parameter), 2)
  expect_true(all(unlist(checks)))
}
test_optimize()

test_summary <- function() {
  f <- y ~ a * x + b
  formula_obj <- OpenStats:::create_formula_optim(f, df, c(0, 0), c(10, 10), 42)
  result <- OpenStats:::optimize(formula_obj, df)
  summary <- OpenStats:::summary_model_optim(formula_obj, result)
  checks <- c()
  checks[[1]] <- expect_inherits(summary, "data.frame")
  checks[[2]] <- expect_equal(names(summary), c("a", "b"))
  checks[[3]] <- expect_equal(nrow(summary), 1)
  expect_true(all(unlist(checks)))
}
test_summary()

test_ic <- function() {
  f <- y ~ a * x + b
  formula_obj <- OpenStats:::create_formula_optim(f, df, c(0, 0), c(10, 10), 42)
  result <- OpenStats:::optimize(formula_obj, df)
  ic <- OpenStats:::information_criterion_optim(result)
  checks <- c()
  checks[[1]] <- expect_inherits(ic, "data.frame")
  checks[[2]] <- expect_equal(names(ic), "Sum of Squared Errors (SSE)")
  checks[[3]] <- expect_true(ic[1, 1] >= 0)
  expect_true(all(unlist(checks)))
}
test_ic()

test_plot <- function() {
  f <- y ~ a * x + b
  formula_obj <- OpenStats:::create_formula_optim(f, df, c(0, 0), c(10, 10), 42)
  result <- OpenStats:::optimize(formula_obj, df)
  p <- OpenStats:::plot_model_optim(formula_obj, result)
  checks <- c()
  checks[[1]] <- expect_inherits(p, "plot")
  checks[[2]] <- expect_true(!is.null(p@p))
  expect_true(all(unlist(checks)))
}
test_plot()

test_assumptions <- function() {
  f <- y ~ a * x + b
  formula_obj <- OpenStats:::create_formula_optim(f, df, c(0, 0), c(10, 10), 42)
  result <- OpenStats:::optimize(formula_obj, df)
  p <- OpenStats:::assumptions_optim(result)
  checks <- c()
  checks[[1]] <- expect_inherits(p, "plot")
  checks[[2]] <- expect_true(!is.null(p@p))
  expect_true(all(unlist(checks)))
}
test_assumptions()
