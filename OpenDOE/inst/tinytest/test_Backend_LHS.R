library(tinytest)

test_lhs_unit_is_stratified <- function() {
  set.seed(1)
  u <- OpenDOE:::lhs_unit(10L, 3L)
  expect_equal(dim(u), c(10L, 3L))
  for (j in 1:3) expect_equal(sort(floor(u[, j] * 10)), 0:9)
}
test_lhs_unit_is_stratified()

test_maximin_not_worse_than_single_draw <- function() {
  set.seed(1)
  single <- OpenDOE:::min_distance(OpenDOE:::lhs_unit(12L, 2L))
  set.seed(1)
  best <- OpenDOE:::min_distance(OpenDOE:::maximin_lhs(12L, 2L, n_candidates = 50L))
  expect_true(best >= single)
}
test_maximin_not_worse_than_single_draw()

test_lhs_design_mixed_types <- function() {
  predictors <- list(
    temp = c("20", "60"),
    pressure = c("1", "2", "3"),
    ph = c("5", "7"),
    solvent = c("A", "B", "C", "D")
  )
  types <- c(temp = "continuous", pressure = "continuous_grid", ph = "discrete", solvent = "categorical")
  df <- OpenDOE:::lhs_design(predictors, types, n = 12L, seed = 42)
  expect_equal(nrow(df), 12L)
  expect_equal(names(df), names(predictors))
  expect_true(all(df$temp >= 20 & df$temp <= 60))
  expect_equal(range(df$temp), c(20, 60))
  expect_equal(df$temp, signif(df$temp, 3))
  expect_true(all(df$pressure %in% c(1, 2, 3)))
  expect_true(all(df$ph %in% c(5, 7)))
  expect_equal(as.vector(table(df$solvent)), c(3L, 3L, 3L, 3L))
  expect_identical(df, OpenDOE:::lhs_design(predictors, types, n = 12L, seed = 42))
}
test_lhs_design_mixed_types()

test_round_lab_value_stays_in_bounds <- function() {
  expect_equal(OpenDOE:::round_lab_value(9.9958, 1, 9.996), 9.996)
  expect_equal(OpenDOE:::round_lab_value(4.203242868, 1, 10), 4.2)
}
test_round_lab_value_stays_in_bounds()
