library(tinytest)

determine_sample_size_kruskal_rejects_multi_predictor <- function() {
  # Kruskal-Wallis is one-way only -- passing a named list (several
  # predictors, like determine_sample_size()'s `levels`) must be rejected
  # rather than silently doing something wrong.
  expect_error(
    Randomization::determine_sample_size_one_way(
      level = list(a = c("x", "y"), b = c("1", "2")), mu = c(0, 1, 2, 3),
      test_fn = Randomization::kruskal_test,
      alpha = 0.05, power_target = 0.8, seed = 42, nsim = 10, n_min = 3, n_max = 10
    )
  )
}
determine_sample_size_kruskal_rejects_multi_predictor()

determine_sample_size_kruskal_test <- function() {
  res <- Randomization::determine_sample_size_one_way(
    level = c("low", "mid", "high"), mu = c(0, 1, 3), sd = c(1, 1, 1),
    test_fn = Randomization::kruskal_test,
    alpha = 0.05, power_target = 0.8, seed = 42, nsim = 200, n_min = 3, n_max = 60
  )
  checks <- logical(5)
  checks[[1]] <- expect_true(is.list(res))
  checks[[2]] <- expect_true(is.numeric(res$n) && !is.na(res$n))
  checks[[3]] <- expect_true(res$power >= 0 && res$power <= 1)
  checks[[4]] <- expect_true(is.data.frame(res$data))
  checks[[5]] <- expect_equal(names(res$data), c("group", "values"))
  expect_true(all(checks))
}
determine_sample_size_kruskal_test()

determine_sample_size_kruskal_two_levels_test <- function() {
  # Works fine with exactly 2 levels too (just less powerful than the
  # matching Wilcoxon test would be for the same data) -- no special-casing
  # needed, kruskal.test() handles k=2 the same as k>2.
  res <- Randomization::determine_sample_size_one_way(
    level = c("A", "B"), mu = c(0, 2), sd = c(1, 1),
    test_fn = Randomization::kruskal_test,
    alpha = 0.05, power_target = 0.8, seed = 42, nsim = 200, n_min = 3, n_max = 60
  )
  checks <- logical(2)
  checks[[1]] <- expect_true(is.numeric(res$n) && !is.na(res$n))
  checks[[2]] <- expect_true(res$power >= 0 && res$power <= 1)
  expect_true(all(checks))
}
determine_sample_size_kruskal_two_levels_test()
