library(tinytest)

determine_sample_size_welch_anova_rejects_multi_predictor <- function() {
  # Same one-way-only restriction as Kruskal-Wallis -- oneway.test() is
  # strictly one-way, no multi-predictor Welch-ANOVA equivalent exists.
  expect_error(
    Randomization::determine_sample_size_one_way(
      level = list(a = c("x", "y"), b = c("1", "2")), mu = c(0, 1, 2, 3),
      test_fn = Randomization::welch_anova_test,
      alpha = 0.05, power_target = 0.8, seed = 42, nsim = 10, n_min = 3, n_max = 10
    )
  )
}
determine_sample_size_welch_anova_rejects_multi_predictor()

determine_sample_size_welch_anova_test <- function() {
  # Previously-missing "Welch-ANOVA" leaf of test_selection_tree.md -- same
  # engine as Kruskal-Wallis, just a different injected test_fn. Unequal
  # sds so this genuinely exercises the var.equal = FALSE path (with equal
  # sds it would numerically match the Student ANOVA case).
  res <- Randomization::determine_sample_size_one_way(
    level = c("low", "mid", "high"), mu = c(0, 1, 3), sd = c(1, 2, 3),
    test_fn = Randomization::welch_anova_test,
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
determine_sample_size_welch_anova_test()

determine_sample_size_welch_anova_two_levels_test <- function() {
  # k = 2 reduces to a Welch t-test under the hood (oneway.test with
  # var.equal = FALSE on two groups) -- should work the same as k > 2.
  res <- Randomization::determine_sample_size_one_way(
    level = c("A", "B"), mu = c(0, 2), sd = c(1, 1.5),
    test_fn = Randomization::welch_anova_test,
    alpha = 0.05, power_target = 0.8, seed = 42, nsim = 200, n_min = 3, n_max = 60
  )
  checks <- logical(2)
  checks[[1]] <- expect_true(is.numeric(res$n) && !is.na(res$n))
  checks[[2]] <- expect_true(res$power >= 0 && res$power <= 1)
  expect_true(all(checks))
}
determine_sample_size_welch_anova_two_levels_test()
