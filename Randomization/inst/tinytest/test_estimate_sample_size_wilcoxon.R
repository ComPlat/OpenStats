library(tinytest)

estimate_sample_size_wilcoxon_error_trigger <- function() {
  means <- c(control = 10, one = 6, two = 6)
  sds <- c(control = 2, one = 1.2, two = 1.2)
  checks <- logical(4)

  # 1: invalid mcc
  checks[[1]] <- expect_error(
    estimate_sample_size(means, sds, test_fn = pairwise_wilcoxon_test, mcc = "invalid", family = "all")
  )
  # 2: invalid family
  checks[[2]] <- expect_error(
    estimate_sample_size(means, sds, test_fn = pairwise_wilcoxon_test, mcc = "bonferroni", family = "invalid")
  )
  # 3: means not numeric
  checks[[3]] <- expect_error(
    estimate_sample_size(means = c("a", "b"), sds, test_fn = pairwise_wilcoxon_test, mcc = "bonferroni", family = "all")
  )
  # 4: means/sds length mismatch
  checks[[4]] <- expect_error(
    estimate_sample_size(means, sds = c(2, 1.2), test_fn = pairwise_wilcoxon_test, mcc = "bonferroni", family = "all")
  )
  expect_true(all(checks))
}
estimate_sample_size_wilcoxon_error_trigger()

estimate_sample_size_wilcoxon_two_groups_test <- function() {
  # Reduces to a single pairwise comparison -- mcc/family become no-ops with
  # only one pair, this is the 1-predictor/2-level non-parametric case.
  means <- c(A = 0, B = 2)
  sds <- c(A = 1, B = 1)
  res <- Randomization::estimate_sample_size(
    means, sds, test_fn = Randomization::pairwise_wilcoxon_test, mcc = "none", family = "any",
    nsim = 200, n_min = 3, n_max = 60, seed = 42
  )
  checks <- logical(3)
  checks[[1]] <- expect_true(is.list(res))
  checks[[2]] <- expect_true(is.numeric(res$n) && !is.na(res$n))
  checks[[3]] <- expect_true(res$power >= 0 && res$power <= 1)
  expect_true(all(checks))
}
estimate_sample_size_wilcoxon_two_groups_test()

estimate_sample_size_wilcoxon_multi_group_test <- function() {
  # Multi-predictor pairwise case: group labels could come from crossing
  # several predictors, this function doesn't care -- it only sees group
  # means/sds either way.
  means <- c(control = 10, one = 6, two = 6)
  sds <- c(control = 2, one = 1.2, two = 1.2)
  res <- Randomization::estimate_sample_size(
    means, sds, test_fn = Randomization::pairwise_wilcoxon_test, mcc = "bonferroni", family = "all",
    nsim = 200, n_min = 2, n_max = 60, seed = 42
  )
  checks <- logical(3)
  checks[[1]] <- expect_true(is.list(res))
  checks[[2]] <- expect_true(is.numeric(res$n) && !is.na(res$n))
  checks[[3]] <- expect_true(res$power >= 0 && res$power <= 1)
  expect_true(all(checks))
}
estimate_sample_size_wilcoxon_multi_group_test()
