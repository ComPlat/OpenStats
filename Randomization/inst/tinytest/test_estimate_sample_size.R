library(tinytest)

estimate_sample_size_error_trigger <- function() {
  means <- c(control = 10, one = 6, two = 6)
  sds <- c(control = 2, one = 1.2, two = 1.2)
  checks <- logical(4)

  # 1: invalid mcc
  checks[[1]] <- expect_error(
    estimate_sample_size(means, sds, mcc = "invalid", family = "all")
  )
  # 2: invalid family
  checks[[2]] <- expect_error(
    estimate_sample_size(means, sds, mcc = "bonferroni", family = "invalid")
  )
  # 3: means not numeric
  checks[[3]] <- expect_error(
    estimate_sample_size(means = c("a", "b"), sds, mcc = "bonferroni", family = "all")
  )
  # 4: means/sds length mismatch
  checks[[4]] <- expect_error(
    estimate_sample_size(means, sds = c(2, 1.2), mcc = "bonferroni", family = "all")
  )
  expect_true(all(checks))
}
estimate_sample_size_error_trigger()

estimate_sample_size_test <- function() {
  means <- c(control = 10, one = 6, two = 6)
  sds <- c(control = 2, one = 1.2, two = 1.2)
  res <- Randomization::estimate_sample_size(
    means, sds, mcc = "bonferroni", family = "all",
    nsim = 200, n_min = 2, n_max = 60, seed = 42
  )
  checks <- logical(3)
  checks[[1]] <- expect_true(is.list(res))
  checks[[2]] <- expect_equal(res$n, 6)
  checks[[3]] <- expect_true(res$power >= 0 && res$power <= 1)
  expect_true(all(checks))
}
estimate_sample_size_test()
