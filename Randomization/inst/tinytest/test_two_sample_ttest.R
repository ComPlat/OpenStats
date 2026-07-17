library(tinytest)

ttest_error_trigger <- function() {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = c("Control", "Drug")
  )
  checks <- logical(4)

  # 1: predictors not list
  checks[[1]] <- expect_error(
    calc_n_two_sample_ttest(
      predictors = 1:3,
      primary_factor = "Treatment",
      cohens_d = 0.8,
      sig_level = 0.05,
      desired_power = 0.8
    )
  )
  # 2: primary factor invalid
  checks[[2]] <- expect_error(
    calc_n_two_sample_ttest(
      predictors = predictors,
      primary_factor = "invalid",
      cohens_d = 0.8,
      sig_level = 0.05,
      desired_power = 0.8
    )
  )
  # 3: invalid sig_level
  checks[[3]] <- expect_error(
    calc_n_two_sample_ttest(
      predictors = predictors,
      primary_factor = "Treatment",
      cohens_d = 0.8,
      sig_level = "Invalid",
      desired_power = 0.8
    )
  )
  # 4: primary factor does not have exactly two levels
  checks[[4]] <- expect_error(
    calc_n_two_sample_ttest(
      predictors = list(cellLines = c("HeLa", "Hek"), Treatment = LETTERS[1:3]),
      primary_factor = "Treatment",
      cohens_d = 0.8,
      sig_level = 0.05,
      desired_power = 0.8
    )
  )
  expect_true(all(checks))
}
ttest_error_trigger()

ttest_test <- function() {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = c("Control", "Drug")
  )
  res <- Randomization::calc_n_two_sample_ttest(
    predictors = predictors,
    primary_factor = "Treatment",
    cohens_d = 0.8,
    sig_level = 0.05,
    desired_power = 0.8
  )
  checks <- logical(2)
  checks[[1]] <- expect_true(is.data.frame(res))
  checks[[2]] <- expect_equal(names(res), c("cellLines", "Treatment", "id"))
  expect_true(all(checks))
}
ttest_test()
