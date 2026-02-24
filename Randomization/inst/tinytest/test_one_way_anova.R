library(tinytest)

anova_error_trigger <- function() {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
  )
  checks <- logical(3)

  # 1: predictors not list
  checks[[1]] <- expect_error(
    calc_n_one_way_anova(
      predictors = 1:3,
      primary_factor = "Treatment",
      cohens_f = 0.25,
      sig_level = 0.05,
      desired_power = 0.8
    )
  )
  # 2: primary factor invalid
  checks[[2]] <- expect_error(
    calc_n_one_way_anova(
      predictors = predictors,
      primary_factor = "invalid",
      cohens_f = 0.25,
      sig_level = 0.05,
      desired_power = 0.8
    )
  )
  # 3: invalid sig_level
  checks[[3]] <- expect_error(
    calc_n_one_way_anova(
      predictors = predictors,
      primary_factor = "Treatment",
      ratios = c(1,1,1),
      cohens_f = 0.25,
      sig_level = "Invalid",
      desired_power = 0.8
    )
  )
  expect_true(all(checks))
}
anova_error_trigger()

anova_test <- function() {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
  )
  res <- Randomization::calc_n_one_way_anova(
    predictors = predictors,
    primary_factor = "Treatment",
    cohens_f = 0.25,
    sig_level = 0.05,
    desired_power = 0.8
  )
  checks <- logical(2)
  checks[[1]] <- expect_true(is.data.frame(res))
  checks[[2]] <- expect_equal(names(res), c("cellLines", "Treatment", "id"))
  expect_true(all(checks))
}
anova_test()

