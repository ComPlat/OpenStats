calc_n_block_two_sample_ttest <- function(cohens_d, sig_level, desired_power) {
  res_ttest <- pwr::pwr.t.test(
    d = cohens_d, sig.level = sig_level, power = desired_power, n = NULL, type = "two.sample"
  )
  ceiling(res_ttest$n)
}

calc_n_two_sample_ttest <- function(predictors, primary_factor, cohens_d, sig_level, desired_power) {
  stopifnot(
    is.list(predictors), length(predictors) >= 1L,
    length(names(predictors)) == length(predictors),
    is.character(primary_factor), length(primary_factor) == 1L,
    primary_factor %in% names(predictors),
    length(predictors[[primary_factor]]) == 2L,

    is.numeric(cohens_d), length(cohens_d) == 1L,
    is.numeric(sig_level), length(sig_level) == 1L,
    sig_level > .Machine$double.xmin && sig_level <= 1.0,
    is.numeric(desired_power), length(desired_power) == 1L
  )

  # ---- build design for the t-test ----
  n_per_level <- calc_n_block_two_sample_ttest(cohens_d, sig_level, desired_power)

  other_factors <- predictors[setdiff(names(predictors), primary_factor)]
  combos_per_level <- if (length(other_factors) == 0L) 1L else prod(vapply(other_factors, length, integer(1)))
  n_per_combo <- ceiling(n_per_level / combos_per_level)

  completely_randomised_design(predictors, n_per_combo)
}
