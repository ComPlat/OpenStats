calc_n_block_one_way_anova <- function(n_levels, cohens_f, sig_level, desired_power) {
  res_anova <- pwr::pwr.anova.test(
    k = n_levels, f = cohens_f, sig.level = sig_level, power = desired_power, n = NULL
  )
  ceiling(res_anova$n)
}

calc_n_one_way_anova <- function(predictors, primary_factor, cohens_f, sig_level, desired_power) {
  stopifnot(
    is.list(predictors), length(predictors) >= 1L,
    length(names(predictors)) == length(predictors),
    is.character(primary_factor), length(primary_factor) == 1L,
    primary_factor %in% names(predictors),

    is.numeric(cohens_f), length(cohens_f) == 1L,
    is.numeric(sig_level), length(sig_level) == 1L,
    sig_level > .Machine$double.xmin && sig_level <= 1.0,
    is.numeric(desired_power), length(desired_power) == 1L
  )

  # ---- build design for the ANOVA ----
  n_levels <- length(predictors[[primary_factor]])
  n_per_level <- calc_n_block_one_way_anova(n_levels, cohens_f, sig_level, desired_power)

  other_factors <- predictors[setdiff(names(predictors), primary_factor)]
  combos_per_level <- if (length(other_factors) == 0L) 1L else prod(vapply(other_factors, length, integer(1)))
  n_per_combo <- ceiling(n_per_level / combos_per_level)

  completely_randomised_design(predictors, n_per_combo)
}
