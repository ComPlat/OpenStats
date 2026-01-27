calc_n_block_one_way_anova <- function(n_levels, cohens_f, sig_level, desired_power) {
  res_anova <- pwr::pwr.anova.test(
    k = n_levels, f = cohens_f, sig.level = sig_level, power = desired_power, n = NULL
  )
  ceiling(res_anova$n)
}

randomization_one_way_anova <- function(predictors, primary_factor,
                                        groups, group_type, ratios,
                                        cohens_f, sig_level, desired_power,
                                        randomization_method, n_quantiles,
                                        strata_cols = NULL, seed) {
  stopifnot(
    is.list(predictors), length(predictors) >= 1L,
    group_type %in% c("finite", "infinite"),
    length(names(predictors)) == length(predictors),
    is.character(primary_factor), length(primary_factor) == 1L,
    primary_factor %in% names(predictors),
    length(groups) >= 1L,
    is.numeric(cohens_f), length(cohens_f) == 1L,
    is.numeric(sig_level), length(sig_level) == 1L,
    sig_level > .Machine$double.xmin && sig_level <= 1.0,
    is.numeric(desired_power), length(desired_power) == 1L,
    randomization_method %in% c("simple", "block", "block_stratified"),
    is.numeric(seed), length(seed) == 1L
  )
  if (group_type == "infinite") {
    stopifnot(
      nrow(df) >= length(groups),
      is.character(groups),
      is.numeric(ratios),
      length(ratios) == length(groups)
    )
  } else {
    stopifnot(
      is.numeric(groups), missing(ratios) || is.null(ratios),
      nrow(df) <= length(groups),
      is.numeric(n_quantiles)
    )
    ratios <- NULL
  }

  n_levels <- length(predictors[[primary_factor]])
  n_per_level <- calc_n_block_one_way_anova(n_levels, cohens_f, sig_level, desired_power)
  other_factors <- predictors[setdiff(names(predictors), primary_factor)]
  if (length(other_factors) == 0L) {
    combos_per_level <- 1L
  } else {
    combos_per_level <- prod(vapply(other_factors, length, integer(1)))
  }
  n_per_combo <- ceiling(n_per_level / combos_per_level)
  design <- completely_randomised_design(predictors, n_per_combo)

  if (randomization_method == "simple") {
    random_assign(
      df = design, groups = groups, group_type = group_type,
      ratios = ratios, col = "random_group", block_col = NULL, strata_cols = NULL,
      randomization_method = "simple", n_quantiles = n_quantiles, seed = seed
    )
  } else if (randomization_method == "block") {
    random_assign(
      df = design, groups = groups, group_type = group_type,
      ratios = ratios, col = "random_group", block_col = primary_factor, strata_cols = NULL,
      randomization_method = "block", n_quantiles = n_quantiles, seed = seed
    )
  } else if (randomization_method == "block_stratified") {
    stopifnot(!is.null(strata_cols))
    stopifnot(all(strata_cols %in% names(design)))
    random_assign(
      df = design, groups = groups, group_type = group_type,
      ratios = ratios, col = "random_group", block_col = primary_factor, strata_cols = strata_cols,
      randomization_method = "block_stratified", n_quantiles = n_quantiles, seed = seed
    )
  }
}
