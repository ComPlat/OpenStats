run_two_sample_ttest <- function(predictors, primary_factor, cohens_d, sig_level, desired_power) {
  Randomization::calc_n_two_sample_ttest(
    predictors = predictors, primary_factor = primary_factor,
    cohens_d = cohens_d, sig_level = sig_level, desired_power = desired_power
  )
}

run_one_way_anova <- function(predictors, primary_factor, cohens_f, sig_level, desired_power) {
  Randomization::calc_n_one_way_anova(
    predictors = predictors, primary_factor = primary_factor,
    cohens_f = cohens_f, sig_level = sig_level, desired_power = desired_power
  )
}

run_estimate_sample_size <- function(means, sds, power_target, alpha, mcc, family, nsim, n_min, n_max, seed) {
  Randomization::estimate_sample_size(
    means = means, sds = sds, power_target = power_target, alpha = alpha,
    mcc = mcc, family = family, nsim = nsim, n_min = n_min, n_max = n_max, seed = seed
  )
}

run_determine_sample_size <- function(levels, means, cv, sd, interactions, formula, alphas,
                                       power_target, seed, nsim, n_min, n_max) {
  Randomization::determine_sample_size(
    levels = levels, means = means, cv = cv, sd = sd, interactions = interactions,
    formula = formula, alphas = alphas, power_target = power_target, seed = seed,
    nsim = nsim, n_min = n_min, n_max = n_max
  )
}

run_random_assign <- function(df, groups, ratios, col, block_col, strata_cols,
                               randomization_method, seed) {
  Randomization::random_assign(
    df = df, groups = groups, ratios = ratios, col = col,
    block_col = block_col, strata_cols = strata_cols,
    randomization_method = randomization_method, seed = seed
  )
}

run_completely_randomised_design <- function(predictors, n_per_level) {
  Randomization::completely_randomised_design(predictors = predictors, n_per_level = n_per_level)
}

run_random_finite_assign <- function(seed, groups, design, max_iter, loss_function) {
  res <- Randomization::random_finite_assign(
    seed = seed, groups = groups, design = design,
    max_iter = max_iter, loss_function = loss_function
  )
  list(df = cbind(res$assigned, design), loss = res$loss)
}
