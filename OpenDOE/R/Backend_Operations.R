# Plain top-level wrappers around the Randomization:: calls used by the
# Sample size tab. Shared by the live handlers (Server_SampleSize.R) and by
# history replay (Server_History.R) so both run the exact same code path.
# Kept as standalone functions (not closures) so they are safe to pass
# straight into bg_process$start() without any environment stripping.

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
