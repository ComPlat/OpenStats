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
    means = means, sds = sds, test_fn = Randomization::pairwise_welch_t_test,
    power_target = power_target, alpha = alpha,
    mcc = mcc, family = family, nsim = nsim, n_min = n_min, n_max = n_max, seed = seed
  )
}

run_estimate_sample_size_wilcoxon <- function(means, sds, power_target, alpha, mcc, family, nsim, n_min, n_max, seed) {
  Randomization::estimate_sample_size(
    means = means, sds = sds, test_fn = Randomization::pairwise_wilcoxon_test,
    power_target = power_target, alpha = alpha,
    mcc = mcc, family = family, nsim = nsim, n_min = n_min, n_max = n_max, seed = seed
  )
}

run_estimate_sample_size_student_t <- function(means, sds, power_target, alpha, mcc, family, nsim, n_min, n_max, seed) {
  Randomization::estimate_sample_size(
    means = means, sds = sds, test_fn = Randomization::pairwise_t_test,
    power_target = power_target, alpha = alpha,
    mcc = mcc, family = family, nsim = nsim, n_min = n_min, n_max = n_max, seed = seed
  )
}

run_determine_sample_size <- function(levels, mu, cv, sd, formula, alphas,
                                       target_effect, power_target, seed, nsim, n_min, n_max) {
  Randomization::determine_sample_size(
    levels = levels, mu = mu, cv = cv, sd = sd,
    formula = formula, alphas = alphas, target_effect = target_effect,
    power_target = power_target, seed = seed,
    nsim = nsim, n_min = n_min, n_max = n_max
  )
}

run_determine_sample_size_kruskal <- function(level, mu, cv, sd, alpha, power_target, seed, nsim, n_min, n_max) {
  Randomization::determine_sample_size_one_way(
    level = level, mu = mu, test_fn = Randomization::kruskal_test, cv = cv, sd = sd, alpha = alpha,
    power_target = power_target, seed = seed,
    nsim = nsim, n_min = n_min, n_max = n_max
  )
}

run_determine_sample_size_welch_anova <- function(level, mu, cv, sd, alpha, power_target, seed, nsim, n_min, n_max) {
  Randomization::determine_sample_size_one_way(
    level = level, mu = mu, test_fn = Randomization::welch_anova_test, cv = cv, sd = sd, alpha = alpha,
    power_target = power_target, seed = seed,
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

run_random_finite_assign <- function(seed, groups, design, ids, max_iter, loss_function) {
  res <- Randomization::random_finite_assign(
    seed = seed, groups = groups, design = design, ids = ids,
    max_iter = max_iter, loss_function = loss_function
  )
  assigned <- res$assigned
  # Randomization always adds `unit_index` (a meaningless row position);
  # OpenDOE requires a real `ids` column instead, which comes back as
  # `unit_id` -- drop the positional one so only the real identifier shows.
  assigned$unit_index <- NULL
  list(df = cbind(assigned, design), loss = res$loss)
}
