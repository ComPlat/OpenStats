library(tinytest)

determine_sample_size_test <- function() {
  genotype <- c("WT", "mutant")
  treatment <- c("untreated", "treated")
  levels <- list(genotype = genotype, treatment = treatment)

  # Cell means, one per expand.grid(levels) row (genotype varies fastest):
  # (WT,untreated), (mutant,untreated), (WT,treated), (mutant,treated).
  # Large genotype main effect (40 vs ~100), flat treatment main effect,
  # and a real but modest genotype:treatment interaction (mutant drops
  # under treatment, WT doesn't) -- direct cell-mean input, no more
  # baseline + additive deviation + multiplicative interaction mask.
  mu <- c(40, 100, 40, 70)
  cv <- 0.25
  formula <- formula(values ~ genotype * treatment)
  alphas <- list(genotype = 0.05, treatment = 0.05, genotype_treatment = 0.05)

  # Power is per effect, not one all-effects-significant criterion: the
  # "genotype" main effect is easy to detect and is already powered at
  # n_min, while the "genotype_treatment" interaction needs more n.
  res_main <- Randomization::determine_sample_size(
    levels = levels, mu = mu, cv = cv,
    formula = formula, alphas = alphas, target_effect = "genotype", power_target = 0.8, seed = 1234,
    nsim = 100, n_min = 3, n_max = 30
  )
  checks <- logical(6)
  checks[[1]] <- expect_true(is.list(res_main))
  checks[[2]] <- expect_equal(res_main$n, 3)
  checks[[3]] <- expect_true(res_main$power >= 0.8)
  checks[[4]] <- expect_true(is.data.frame(res_main$data))
  checks[[5]] <- expect_equal(names(res_main$data), c("genotype", "treatment", "values"))
  checks[[6]] <- expect_equal(names(res_main$power_by_effect), c("genotype", "treatment", "genotype_treatment"))
  expect_true(all(checks))

  res_interaction <- Randomization::determine_sample_size(
    levels = levels, mu = mu, cv = cv,
    formula = formula, alphas = alphas, target_effect = "genotype_treatment", power_target = 0.8, seed = 1234,
    nsim = 100, n_min = 3, n_max = 30
  )
  expect_true(res_interaction$n > res_main$n)
}
determine_sample_size_test()

single_predictor_test <- function() {
  levels <- list(genotype = c("WT", "mutant"))
  mu <- c(40, 100)
  alphas <- list(genotype = 0.05)

  res <- Randomization::determine_sample_size(
    levels = levels, mu = mu, cv = 0.25,
    formula = formula(values ~ genotype), alphas = alphas,
    target_effect = "genotype", power_target = 0.8, seed = 1234,
    nsim = 100, n_min = 3, n_max = 30
  )
  checks <- logical(3)
  checks[[1]] <- expect_true(is.list(res))
  checks[[2]] <- expect_true(res$power >= 0 && res$power <= 1)
  checks[[3]] <- expect_equal(names(res$power_by_effect), "genotype")
  expect_true(all(checks))
}
single_predictor_test()
