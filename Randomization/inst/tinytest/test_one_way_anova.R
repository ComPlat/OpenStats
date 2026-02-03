library(tinytest)

anova_error_trigger <- function() {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
  )
  groups_inf <- paste0("Day", 1:3)
  groups_fin <- rnorm(200, 300, 25)
  checks <- logical(10)

  # 1: predictors not list
  checks[[1]] <- expect_error(
    randomization_one_way_anova(
      predictors = 1:3,
      primary_factor = "Treatment",
      groups = groups_inf,
      group_type = "infinite",
      ratios = c(1,1,1),
      cohens_f = 0.25,
      sig_level = 0.05,
      desired_power = 0.8,
      randomization_method = "simple",
      n_quantiles = 10L,
      seed = 1
    )
  )
  # 2: invalid group_type
  checks[[2]] <- expect_error(
    randomization_one_way_anova(
      predictors = predictors,
      primary_factor = "Treatment",
      groups = groups_inf,
      group_type = "invalid",
      ratios = c(1,1,1),
      cohens_f = 0.25,
      sig_level = 0.05,
      desired_power = 0.8,
      randomization_method = "simple",
      n_quantiles = 10L,
      seed = 1
    )
  )
  # 3: primary factor invalid
  checks[[3]] <- expect_error(
    randomization_one_way_anova(
      predictors = predictors,
      primary_factor = "invalid",
      groups = groups_inf,
      group_type = "infinite",
      ratios = c(1,1,1),
      cohens_f = 0.25,
      sig_level = 0.05,
      desired_power = 0.8,
      randomization_method = "simple",
      n_quantiles = 10L,
      seed = 1
    )
  )
  # 4: invalid sig_level
  checks[[4]] <- expect_error(
    randomization_one_way_anova(
      predictors = predictors,
      primary_factor = "Treatment",
      groups = groups_inf,
      group_type = "finite",
      ratios = c(1,1,1),
      cohens_f = 0.25,
      sig_level = "Invalid",
      desired_power = 0.8,
      randomization_method = "simple",
      n_quantiles = 10L,
      seed = 1
    )
  )
  # 5: invalid randomization_method
  checks[[5]] <- expect_error(
    randomization_one_way_anova(
      predictors = predictors,
      primary_factor = "Treatment",
      groups = groups_inf,
      group_type = "infinite",
      ratios = c(1,1,1),
      cohens_f = 0.25,
      sig_level = 0.05,
      desired_power = 0.8,
      randomization_method = "invalid",
      n_quantiles = 10L,
      seed = 1
    )
  )
  # 6: infinite but non-character groups
  checks[[6]] <- expect_error(
    randomization_one_way_anova(
      predictors = predictors,
      primary_factor = "Treatment",
      groups = groups_fin,
      group_type = "infinite",
      ratios = c(1,1,1),
      cohens_f = 0.25,
      sig_level = 0.05,
      desired_power = 0.8,
      randomization_method = "simple",
      n_quantiles = 10L,
      seed = 1
    )
  )
  # 7: finite but non-numeric
  checks[[7]] <- expect_error(
    randomization_one_way_anova(
      predictors = predictors,
      primary_factor = "Treatment",
      groups = groups_inf,
      group_type = "finite",
      ratios = c(1,1,1),
      cohens_f = 0.25,
      sig_level = 0.05,
      desired_power = 0.8,
      randomization_method = "simple",
      n_quantiles = 10L,
      seed = 1
    )
  )
  # 8: block_stratified but no strata_cols
  checks[[8]] <- expect_error(
    randomization_one_way_anova(
      predictors = predictors,
      primary_factor = "Treatment",
      groups = groups_inf,
      group_type = "infinite",
      ratios = c(1,1,1),
      cohens_f = 0.25,
      sig_level = 0.05,
      desired_power = 0.8,
      randomization_method = "block_stratified",
      n_quantiles = 10L,
      seed = 1
    )
  )
  # 9: invalid seed
  checks[[9]] <- expect_error(
    randomization_one_way_anova(
      predictors = predictors,
      primary_factor = "Treatment",
      groups = groups_inf,
      group_type = "infinite",
      ratios = c(1,1,1),
      cohens_f = 0.25,
      sig_level = 0.05,
      desired_power = 0.8,
      strata_cols = "Treatment",
      randomization_method = "block_stratified",
      n_quantiles = 10L,
      seed = c(1, 2)
    )
  )
  # 10: ratios wrong length
  checks[[10]] <- expect_error(
    randomization_one_way_anova(
      predictors = predictors,
      primary_factor = "Treatment",
      groups = groups_inf,
      group_type = "infinite",
      ratios = c(1,1,1,1,1),
      cohens_f = 0.25,
      sig_level = 0.05,
      desired_power = 0.8,
      strata_cols = "Treatment",
      randomization_method = "block_stratified",
      n_quantiles = 10L,
      seed = 1
    )
  )
  expect_true(all(checks))
}
anova_error_trigger()

anova_simple_infinite <- function() {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
  )
  res <- Randomization::randomization_one_way_anova(
    predictors = predictors,
    primary_factor = "Treatment",
    groups = paste0("Day", 1:3),
    group_type = "infinite",
    ratios = c(1,1,1),
    cohens_f = 0.25,
    sig_level = 0.05,
    desired_power = 0.8,
    randomization_method = "simple",
    n_quantiles = 10L,
    seed = 42
  )
  checks <- logical(3)
  checks[[1]] <- expect_true(is.data.frame(res))
  checks[[2]] <- expect_true("random_group" %in% names(res))
  checks[[3]] <- expect_true(all(res$random_group %in% paste0("Day", 1:3)))
  expect_true(all(checks))
}
anova_simple_infinite()

anova_block_infinite <- function() {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
  )
  res <- Randomization::randomization_one_way_anova(
    predictors,
    primary_factor = "Treatment",
    groups = paste0("Day", 1:15),
    group_type = "infinite",
    ratios = rep(1, 15),
    cohens_f = 0.25,
    sig_level = 0.001,
    desired_power = 0.8,
    randomization_method = "block",
    n_quantiles = 10L,
    seed = 1234
  )
  tab <- table(res$random_group, res$Treatment)
  checks <- logical(4)
  checks[[1]] <- expect_true(all(colSums(tab) > 0))
  checks[[2]] <- expect_true(length(unique(tab[, "A"])) == 1)
  checks[[3]] <- expect_true(length(unique(tab[, "B"])) == 1)
  checks[[4]] <- expect_true(length(unique(tab[, "C"])) == 1)
  expect_true(all(checks))
}
anova_block_infinite()

anova_block_finite <- function(seed1, seed2) {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
  )
  set.seed(seed1)
  weights <- rnorm(500, 300, 25)
  res <- Randomization::randomization_one_way_anova(
    predictors,
    primary_factor = "Treatment",
    groups = weights,
    group_type = "finite",
    ratios = NULL,
    cohens_f = 0.25,
    sig_level = 0.05,
    desired_power = 0.8,
    randomization_method = "block",
    n_quantiles = 10L,
    seed = seed2
  )
  subs <- split(res, res$Treatment)
  Fg <- ecdf(weights)
  xs <- sort(weights)
  Dvals <- vapply(subs, function(d) {
    values <- weights[as.integer(d$random_group)]
    xb <- as.numeric(values)
    Fb <- ecdf(xb)
    max(abs(Fg(xs) - Fb(xs)))
  }, numeric(1))
  checks <- logical(5)
  checks[[1]] <- expect_true(all(Dvals < 0.25))
  checks[[2]] <- expect_equal(length(unique(res$random_group)),
                              length(res$random_group))
  checks[[3]] <- expect_true(nrow(res) > 0)
  checks[[4]] <- expect_true(is.numeric(as.numeric(res$random_group)))
  checks[[5]] <- expect_true(!anyNA(res$random_group))

  expect_true(all(checks))
}
anova_block_finite(1, 42)
anova_block_finite(12, 77)
anova_block_finite(999, 314)

anova_block_strat_infinite <- function() {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
  )
  res <- Randomization::randomization_one_way_anova(
    predictors,
    primary_factor = "Treatment",
    groups = paste0("Day", 1:3),
    group_type = "infinite",
    ratios = c(1,1,1),
    cohens_f = 0.25,
    sig_level = 0.05,
    desired_power = 0.8,
    randomization_method = "block_stratified",
    strata_cols = c("cellLines"),
    n_quantiles = 10L,
    seed = 123
  )
  checks <- logical(3)
  checks[[1]] <- expect_true("random_group" %in% names(res))
  checks[[2]] <- expect_true(all(res$random_group %in% paste0("Day", 1:3)))
  checks[[3]] <- expect_true(nrow(res) > 0)
  expect_true(all(checks))
}
anova_block_strat_infinite()

anova_seed_reproducibility <- function() {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
  )
  res1 <- Randomization::randomization_one_way_anova(
    predictors, "Treatment",
    paste0("Day", 1:3), "infinite", c(1,1,1),
    0.25, 0.05, 0.8,
    n_quantiles = 10L,
    "block", seed = 999
  )
  res2 <- Randomization::randomization_one_way_anova(
    predictors, "Treatment",
    paste0("Day", 1:3), "infinite", c(1,1,1),
    0.25, 0.05, 0.8,
    "block",
    n_quantiles = 10L,
    seed = 999
  )
  expect_equal(res1$random_group, res2$random_group)
}
anova_seed_reproducibility()
