# -------------------------------------------------------------------------------
# Pairwise test functions -- each takes the two groups' raw values and
# returns a single p-value. Passed into estimate_sample_size() below as
# `test_fn` instead of duplicating the whole Monte Carlo search (means/sds
# handling, mcc, family, bisection) once per test: t-Test (Student), Welch
# t-Test and the Wilcoxon rank-sum test only ever differed in this one call.
# -------------------------------------------------------------------------------
pairwise_t_test <- function(x, z) stats::t.test(x, z, var.equal = TRUE)$p.value
pairwise_welch_t_test <- function(x, z) stats::t.test(x, z, var.equal = FALSE)$p.value
pairwise_wilcoxon_test <- function(x, z) stats::wilcox.test(x, z)$p.value

# -------------------------------------------------------------------------------
# Monte Carlo: pairwise comparisons + multiple comparison correction. Which
# test runs is entirely down to `test_fn` (see the three functions above,
# or supply your own). Works whether "groups" is 2 groups (reduces to a
# single pairwise comparison, mcc/family become no-ops) or many (e.g. every
# level-combination of >1 predictor) -- pairwise comparisons don't care how
# many predictors produced the group labels, unlike an omnibus test.
# -------------------------------------------------------------------------------
estimate_sample_size <- function(means, sds, test_fn,
                                 power_target = 0.80, alpha = 0.05,
                                 mcc, family, nsim = 2000, n_min = 2,
                                 n_max = 500, seed = 42) {

  stopifnot(mcc %in% c("holm", "hochberg", "bonferroni", "none"))
  stopifnot(family %in% c("any", "all"))
  stopifnot(is.numeric(means), is.numeric(sds))
  stopifnot(length(means) >= 2, length(means) == length(sds))
  if (is.null(names(means))) names(means) <- paste0("g", seq_along(means))
  if (is.null(names(sds))) names(sds) <- names(means)

  groups <- names(means)
  pairs <- combn(groups, 2, simplify = FALSE)
  is_true_effect <- vapply(pairs, function(pr) means[[pr[1]]] != means[[pr[2]]], logical(1))

  sim_df <- function(n, means, sds, groups) {
    y <- unlist(Map(function(mu, sd) rnorm(n, mean = mu, sd = sd), means, sds))
    predictors <- factor(rep(groups, each = n), levels = groups)
    data.frame(y = y, predictors = predictors)
  }

  one_run_success <- function(n, means, sds, groups, alpha) {
    df <- sim_df(n, means, sds, groups)

    p_raw <- vapply(pairs, function(pr) {
      x <- df$y[df$predictors == pr[1]]
      z <- df$y[df$predictors == pr[2]]
      test_fn(x, z)
    }, numeric(1))

    # MCC
    p_adj <- switch(
      mcc,
      none = p_raw,
      bonferroni = p.adjust(p_raw, method = "bonferroni"),
      holm = p.adjust(p_raw, method = "holm"),
      hochberg = p.adjust(p_raw, method = "hochberg")
    )

    detected <- p_adj <= alpha

    if (family == "any") {
      any(detected[is_true_effect])
    } else if (family == "all") {
      all(detected[is_true_effect])
    }
  }

  est_power <- function(n, means, sds, groups, alpha) {
    set.seed(seed + n)
    mean(replicate(nsim, one_run_success(n, means, sds, groups, alpha)))
  }

  p_min <- est_power(n_min, means, sds, groups, alpha)
  if (p_min >= power_target) return(list(n = n_min, power = p_min))

  p_max <- est_power(n_max, means, sds, groups, alpha)
  if (p_max < power_target) {
    return(list(n = NA_integer_, power = p_max,
      message = "Target power not reached within n_max; increase n_max or adjust assumptions."))
  }

  lo <- n_min
  hi <- n_max
  while (lo + 1 < hi) {
    mid <- floor((lo + hi) / 2)
    p_mid <- est_power(mid, means, sds, groups, alpha)
    if (p_mid >= power_target) hi <- mid else lo <- mid
  }

  list(n = hi, power = est_power(hi, means, sds, groups, alpha), mcc = mcc, family = family,
    alpha = alpha, power_target = power_target, nsim = nsim)
}

# -------------------------------------------------------------------------------
# Monte Carlo: anova
# -------------------------------------------------------------------------------
# mu: one mean per design cell, in the same order as expand.grid(levels)
# produces (first factor varies fastest) -- same convention as
# Superpower::ANOVA_design's `mu`. No more baseline + additive per-factor
# deviation + multiplicative interaction correction: that parameterization
# entangled main effects and interactions in a nonstandard, hard-to-reason-
# about way. Direct cell means are unambiguous -- what you type is the
# effect you're simulating.
define_grid <- function(levels, mu, cv = NULL, sd = NULL) {
  stopifnot(xor(is.null(cv), is.null(sd)))

  grid <- expand.grid(levels)
  stopifnot(length(mu) == nrow(grid))

  grid$mean <- mu
  grid$sd <- if (!is.null(cv)) grid$mean * cv else sd
  grid
}

simulate_data <- function(grid, n, n_covariates) {
  N <- nrow(grid)
  out <- grid[rep(seq_len(N), each = n), seq_len(n_covariates), drop = FALSE]
  out$values <- rnorm(n * N,
    mean = rep(grid$mean, each = n),
    sd = rep(grid$sd, each = n)
  )
  out
}

# Term names as they come out of an aov summary table use ":" for
# interactions (e.g. "CO2:genotype"); alphas/target_effect use "_"
# (e.g. "CO2_genotype") since ":" isn't convenient in list names.
term_pvalues <- function(sim, formula) {
  tab <- summary(aov(formula, data = sim))[[1L]]
  term_names <- gsub(":", "_", trimws(rownames(tab)))
  ps <- stats::setNames(tab[, "Pr(>F)"], term_names)
  ps[!is.na(ps)]
}

factorize_covariates <- function(sim, n_covariates) {
  for (c in seq_len(n_covariates)) {
    sim[, c] <- factor(sim[, c])
  }
  sim
}

# One hit/miss per effect (main effect or interaction), not a single
# all-effects-significant criterion -- power is defined per effect, same
# as Superpower::ANOVA_power does it, since different effects (e.g. an
# interaction vs. a main effect) generally need different n to detect at
# a given power.
run_anova <- function(grid, n, n_covariates, formula, alphas) {
  sim <- factorize_covariates(simulate_data(grid, n, n_covariates), n_covariates)
  ps <- term_pvalues(sim, formula)
  ps <= alphas[names(ps)]
}

simulate_successful_data <- function(grid, n, n_covariates, formula, alphas, target_effect, max_tries = 1000L) {
  for (i in seq_len(max_tries)) {
    sim <- factorize_covariates(simulate_data(grid, n, n_covariates), n_covariates)
    ps <- term_pvalues(sim, formula)
    if (isTRUE(ps[[target_effect]] <= alphas[[target_effect]])) {
      return(sim)
    }
  }
  NULL
}

determine_sample_size <- function(
  levels, mu, cv = NULL, sd = NULL, formula, alphas,
  target_effect, power_target, seed,
  nsim = 10000L, n_min = 3L, n_max = 50L
) {
  alphas <- unlist(alphas)
  stopifnot(target_effect %in% names(alphas))
  grid <- define_grid(levels, mu, cv = cv, sd = sd)
  n_covariates <- length(levels)

  est_power <- function(n) {
    set.seed(seed + n)
    hits_list <- lapply(seq_len(nsim), function(i) run_anova(grid, n, n_covariates, formula, alphas))
    hits <- matrix(unlist(hits_list), nrow = length(hits_list[[1L]]),
      dimnames = list(names(hits_list[[1L]]), NULL))
    rowMeans(hits)
  }

  p_min <- est_power(n_min)
  if (p_min[[target_effect]] >= power_target) {
    se_power <- sqrt(p_min[[target_effect]] * (1 - p_min[[target_effect]]) / nsim)
    return(
      list(n = n_min, power = p_min[[target_effect]], power_by_effect = p_min, power_se = se_power,
        data = simulate_successful_data(grid, n_min, n_covariates, formula, alphas, target_effect)
      )
    )
  }

  p_max <- est_power(n_max)
  if (p_max[[target_effect]] < power_target) {
    return(list(n = NA_integer_, power = p_max[[target_effect]], power_by_effect = p_max,
      power_se = NA_real_, data = NA,
      message = "Target power not reached within n_max; increase n_max or adjust assumptions."))
  }

  lo <- n_min
  hi <- n_max
  while (lo + 1 < hi) {
    mid <- floor((lo + hi) / 2)
    p_mid <- est_power(mid)
    if (p_mid[[target_effect]] >= power_target) hi <- mid else lo <- mid
  }

  p_hi <- est_power(hi)
  se_power <- sqrt(p_hi[[target_effect]] * (1 - p_hi[[target_effect]]) / nsim)
  list(
    n = hi, power = p_hi[[target_effect]], power_by_effect = p_hi, power_se = se_power,
    data = simulate_successful_data(grid, hi, n_covariates, formula, alphas, target_effect)
  )
}

# -------------------------------------------------------------------------------
# One-way omnibus test functions -- each takes the simulated values and the
# group factor and returns a single p-value. Injected into
# determine_sample_size_one_way() below the same way the pairwise tests are
# injected into estimate_sample_size() above.
# -------------------------------------------------------------------------------
kruskal_test <- function(values, group) stats::kruskal.test(values, group)$p.value
welch_anova_test <- function(values, group) stats::oneway.test(values ~ group, var.equal = FALSE)$p.value

# -------------------------------------------------------------------------------
# Monte Carlo: one-way omnibus test (single predictor only -- neither
# Kruskal-Wallis nor Welch's ANOVA have a clean, fast equivalent for >1
# predictor: oneway.test() is strictly one-way by construction, and a
# permutation-based generalisation would be computationally impractical
# nested inside a Monte Carlo sample-size search. Use estimate_sample_size()
# (pairwise) instead for the >1-predictor case -- pairwise comparisons don't
# need a single-factor omnibus test, so that restriction doesn't apply there.
# -------------------------------------------------------------------------------
run_one_way <- function(grid, n, test_fn) {
  sim <- simulate_data(grid, n, n_covariates = 1L)
  sim[[1L]] <- factor(sim[[1L]])
  test_fn(sim$values, sim[[1L]])
}

simulate_successful_one_way_data <- function(grid, n, test_fn, alpha, max_tries = 1000L) {
  for (i in seq_len(max_tries)) {
    sim <- simulate_data(grid, n, n_covariates = 1L)
    sim[[1L]] <- factor(sim[[1L]])
    if (isTRUE(test_fn(sim$values, sim[[1L]]) <= alpha)) {
      return(sim)
    }
  }
  NULL
}

determine_sample_size_one_way <- function(
  level, mu, test_fn, cv = NULL, sd = NULL, alpha = 0.05,
  power_target, seed, nsim = 10000L, n_min = 3L, n_max = 50L
) {
  stopifnot(
    "one-way test -- pass a single predictor's levels (e.g. c('low', 'high')), not a named list of several predictors" =
      !is.list(level)
  )
  grid <- define_grid(list(group = level), mu, cv = cv, sd = sd)

  est_power <- function(n) {
    set.seed(seed + n)
    mean(replicate(nsim, run_one_way(grid, n, test_fn) <= alpha))
  }

  p_min <- est_power(n_min)
  if (p_min >= power_target) {
    se_power <- sqrt(p_min * (1 - p_min) / nsim)
    return(list(
      n = n_min, power = p_min, power_se = se_power,
      data = simulate_successful_one_way_data(grid, n_min, test_fn, alpha)
    ))
  }

  p_max <- est_power(n_max)
  if (p_max < power_target) {
    return(list(n = NA_integer_, power = p_max, power_se = NA_real_, data = NA,
      message = "Target power not reached within n_max; increase n_max or adjust assumptions."))
  }

  lo <- n_min
  hi <- n_max
  while (lo + 1 < hi) {
    mid <- floor((lo + hi) / 2)
    p_mid <- est_power(mid)
    if (p_mid >= power_target) hi <- mid else lo <- mid
  }

  p_hi <- est_power(hi)
  se_power <- sqrt(p_hi * (1 - p_hi) / nsim)
  list(
    n = hi, power = p_hi, power_se = se_power,
    data = simulate_successful_one_way_data(grid, hi, test_fn, alpha)
  )
}
