# -------------------------------------------------------------------------------
# Monte Carlo: multiple comparison
# -------------------------------------------------------------------------------
estimate_sample_size <- function(means, sds,
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
      t.test(x, z, var.equal = FALSE)$p.value
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
define_grid <- function(levels, means, cv = NULL, sd = NULL, interactions) {
  stopifnot(xor(is.null(cv), is.null(sd)))

  grid <- expand.grid(levels)
  ncols <- ncol(grid)
  baseline <- means[[1L]][[1L]]

  grid$mean <- baseline
  for (c in seq_len(ncols)) {
    grid_col <- grid[[c]]
    mean_col <- means[[c]]
    levels_col <- levels[[c]]
    deviation <- mean_col - mean_col[[1L]] # 0 at this factor's own baseline level
    grid$mean <- grid$mean + deviation[match(grid_col, levels_col)]
  }
  grid$interaction <- 1.0
  for (i in seq_along(interactions)) {
    idx <- with(grid, eval(interactions[[i]]$mask))
    grid$interaction[idx] <- grid$interaction[idx] * interactions[[i]]$value
  }
  grid$mean <- grid$mean * grid$interaction
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

run_anova <- function(grid, n, n_covariates, formula, alphas) {
  sim <- simulate_data(grid, n, n_covariates)
  for (c in seq_len(n_covariates)) {
    sim[, c] <- factor(sim[, c])
  }
  tab <- summary(aov(formula, data = sim))[[1L]]
  ps <- tab[, "Pr(>F)"]
  ps <- ps[!is.na(ps)]
  all(ps <= alphas)
}

simulate_successful_data <- function(grid, n, n_covariates, formula, alphas, max_tries = 1000L) {
  for (i in seq_len(max_tries)) {
    sim <- simulate_data(grid, n, n_covariates)
    tab <- summary(aov(formula, data = sim))[[1L]]
    ps <- tab[, "Pr(>F)"]
    ps <- ps[!is.na(ps)]
    if (all(ps <= alphas)) {
      return(sim)
    }
  }
  NULL
}

determine_sample_size <- function(
  levels, means, cv = NULL, sd = NULL, interactions, formula, alphas, power_target, seed,
  nsim = 10000L, n_min = 3L, n_max = 50L
) {
  alphas <- unlist(alphas)
  grid <- define_grid(levels, means, cv = cv, sd = sd, interactions = interactions)
  n_covariates <- length(levels)

  est_power <- function(grid, n, n_covariates, formula, alphas) {
    set.seed(seed + n)
    mean(replicate(nsim, run_anova(grid, n, n_covariates, formula, alphas)))
  }

  p_min <- est_power(grid, n_min, n_covariates, formula, alphas)
  if (p_min >= power_target) {
    se_power <- sqrt(p_min * (1 - p_min) / nsim)
    return(
      list(n = n_min, power = p_min, power_se = se_power,
        data = simulate_successful_data(grid, n_min, n_covariates, formula, alphas)
      )
    )
  }

  p_max <- est_power(grid, n_max, n_covariates, formula, alphas)
  if (p_max < power_target) {
    return(list(n = NA_integer_, power = p_max, power_se = NA_real_, data = NA,
      message = "Target power not reached within n_max; increase n_max or adjust assumptions."))
  }

  lo <- n_min
  hi <- n_max
  while (lo + 1 < hi) {
    mid <- floor((lo + hi) / 2)
    p_mid <- est_power(grid, mid, n_covariates, formula, alphas)
    if (p_mid >= power_target) hi <- mid else lo <- mid
  }

  p_hi <- est_power(grid, hi, n_covariates, formula, alphas)
  se_power <- sqrt(p_hi * (1 - p_hi) / nsim)
  list(
    n = hi, power = p_hi, power_se = se_power,
    data = simulate_successful_data(grid, hi, n_covariates, formula, alphas)
  )
}
