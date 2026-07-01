# Simulate starch concentration in Arabidopsis thaliana plants
# genotypes: "Col-0", "hpr1-1", "sex1"
# time: 8, 12, 16, 24
# CO2 ppm: 300, 800, 1200

# ------------------------------------------------------
# Define for each covariate the levels, means and global cv
# Moreover, define the formual, and the alphas
# ------------------------------------------------------
# Lets say the first time point (8/begin of day) is 1.0
time <- c(8, 12, 16, 24)
means_time <- c(10, 25, 45, 20)
# Moreover, do the same for the CO2 concentrations
CO2 <- c(300, 800, 1200)
means_CO2 <- c(40, 60, 80)
# Furthermore, consider the genotypes
genotype <- as.factor(c("Col-0", "hpr1-1", "sex1"))
means_genotypes <- c(40, 40, 120)

levels <- list(
  time = time, CO2 = CO2, genotype = genotype
)
means <- list(
  time = means_time, CO2 = means_CO2, genotype = means_genotypes
)
interactions <- list(
  list(
    mask = quote(CO2 == 300 & genotype == "hpr1-1"), value = 0.8
  ),
  list(
    mask = quote(CO2 == 1200 & genotype == "hpr1-1"), value = 1.2
  )
)
coefficient_of_variance <- 0.25
formula <- formula(values ~ time + CO2*genotype)
alphas <- list(
  time = 0.05, CO2 = 0.05, genotype = 0.05, CO2_genotype = 0.05
)
# ------------------------------------------------------
# Create grid
# ------------------------------------------------------
define_grid <- function(levels, means, cv, interactions) {
  grid <- expand.grid(levels)
  means <- lapply(means, function(m) { # Using relative values
    m / m[[1L]]
  })
  ncols <- ncol(grid)
  grid$mean <- 1.0
  for (c in seq_len(ncols)) {
    grid_col <- grid[[c]]
    mean_col <- means[[c]]
    levels_col <- levels[[c]]
    grid$mean <- grid$mean * mean_col[match(grid_col, levels_col)]
  }
  grid$interaction <- 1.0
  for (i in seq_along(interactions)) {
    idx <- with(grid, eval(interactions[[i]]$mask))
    grid$interaction[idx] <- grid$interaction[idx] * interactions[[i]]$value
  }
  grid$mean <- grid$mean * grid$interaction
  grid$sd <- grid$mean * cv
  grid
}

# ------------------------------------------------------
# Simulate data
# ------------------------------------------------------
simulate_data <- function(grid, n, n_covariates) {
  N <- nrow(grid)
  out <- grid[rep(seq_len(N), each = n), seq_len(n_covariates), drop = FALSE]
  out$values <- rnorm(n * N,
    mean = rep(grid$mean, each = n),
    sd = rep(grid$sd, each = n)
  )
  out
}

# ------------------------------------------------------
# run anova
# ------------------------------------------------------
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

# ------------------------------------------------------
# Simulate successfull data
# ------------------------------------------------------
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

# ------------------------------------------------------
# determine sample size
# ------------------------------------------------------
determine_sample_size <- function(
  levels, means, cv, interactions, formula, alphas, power_target, seed,
  nsim = 10000L, n_min = 3L, n_max = 50L
) {
  alphas <- unlist(alphas)
  grid <- define_grid(levels, means, cv, interactions)
  n_covariates <- length(levels)

  est_power <- function(grid, n, n_covariates, formula, alphas) {
    cat("Current n: ", n, "\n")
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
res <- determine_sample_size(
  levels = levels, means = means, cv = coefficient_of_variance, interactions = interactions,
  formula = formula, alphas = alphas, power_target = 0.8, seed = 1234
)
res$n
res$power
summary(aov(formula, data = res$data))
head(res$data)
library(ggplot2)
ggplot(data = res$data, aes(y = values, x = time, fill = factor(CO2), group = interaction(time, factor(CO2)))) +
  geom_boxplot() +
  facet_wrap(~ genotype, scales = "free")

ggplot(data = res$data, aes(y = values, x = factor(CO2), fill = genotype, group = interaction(genotype, factor(CO2)))) +
  geom_boxplot()
