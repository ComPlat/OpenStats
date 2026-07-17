library(tinytest)

determine_sample_size_test <- function() {
  time <- c(8, 12, 16, 24)
  means_time <- c(10, 25, 45, 20)
  CO2 <- c(300, 800, 1200)
  means_CO2 <- c(40, 60, 80)
  genotype <- as.factor(c("Col-0", "hpr1-1", "sex1"))
  means_genotypes <- c(40, 40, 120)

  levels <- list(time = time, CO2 = CO2, genotype = genotype)
  means <- list(time = means_time, CO2 = means_CO2, genotype = means_genotypes)
  interactions <- list(
    list(mask = quote(CO2 == 300 & genotype == "hpr1-1"), value = 0.8),
    list(mask = quote(CO2 == 1200 & genotype == "hpr1-1"), value = 1.2)
  )
  cv <- 0.25
  formula <- formula(values ~ time + CO2 * genotype)
  alphas <- list(time = 0.05, CO2 = 0.05, genotype = 0.05, CO2_genotype = 0.05)

  res <- Randomization::determine_sample_size(
    levels = levels, means = means, cv = cv, interactions = interactions,
    formula = formula, alphas = alphas, power_target = 0.8, seed = 1234,
    nsim = 50, n_min = 3, n_max = 15
  )
  checks <- logical(5)
  checks[[1]] <- expect_true(is.list(res))
  checks[[2]] <- expect_equal(res$n, 5)
  checks[[3]] <- expect_true(res$power >= 0 && res$power <= 1)
  checks[[4]] <- expect_true(is.data.frame(res$data))
  checks[[5]] <- expect_equal(names(res$data), c("time", "CO2", "genotype", "values"))
  expect_true(all(checks))
}
determine_sample_size_test()
