estimate_sample_size <- function(means, sds,
                                 power_target = 0.80, alpha = 0.05,
                                 mcc, family, nsim = 2000, n_min = 2,
                                 n_max = 500, seed = 42) {

  stopifnot(mcc %in% c("holm", "hochberg", "bonferroni", "none"))
  stopifnot(family %in% c("any", "all"))

  groups <- names(means)
  pairs <- combn(groups, 2, simplify = FALSE)
  is_true_effect <- vapply(pairs, function(pr)
    means[[pr[1]]] != means[[pr[2]]], logical(1))

  sim_df <- function(n) {
    y <- unlist(Map(function(mu, sd)
      rnorm(n, mean = mu, sd = sd), means, sds))
    predictors <- factor(rep(groups, each = n), levels = groups)
    data.frame(y = y, predictors = predictors)
  }

  one_run_success <- function(n) {
    df <- sim_df(n)

    p_raw <- vapply(pairs, function(pr) {
      x <- df$y[df$predictors == pr[1]]
      z <- df$y[df$predictors == pr[2]]
      t.test(x, z, var.equal = FALSE)$p.value
    }, numeric(1))

    p_adj <- switch(
      mcc,
      none = p_raw,
      bonferroni = p.adjust(p_raw, "bonferroni"),
      holm = p.adjust(p_raw, "holm"),
      hochberg = p.adjust(p_raw, "hochberg")
    )

    detected <- p_adj <= alpha

    if (family == "any") {
      any(detected[is_true_effect])
    } else {
      all(detected[is_true_effect])
    }
  }

  est_power <- function(n) {
    set.seed(seed + n)
    mean(replicate(nsim, one_run_success(n)))
  }

  lo <- n_min
  hi <- n_max

  while (lo + 1 < hi) {
    mid <- floor((lo + hi) / 2)
    if (est_power(mid) >= power_target) hi <- mid else lo <- mid
  }

  list(n = hi, power = est_power(hi))
}
means <- c(control = 10, one = 6, two = 6)
sds   <- c(control = 2,  one = 1.2, two = 1.2)
estimate_sample_size(
  means,
  sds,
  mcc = "holm",
  family = "all",
  nsim = 2000,
  n_max = 300
)
