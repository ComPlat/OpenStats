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
    cat("Current n: ", n, "\n")
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

means <- c(control = 10, one = 6, two = 6)
sds <- c(control = 2, one = 1.2, two = 1.2)
mcc <- "holm"
mcc <- "bonferroni"
family <- "all"
estimate_sample_size(
  means, sds, mcc = mcc, family = family,
  nsim = 2000, n_max = 300
)
