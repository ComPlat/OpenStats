# Lab values have to be practically adjustable; clamping since signif can round across a bound
round_lab_value <- function(x, lower, upper, digits = 3L) {
  pmin(pmax(signif(x, digits), lower), upper)
}

lhs_unit <- function(n, d) {
  vapply(seq_len(d), function(j) (sample.int(n) - stats::runif(n)) / n, numeric(n))
}

min_distance <- function(u) {
  min(stats::dist(u))
}

maximin_lhs <- function(n, d, n_candidates = 100L) {
  if (n < 2L) return(matrix(lhs_unit(n, d), nrow = n, ncol = d))
  best <- NULL
  best_dist <- -Inf
  for (i in seq_len(n_candidates)) {
    candidate <- matrix(lhs_unit(n, d), nrow = n, ncol = d)
    dist <- min_distance(candidate)
    if (dist > best_dist) {
      best <- candidate
      best_dist <- dist
    }
  }
  best
}

# Level based types pick a level by stratum, so the LHS stratification keeps the levels balanced
map_lhs_column <- function(u, levels, type) {
  if (type == "continuous") {
    # BO later derives its bounds from min/max of the measured data, so the design has to hit both bounds
    if (length(u) >= 2L) {
      u[which.min(u)] <- 0
      u[which.max(u)] <- 1
    }
    values <- as.numeric(levels)
    return(round_lab_value(values[1L] + u * (values[2L] - values[1L]), values[1L], values[2L]))
  }
  picked <- levels[pmin(floor(u * length(levels)) + 1L, length(levels))]
  if (type == "categorical") picked else as.numeric(picked)
}

lhs_design <- function(predictors, predictor_types, n, seed, n_candidates = 100L) {
  if (length(predictors) == 0L) stop("Define predictors first")
  if (is.na(n) || n < 1L) stop("Number of experiments must be at least 1")
  set.seed(seed)
  u <- maximin_lhs(n, length(predictors), n_candidates)
  cols <- lapply(seq_along(predictors), function(j) {
    name <- names(predictors)[[j]]
    map_lhs_column(u[, j], predictors[[name]], predictor_types[[name]])
  })
  names(cols) <- names(predictors)
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}
