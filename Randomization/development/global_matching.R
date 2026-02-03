# Helper
# --------------------------------------------------------------------------------------------
normalize_df <- function(df) {
  as.data.frame(lapply(df, function(col) {
    m <- mean(col, na.rm = TRUE)
    s <- sd(col, na.rm = TRUE)
    if (!is.finite(s) || s == 0) return(col - m)
    (col - m) / s
  }))
}

calc_possible_swaps <- function(blocks) {
  unique_blocks <- unique(blocks)
  indices <- list()
  grid <- combn(unique_blocks, 2)
  for (c in seq_len(ncol(grid))) {
    pair <- grid[, c]
    l_indices <- which(blocks == pair[[1L]])
    r_indices <- which(blocks == pair[[2L]])
    indices[[length(indices) + 1L]] <- expand.grid(l_indices, r_indices)
  }
  Reduce(rbind, indices)
}

# Loss functions
# --------------------------------------------------------------------------------------------
loss_mean_group_vs_group <- function(df, blocks) {
  grid <- combn(unique(blocks), 2)
  error <- 0.0
  for (c in seq_len(ncol(grid))) {
    pair <- grid[, c]
    l <- df[blocks == pair[1], , drop = FALSE]
    r <- df[blocks == pair[2], , drop = FALSE]
    d <- colMeans(l, na.rm = TRUE) - colMeans(r, na.rm = TRUE)
    error <- error + sum(d^2)
  }
  error
}

loss_mean_group_vs_all <- function(df, blocks) {
  ub <- unique(blocks)
  mu_all <- colMeans(df, na.rm = TRUE)
  error <- 0.0
  for (i in seq_len(length(ub))) {
    l <- df[blocks == ub[i], , drop = FALSE]
    d <- colMeans(l, na.rm = TRUE) - mu_all
    error <- error + sum(d^2)
  }
  error
}

loss_mean_m2_group_vs_all <- function(df, blocks, lambda = 1.0) {
  ub <- unique(blocks)
  mu_all <- colMeans(df, na.rm = TRUE)
  # Var =E[x2]−μg2
  m2_all <- colMeans(df^2, na.rm = TRUE)
  error <- 0.0
  for (b in ub) {
    g <- df[blocks == b, , drop = FALSE]
    d_mu <- colMeans(g, na.rm = TRUE) - mu_all
    d_m2 <- colMeans(g^2, na.rm = TRUE) - m2_all
    error <- error + sum(d_mu^2) + lambda * sum(d_m2^2)
  }
  error
}

loss_mean_m2_cov_group_vs_all <- function(df, blocks, lambda_m2 = 1.0, lambda_cov = 1.0) {
  ub <- unique(blocks)
  mu_all <- colMeans(df, na.rm = TRUE)
  m2_all <- colMeans(df^2, na.rm = TRUE)
  # cross-moments: E[x_i * x_j] for i<j
  p <- ncol(df)
  pair_idx <- combn(p, 2)
  cross_all <- apply(pair_idx, 2, function(ix) mean(df[, ix[1]] * df[, ix[2]], na.rm = TRUE))
  error <- 0.0
  for (b in ub) {
    g <- df[blocks == b, , drop = FALSE]
    d_mu <- colMeans(g, na.rm = TRUE) - mu_all
    d_m2 <- colMeans(g^2, na.rm = TRUE) - m2_all
    cross_g <- apply(pair_idx, 2, function(ix) mean(g[, ix[1]] * g[, ix[2]], na.rm = TRUE))
    d_cross <- cross_g - cross_all
    error <- error + sum(d_mu^2) + lambda_m2 * sum(d_m2^2) + lambda_cov * sum(d_cross^2)
  }
  error
}

loss_mean_iqr_group_vs_all <- function(df, blocks, lambda = 0.1, na.rm = TRUE, type = 7) {
  ub <- unique(blocks)
  mu_all <- colMeans(df, na.rm = na.rm)
  iqr_col <- function(x) {
    qs <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm, type = type)
    qs[[2]] - qs[[1]]
  }
  iqr_all <- vapply(df, iqr_col, numeric(1))
  error <- 0.0
  for (b in ub) {
    g <- df[blocks == b, , drop = FALSE]

    d_mu <- colMeans(g, na.rm = na.rm) - mu_all
    error <- error + sum(d_mu * d_mu)

    d_iqr <- vapply(g, iqr_col, numeric(1)) - iqr_all
    error <- error + lambda * sum(d_iqr * d_iqr)
  }
  error
}

# Optimizer
# --------------------------------------------------------------------------------------------
greedy_optimize <- function(groups_inp, blocks, perm_init,
                            mode = "loss_mean_group_vs_all", max_iter = 200L, verbose = TRUE, message = "") {
  stopifnot(
    nrow(groups_inp) == length(blocks),
    mode %in% c("loss_mean_group_vs_group", "loss_mean_group_vs_all", "loss_mean_iqr_group_vs_all",
                "loss_mean_m2_group_vs_all", "loss_mean_m2_cov_group_vs_all"),
    is.character(message)
  )
  loss <- loss_mean_group_vs_all
  if (mode == "loss_mean_group_vs_group") {
    loss <- loss_mean_group_vs_group
  } else if (mode == "loss_mean_iqr_group_vs_all") {
    loss <- loss_mean_iqr_group_vs_all
  } else if (mode == "loss_mean_m2_group_vs_all") {
    loss <- loss_mean_m2_group_vs_all
  } else if (mode == "loss_mean_m2_cov_group_vs_all") {
    loss <- loss_mean_m2_cov_group_vs_all
  }

  swaps <- calc_possible_swaps(blocks)
  swaps <- as.matrix(swaps)
  perm <- perm_init
  groups <- normalize_df(groups_inp)
  cur  <- loss(groups[perm, , drop = FALSE], blocks)
  tol <- 1e-12

  if (verbose) cat(message, "start loss:", cur, "\n")

  one_swap <- function(swaps, perm, best_loss) {
    best_k <- NA_integer_
    for (k in seq_len(nrow(swaps))) {
      i <- swaps[k, 1]
      j <- swaps[k, 2]

      perm2 <- perm
      perm2[c(i, j)] <- perm2[c(j, i)]

      L <- loss(groups[perm2, , drop = FALSE], blocks)
      if (L < (best_loss - tol)) {
        best_loss <- L
        best_k <- k
      }
    }
    list(best_loss = best_loss, best_k = best_k)
  }

  for (it in seq_len(max_iter)) {
    step <- one_swap(swaps, perm, cur)
    if (is.na(step$best_k)) {
      if (verbose) cat(message, "no improvement at iter", it, "-> stop\n")
      break
    }
    i <- swaps[step$best_k, 1]
    j <- swaps[step$best_k, 2]
    perm[c(i, j)] <- perm[c(j, i)]
    cur <- step$best_loss
    if (verbose) cat(message, "iter", it, "loss:", cur, "swap:", i, j, "\n")
  }

  list(
    perm = perm,
    assigned = groups_inp[perm, , drop = FALSE],
    blocks = blocks,
    loss = cur
  )
}

# --------------------------------------------------------------------------------------------
# Tests
# --------------------------------------------------------------------------------------------
run <- function(seed, groups, blocks, mode) {
  set.seed(seed)
  N <- nrow(groups)
  perms <- sample.int(N)
  greedy_optimize(groups, blocks, perms, mode = mode, max_iter = 50L, verbose = TRUE)
}

summary <- function(df, n_blocks) {
  df <- df$assigned
  df$treatment <- n_blocks$treatment
  df$location <- n_blocks$location
  with(df, table(interaction(treatment, location), weights)) |> print()
  with(df, table(interaction(treatment, location), blood)) |> print()
}

# 2 finite groups; where both variables are completly random
# Thus, the distribution of the groups should also be random. This case cannot work
# --------------------------------------------------------------------------------------------

# 1 finite groups but not normally distributed
# --------------------------------------------------------------------------------------------
test_1_finite_group_non_normal <- function() {
  n_blocks <- data.frame(treatment = rep(c("control", "one", "two"), each = 27L))
  blocks <- n_blocks[[1L]]
  groups <- data.frame(blood = c(rep(4000, 27L), rep(3500, 27L), rep(300, 27L)))
  correct <- function(df) {
    df <- df$assigned
    df$treatment <- n_blocks$treatment
    tbl <- with(df, table(treatment, blood))
    attr(tbl, "dimnames") <- NULL
    attr(tbl, "class") <- NULL
    identical(tbl, matrix(9L, 3L, 3L))
  }
  mode <- "loss_mean_m2_group_vs_all"
  checks <- logical(3L)
  checks[[1L]] <- run(42, groups, blocks, mode) |> correct()
  checks[[2L]] <- run(1234, groups, blocks, mode) |> correct()
  checks[[3L]] <- run(3344, groups, blocks, mode) |> correct()
  checks
}
# test_1_finite_group_non_normal() # Works!

# 2 finite groups; linear independent
# --------------------------------------------------------------------------------------------
test_2_independent_groups <- function() {
  n_blocks <- data.frame(
    treatment = rep(c("control", "one", "two"), each = 36L),
    location = rep(rep(c("A", "B", "C", "D"), each = 9L), 3L)
  )
  blocks <- interaction(n_blocks[[1L]], n_blocks[[2L]])
  set.seed(333)
  groups <- data.frame(
    weights = c(rep(325, 36L), rep(300, 36L), rep(275, 36L)),
    blood = rnorm(108, mean = 10, sd = 1)
  )
  mode <- "loss_mean_m2_cov_group_vs_all"
  checks_outer <- logical(1L)
  correct <- function(res) {
    checks <- logical(3L)
    df <- res$assigned
    df$treatment <- n_blocks$treatment
    df$location <- n_blocks$location
    tbl_weights <- with(df, table(treatment, weights))
    attr(tbl_weights, "dimnames") <- NULL
    attr(tbl_weights, "class") <- NULL
    expected <- matrix(12L, 3L, 3L)
    checks[[1L]] <- identical(tbl_weights, expected)
    tbl_bloods <- aggregate(blood ~ treatment, df, function(x)
      c(mean = mean(x), sd = sd(x))) |> as.data.frame()
    b <- tbl_bloods$blood
    checks[[2L]] <- sum(abs(b[, 1L] - rep(10, 3L))) < 0.1
    checks[[3L]] <- sum(abs(b[, 2L] - rep(1, 3L))) < 0.1
    all(checks)
  }
  checks_outer[[1L]] <- run(42, groups, blocks, mode) |> correct()
  all(checks_outer)
}
# test_2_independent_groups() # Works!

# 2 finite groups; linear dependent
# --------------------------------------------------------------------------------------------
test_2_dependent_groups <- function() {
  n_blocks <- data.frame(
    treatment = rep(c("control", "one", "two"), each = 36L),
    location = rep(rep(c("A", "B", "C", "D"), each = 9L), 3L)
  )
  blocks <- interaction(n_blocks[[1L]], n_blocks[[2L]])
  groups <- data.frame(
    weights = c(rep(325, 36L), rep(300, 36L), rep(275, 36L)),
    blood = c(rep(12, 36L), rep(10, 36L), rep(8, 36L))
  )
  correct <- function(df) {
    df <- df$assigned
    df$treatment <- n_blocks$treatment
    tbl_weights <- with(df, table(treatment, weights))
    attr(tbl_weights, "dimnames") <- NULL
    attr(tbl_weights, "class") <- NULL
    tbl_blood <- with(df, table(treatment, blood))
    attr(tbl_blood, "dimnames") <- NULL
    attr(tbl_blood, "class") <- NULL
    identical(tbl_weights, matrix(12L, 3L, 3L)) && identical(tbl_blood, matrix(12L, 3L, 3L))
  }
  # But the linear dependency has to be confirmed when setting this method!
  # But if they are linear dependent one could assign animals based on one variable
  mode <- "loss_mean_m2_group_vs_all"
  checks <- logical(3L)
  checks[[1L]] <- run(42, groups, blocks, mode) |> correct()
  checks[[2L]] <- run(1234, groups, blocks, mode) |> correct()
  checks[[3L]] <- run(3344, groups, blocks, mode) |> correct()
  all(checks)
}
# test_2_dependent_groups() # Works!

# 1 finite groups
# --------------------------------------------------------------------------------------------
test_1_finite_group <- function() {
  n_blocks <- data.frame(treatment = rep(c("control", "one", "two"), each = 27L))
  blocks <- n_blocks[[1L]]
  groups <- data.frame(weights = c(rep(325, 27L), rep(300, 27L), rep(275, 27L)))
  correct <- function(df) {
    df <- df$assigned
    df$treatment <- n_blocks$treatment
    tbl <- with(df, table(treatment, weights))
    attr(tbl, "dimnames") <- NULL
    attr(tbl, "class") <- NULL
    identical(tbl, matrix(9L, 3L, 3L))
  }
  mode <- "loss_mean_m2_group_vs_all"
  checks <- logical(3L)
  checks[[1L]] <- run(42, groups, blocks, mode) |> correct()
  checks[[2L]] <- run(1234, groups, blocks, mode) |> correct()
  checks[[3L]] <- run(3344, groups, blocks, mode) |> correct()
  checks
}
# test_1_finite_group() # Works!
