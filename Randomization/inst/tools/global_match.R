# Loss functions and swap search of the global matching (R/global_match.R),
# compiled by ast2ast::make_package (development/testing.R).
# Entry points: loss_m2cov_vs_all_perm, loss_mahal_perm (loss of one perm),
# best_swap_m2cov, best_swap_mahal (best single swap of one greedy iteration).

new_type(
  MaskedData,
  slots(
    Z |> type(mat(double)),
    M |> type(mat(double))
  )
)
new_type(
  StatsM2Cov,
  slots(
    GramAll |> type(mat(double)),
    CntAll |> type(mat(double)),
    mu_all |> type(vec(double)),
    m2_all |> type(vec(double))
  )
)
# best_k = 0: no swap improves the loss
new_type(
  SwapResult,
  slots(
    best_loss |> type(double),
    best_k |> type(int)
  )
)

# NA mask: Z = values with NA -> 0, M = present indicator
na_mask <- function(groups) {
  argtypes(groups |> type(borrow_mat(double)) |> const())
  returntype(MaskedData)
  nrows <- nrow(groups)
  ncols <- ncol(groups)
  data |> type(MaskedData)
  data$Z <- matrix(0.0, nrows, ncols)
  data$M <- matrix(0.0, nrows, ncols)
  for (row in seq_len(nrows)) {
    for (col in seq_len(ncols)) {
      x <- groups[row, col]
      if (!is.na(x)) {
        data$Z[row, col] <- x
        data$M[row, col] <- 1.0
      }
    }
  }
  return(data)
}

# ---------------------------------------------------------------------
# Default loss: mean, second moment and cross moments of each group vs all
# ---------------------------------------------------------------------

# global stats: diag = sum2/count, off-diag = cross/paircount, ones%*%Z = sums
stats_m2cov <- function(data) {
  argtypes(data |> type(MaskedData) |> ref() |> const())
  returntype(StatsM2Cov)
  nrows <- nrow(data$Z)
  ncols <- ncol(data$Z)
  stats |> type(StatsM2Cov)
  stats$GramAll <- t(data$Z) %*% data$Z
  stats$CntAll <- t(data$M) %*% data$M
  SumAll <- matrix(1.0, 1L, nrows) %*% data$Z   # 1 x ncols

  stats$mu_all <- numeric(ncols)
  stats$m2_all <- numeric(ncols)
  for (col in seq_len(ncols)) {
    cc <- stats$CntAll[col, col]
    if (cc > 0.0) {
      stats$mu_all[col] <- SumAll[1L, col] / cc
      stats$m2_all[col] <- stats$GramAll[col, col] / cc
    }
  }
  return(stats)
}

loss_m2cov <- function(data, stats, blocks, perm, lambda_m2, lambda_cov, w, UNUSED) {
  argtypes(
    data |> type(MaskedData) |> ref() |> const(),
    stats |> type(StatsM2Cov) |> ref() |> const(),
    blocks |> type(borrow_vec(int)) |> const(),
    perm |> type(vec(int)) |> ref() |> const(),
    lambda_m2 |> type(double),
    lambda_cov |> type(double),
    w |> type(borrow_vec(double)) |> const(),
    UNUSED |> type(int)
  )
  returntype(double)
  ncols    <- ncol(data$Z)
  n_groups <- max(blocks)

  L <- 0.0
  for (g in seq_len(n_groups)) {
    if (g != UNUSED) {
      rows_g <- perm[blocks == g]          # data rows assigned to group g
      ng     <- length(rows_g)
      if (ng > 0L) {
        Zg <- data$Z[rows_g, ]
        Mg <- data$M[rows_g, ]
        Gg <- t(Zg) %*% Zg
        Cg <- t(Mg) %*% Mg
        Sg <- matrix(1.0, 1L, ng) %*% Zg   # 1 x ncols

        # mean + m2 (matrix diagonal), weighted per covariate
        for (col in seq_len(ncols)) {
          cc <- Cg[col, col]
          if (cc > 0.0) {
            dmu <- Sg[1L, col] / cc - stats$mu_all[col]
            dm2 <- Gg[col, col] / cc - stats$m2_all[col]
            L <- L + w[col] * (dmu * dmu + lambda_m2 * (dm2 * dm2))
          }
        }
        # cross (matrix off-diagonal), weighted by product w[a] * w[b]
        for (a in seq_len(ncols - 1L)) {
          for (b in (a + 1L):ncols) {
            cc <- Cg[a, b]
            if (cc > 0.0) {
              dc <- Gg[a, b] / cc - stats$GramAll[a, b] / stats$CntAll[a, b]
              L <- L + lambda_cov * w[a] * w[b] * (dc * dc)
            }
          }
        }
      }
    }
  }
  return(L)
}

loss_m2cov_vs_all_perm <- function(groups, blocks, perm, lambda_m2, lambda_cov, w, UNUSED) {
  argtypes(
    groups |> type(borrow_mat(double)) |> const(),
    blocks |> type(borrow_vec(int)) |> const(),
    perm |> type(vec(int)),
    lambda_m2 |> type(double),
    lambda_cov |> type(double),
    w |> type(borrow_vec(double)) |> const(),
    UNUSED |> type(int)
  )
  returntype(double)
  data <- na_mask(groups)
  stats <- stats_m2cov(data)
  L <- loss_m2cov(data, stats, blocks, perm, lambda_m2, lambda_cov, w, UNUSED)
  return(L)
}

# Tries every swap of swaps (k-th row = positions i, j in perm) and returns the
# best one if it lowers best_loss by more than tol.
best_swap_m2cov <- function(groups, blocks, perm, swaps, best_loss, tol,
                            lambda_m2, lambda_cov, w, UNUSED) {
  argtypes(
    groups |> type(borrow_mat(double)) |> const(),
    blocks |> type(borrow_vec(int)) |> const(),
    perm |> type(vec(int)),
    swaps |> type(borrow_mat(int)) |> const(),
    best_loss |> type(double),
    tol |> type(double),
    lambda_m2 |> type(double),
    lambda_cov |> type(double),
    w |> type(borrow_vec(double)) |> const(),
    UNUSED |> type(int)
  )
  returntype(SwapResult)
  data <- na_mask(groups)
  stats <- stats_m2cov(data)
  res |> type(SwapResult)
  res$best_loss <- best_loss
  res$best_k <- 0L
  for (k in seq_len(nrow(swaps))) {
    i <- swaps[k, 1L]
    j <- swaps[k, 2L]
    tmp <- perm[[i]]
    perm[[i]] <- perm[[j]]
    perm[[j]] <- tmp
    L <- loss_m2cov(data, stats, blocks, perm, lambda_m2, lambda_cov, w, UNUSED)
    perm[[j]] <- perm[[i]]
    perm[[i]] <- tmp
    if (L < (res$best_loss - tol)) {
      res$best_loss <- L
      res$best_k <- k
    }
  }
  return(res)
}

# ---------------------------------------------------------------------
# Mahalanobis loss: z^T P z with z = group mean - global mean
# ---------------------------------------------------------------------

# global per-column mean (only first moments are needed here)
mean_all <- function(data) {
  argtypes(data |> type(MaskedData) |> ref() |> const())
  returntype(vec(double))
  nrows <- nrow(data$Z)
  ncols <- ncol(data$Z)
  ones_all <- matrix(1.0, 1L, nrows)
  SumAll   <- ones_all %*% data$Z   # 1 x ncols
  CntAll   <- ones_all %*% data$M   # 1 x ncols
  mu_all   <- numeric(ncols)
  for (col in seq_len(ncols)) {
    cc <- CntAll[1L, col]
    if (cc > 0.0) mu_all[col] <- SumAll[1L, col] / cc
  }
  return(mu_all)
}

loss_mahal <- function(data, mu_all, blocks, perm, P, UNUSED) {
  argtypes(
    data |> type(MaskedData) |> ref() |> const(),
    mu_all |> type(vec(double)) |> ref() |> const(),
    blocks |> type(borrow_vec(int)) |> const(),
    perm |> type(vec(int)) |> ref() |> const(),
    P |> type(borrow_mat(double)) |> const(),
    UNUSED |> type(int)
  )
  returntype(double)
  ncols    <- ncol(data$Z)
  n_groups <- max(blocks)

  L <- 0.0
  for (g in seq_len(n_groups)) {
    if (g != UNUSED) {
      rows_g <- perm[blocks == g]
      ng     <- length(rows_g)
      if (ng > 0L) {
        Zg <- data$Z[rows_g, ]
        Mg <- data$M[rows_g, ]
        Sg <- matrix(1.0, 1L, ng) %*% Zg   # 1 x ncols
        Cg <- matrix(1.0, 1L, ng) %*% Mg   # 1 x ncols

        # z = mu_g - mu_all (column vector); empty columns keep mu_g = 0
        z <- matrix(0.0, ncols, 1L)
        for (col in seq_len(ncols)) {
          cc <- Cg[1L, col]
          mu_gj <- 0.0
          if (cc > 0.0) mu_gj <- Sg[1L, col] / cc
          z[col, 1L] <- mu_gj - mu_all[col]
        }

        # d2 = z^T P z
        Pz <- P %*% z          # ncols x 1
        L <- L + sum(z * Pz)
      }
    }
  }
  return(L)
}

loss_mahal_perm <- function(groups, blocks, perm, P, UNUSED) {
  argtypes(
    groups |> type(borrow_mat(double)) |> const(),
    blocks |> type(borrow_vec(int)) |> const(),
    perm |> type(vec(int)),
    P |> type(borrow_mat(double)) |> const(),
    UNUSED |> type(int)
  )
  returntype(double)
  data <- na_mask(groups)
  mu_all <- mean_all(data)
  L <- loss_mahal(data, mu_all, blocks, perm, P, UNUSED)
  return(L)
}

best_swap_mahal <- function(groups, blocks, perm, swaps, best_loss, tol, P, UNUSED) {
  argtypes(
    groups |> type(borrow_mat(double)) |> const(),
    blocks |> type(borrow_vec(int)) |> const(),
    perm |> type(vec(int)),
    swaps |> type(borrow_mat(int)) |> const(),
    best_loss |> type(double),
    tol |> type(double),
    P |> type(borrow_mat(double)) |> const(),
    UNUSED |> type(int)
  )
  returntype(SwapResult)
  data <- na_mask(groups)
  mu_all <- mean_all(data)
  res |> type(SwapResult)
  res$best_loss <- best_loss
  res$best_k <- 0L
  for (k in seq_len(nrow(swaps))) {
    i <- swaps[k, 1L]
    j <- swaps[k, 2L]
    tmp <- perm[[i]]
    perm[[i]] <- perm[[j]]
    perm[[j]] <- tmp
    L <- loss_mahal(data, mu_all, blocks, perm, P, UNUSED)
    perm[[j]] <- perm[[i]]
    perm[[i]] <- tmp
    if (L < (res$best_loss - tol)) {
      res$best_loss <- L
      res$best_k <- k
    }
  }
  return(res)
}
