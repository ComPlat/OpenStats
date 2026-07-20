# --------------------------------------------------------------------------------------------
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

calc_possible_swaps <- function(blocks_i) {
  unique_blocks <- unique(blocks_i)
  indices <- list()
  grid <- combn(unique_blocks, 2)
  for (c in seq_len(ncol(grid))) {
    pair <- grid[, c]
    l_indices <- which(blocks_i == pair[[1L]])
    r_indices <- which(blocks_i == pair[[2L]])
    indices[[length(indices) + 1L]] <- expand.grid(l_indices, r_indices)
  }
  Reduce(rbind, indices)
}

# --------------------------------------------------------------------------------------------
# Loss function
# --------------------------------------------------------------------------------------------
loss_m2cov_vs_all_perm <- function(groups, blocks, perm, lambda_m2, lambda_cov, w, UNUSED) {
  # groups |> type(mat(double)) |> ref()
  # blocks |> type(vec(int))    |> ref()
  # perm   |> type(vec(int))    |> ref()
  # w      |> type(vec(double)) |> ref()   # length ncols, per-covariate weight
  # UNUSED |> type(int)

  nrows    <- length(perm)
  ncols    <- ncol(groups)
  n_groups <- max(blocks)

  # NA mask over rows 1..nrows: Z = values with NA->0, M = present indicator
  Z <- matrix(0.0, nrows, ncols)
  M <- matrix(0.0, nrows, ncols)
  for (row in seq_len(nrows)) {
    for (col in seq_len(ncols)) {
      x <- groups[row, col]
      if (!is.na(x)) {
        Z[row, col] <- x
        M[row, col] <- 1.0
      }
    }
  }

  # global stats: diag = sum2/count, off-diag = cross/paircount, ones%*%Z = sums
  GramAll <- t(Z) %*% Z
  CntAll  <- t(M) %*% M
  SumAll  <- matrix(1.0, 1L, nrows) %*% Z   # 1 x ncols

  mu_all <- numeric(ncols)
  m2_all <- numeric(ncols)
  for (col in seq_len(ncols)) {
    cc <- CntAll[col, col]
    if (cc > 0.0) {
      mu_all[col] <- SumAll[1L, col] / cc
      m2_all[col] <- GramAll[col, col] / cc
    }
  }

  L <- 0.0
  for (g in seq_len(n_groups)) {
    if (g != UNUSED) {
      rows_g <- perm[blocks == g]          # data rows assigned to group g
      ng     <- length(rows_g)
      if (ng > 0L) {
        Zg <- Z[rows_g, ]
        Mg <- M[rows_g, ]
        Gg <- t(Zg) %*% Zg
        Cg <- t(Mg) %*% Mg
        Sg <- matrix(1.0, 1L, ng) %*% Zg   # 1 x ncols

        # mean + m2 (matrix diagonal), weighted per covariate
        for (col in seq_len(ncols)) {
          cc <- Cg[col, col]
          if (cc > 0.0) {
            dmu <- Sg[1L, col] / cc - mu_all[col]
            dm2 <- Gg[col, col] / cc - m2_all[col]
            L <- L + w[col] * (dmu * dmu + lambda_m2 * (dm2 * dm2))
          }
        }
        # cross (matrix off-diagonal), weighted by product w[a] * w[b]
        a <- 1L
        while (a <= (ncols - 1L)) {
          b <- a + 1L
          while (b <= ncols) {
            cc <- Cg[a, b]
            if (cc > 0.0) {
              dc <- Gg[a, b] / cc - GramAll[a, b] / CntAll[a, b]
              L <- L + lambda_cov * w[a] * w[b] * (dc * dc)
            }
            b <- b + 1L
          }
          a <- a + 1L
        }
      }
    }
  }
  return(L)
}


loss_m2cov_args <- function(groups, blocks, perm, lambda_m2, lambda_cov, w, UNUSED) {
  groups |> type(mat(double))
  blocks |> type(vec(int))
  perm |> type(vec(int))
  lambda_m2 |> type(double)
  lambda_cov |> type(double)
  w |> type(vec(double))
  UNUSED |> type(int)
}

one_swap <- function(swaps, perm, best_loss, X, blocks_i,
                     tol = 1e-12, lambda_m2 = 1.0, lambda_cov = 1.0, w, UNUSED, loss_ast) {
  best_k <- NA_integer_
  for (k in seq_len(nrow(swaps))) {
    i <- swaps[k, 1L]
    j <- swaps[k, 2L]
    perm2 <- perm
    perm2[c(i, j)] <- perm2[c(j, i)]
    L <- loss_ast(X, blocks_i, perm2, lambda_m2, lambda_cov, w, UNUSED)
    if (L < (best_loss - tol)) {
      best_loss <- L
      best_k <- k
    }
  }
  list(best_loss = best_loss, best_k = best_k)
}

# ---------------------------------------------------------------------
# Mahalanobis loss (perm-based)
# ---------------------------------------------------------------------
loss_mahal_perm <- function(groups, blocks, perm, P, UNUSED) {
  # groups |> type(mat(double)) |> ref()
  # blocks |> type(vec(int))    |> ref()
  # perm   |> type(vec(int))    |> ref()
  # P      |> type(mat(double)) |> ref()
  # UNUSED |> type(int)

  nrows    <- length(perm)
  ncols    <- ncol(groups)
  n_groups <- max(blocks)

  # NA mask over rows 1..nrows: Z = values with NA->0, M = present indicator
  Z <- matrix(0.0, nrows, ncols)
  M <- matrix(0.0, nrows, ncols)
  for (row in seq_len(nrows)) {
    for (col in seq_len(ncols)) {
      x <- groups[row, col]
      if (!is.na(x)) {
        Z[row, col] <- x
        M[row, col] <- 1.0
      }
    }
  }

  # global per-column mean (only first moments are needed here)
  ones_all <- matrix(1.0, 1L, nrows)
  SumAll   <- ones_all %*% Z   # 1 x ncols
  CntAll   <- ones_all %*% M   # 1 x ncols
  mu_all   <- numeric(ncols)
  for (col in seq_len(ncols)) {
    cc <- CntAll[1L, col]
    if (cc > 0.0) mu_all[col] <- SumAll[1L, col] / cc
  }

  L <- 0.0
  for (g in seq_len(n_groups)) {
    if (g != UNUSED) {
      rows_g <- perm[blocks == g]
      ng     <- length(rows_g)
      if (ng > 0L) {
        Zg <- Z[rows_g, ]
        Mg <- M[rows_g, ]
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

loss_mahal_args <- function(groups, blocks, perm, P, UNUSED) {
  groups |> type(mat(double))
  blocks |> type(vec(int))
  perm   |> type(vec(int))
  P      |> type(mat(double))
  UNUSED |> type(int)
}

one_swap_mahal <- function(swaps, perm, best_loss, X, blocks_i, P,
                           tol = 1e-12, UNUSED, loss_ast) {
  best_k <- NA_integer_
  for (k in seq_len(nrow(swaps))) {
    i <- swaps[k, 1L]
    j <- swaps[k, 2L]
    perm2 <- perm
    perm2[c(i, j)] <- perm2[c(j, i)]
    L <- loss_ast(X, blocks_i, perm2, P, UNUSED)
    if (L < (best_loss - tol)) {
      best_loss <- L
      best_k <- k
    }
  }
  list(best_loss = best_loss, best_k = best_k)
}

# ---------------------------------------------------------------------
# Optimizer
# ---------------------------------------------------------------------
greedy_optimize <- function(groups_inp, blocks, perm_init,
                            max_iter = 200L, tol = 1e-12, verbose = FALSE,
                            ridge = 1e-8, loss_ast, loss_function, w = NULL,
                            lambda_m2 = 1.0, lambda_cov = 1.0) {

  X <- as.matrix(normalize_df(groups_inp))

  C <- stats::cov(X)
  P <- solve(C + diag(ridge, ncol(X)))

  if (is.null(w)) w <- rep(1.0, ncol(X))

  blocks_i <- as.integer(factor(blocks))
  UNUSED <- -1L
  if (any(blocks == "UNUSED")) {
    UNUSED <- blocks_i[which(blocks == "UNUSED")[1L]]
  }

  swaps <- as.matrix(calc_possible_swaps(blocks_i))
  storage.mode(swaps) <- "integer"

  perm <- as.integer(perm_init)
  if (loss_function == "Default") {
    cur <- loss_ast(X, blocks_i, perm, lambda_m2, lambda_cov, w, UNUSED)
  } else if (loss_function == "Mahalanobis") {
    cur <- loss_ast(X, blocks_i, perm, P, UNUSED)
  }

  for (it in seq_len(max_iter)) {
    if (loss_function == "Default") {
      step <- one_swap(swaps, perm, cur, X, blocks_i, tol, lambda_m2, lambda_cov, w, UNUSED, loss_ast)
    } else if (loss_function == "Mahalanobis") {
      step <- one_swap_mahal(swaps, perm, cur, X, blocks_i, P, tol = tol, UNUSED = UNUSED, loss_ast = loss_ast)
    }
    if (is.na(step$best_k)) {
      if (verbose) cat("no improvement at iter", it, "-> stop\n")
      break
    }
    i <- swaps[step$best_k, 1L]
    j <- swaps[step$best_k, 2L]
    perm[c(i, j)] <- perm[c(j, i)]
    cur <- step$best_loss
    if (verbose) cat("iter", it, "loss:", cur, "swap:", i, j, "\n")
  }

  list(
    perm = perm,
    assigned = groups_inp[perm, , drop = FALSE],
    blocks = blocks,
    loss = cur,
    P = P
  )
}

random_finite_assign <- function(seed, groups, design, max_iter = 50L,
                                 ridge = 1e-8, loss_function = "Default",
                                 verbose = FALSE, ids = NULL, w = NULL,
                                 lambda_m2 = 1.0, lambda_cov = 1.0) {
  stopifnot(
    is.numeric(seed), length(seed) == 1L,
    is.data.frame(groups),
    is.data.frame(design),
    is.numeric(max_iter), length(max_iter) == 1L,
    is.null(ids) || (length(ids) == nrow(groups)),
    is.numeric(ridge), length(ridge) == 1L,
    loss_function %in% c("Default", "Mahalanobis"),
    is.null(w) || (is.numeric(w) && length(w) == ncol(groups)),
    is.numeric(lambda_m2), length(lambda_m2) == 1L,
    is.numeric(lambda_cov), length(lambda_cov) == 1L
  )

  if (loss_function == "Mahalanobis") {
    loss_ast <- ast2ast::translate(loss_mahal_perm, loss_mahal_args)
  } else if (loss_function == "Default") {
    loss_ast <- ast2ast::translate(loss_m2cov_vs_all_perm, loss_m2cov_args)
  }

  blocks <- interaction(design) |> as.factor()

  set.seed(seed)
  N <- nrow(groups)

  if (N > length(blocks)) { # surplus
    stopifnot("You cannot use UNUSED as blocks id" = !("UNUSED" %in% unique(blocks)))
    blocks <- c(blocks, rep("UNUSED", N - length(blocks)))
  }

  perm_init <- sample.int(N)
  res <- greedy_optimize(groups, blocks, perm_init,
    max_iter = max_iter, tol = 1e-12, verbose = verbose, ridge = ridge,
    loss_ast = loss_ast, loss_function = loss_function, w = w,
    lambda_m2 = lambda_m2, lambda_cov = lambda_cov)

  res$unit_index <- res$perm
  if (!is.null(ids)) {
    res$unit_id <- ids[res$perm]
  }

  res$assigned$unit_index <- res$unit_index
  if (!is.null(ids)) {
    res$assigned$unit_id <- res$unit_id
  }

  keep <- which(blocks != "UNUSED")
  res$UNUSED <- res$assigned[which(blocks == "UNUSED"), , drop = FALSE]

  res$perm <- res$perm[keep]
  res$unit_index <- res$unit_index[keep]
  if (!is.null(ids)) res$unit_id <- res$unit_id[keep]

  res$assigned <- res$assigned[keep, , drop = FALSE]
  res$blocks <- res$blocks[keep]

  res
}
