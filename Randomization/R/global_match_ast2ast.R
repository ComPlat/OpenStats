# Loss function
# --------------------------------------------------------------------------------------------
loss_m2cov_vs_all_perm <- function(groups, blocks, perm, lambda_m2, lambda_cov, UNUSED) {
  nrows <- length(perm)
  ncols <- dim(groups)[2L]

  # --- number of groups = max(block ids) ---
  n_groups <- 0L
  i <- 1L
  while (i <= nrows) {
    bi <- blocks[i]
    if (bi > n_groups) {
      n_groups <- bi
    }
    i <- i + 1L
  }
  # --- number of feature pairs (a,b) with a<b ---
  # (ncols 2) ncols over 2; number of pairs which can be formed with the content of groups
  n_pairs <- (ncols * (ncols - 1L)) / 2L
  if (n_pairs == 0L) {
    n_pairs <- 1L # keep vectors non-empty for ast2ast/static reasons; loop won't run anyway
  }

  # --------------------------------------------------------------------------
  # Global (perm-invariant) moments
  #   mu_all[j]    = E[groups[,j]]
  #   m2_all[j]    = E[groups[,j]^2]
  #   cross_all[t] = E[groups[,a]*groups[,b]] for pairs (a<b) packed into t=1..n_pairs
  # --------------------------------------------------------------------------
  mu_all <- numeric(ncols)
  m2_all <- numeric(ncols)

  j <- 1L
  while (j <= ncols) {
    s <- 0.0
    s2 <- 0.0
    cnt <- 0L
    i <- 1L
    while (i <= nrows) {
      x <- groups[i, j]
      if (!is.na(x)) {
        s <- s + x
        s2 <- s2 + x * x
        cnt <- cnt + 1L
      }
      i <- i + 1L
    }
    mu_all[j] <- s / cnt
    m2_all[j] <- s2 / cnt
    j <- j + 1L
  }

  cross_all <- numeric(n_pairs)
  t <- 1L
  a <- 1L
  while (a <= (ncols - 1L)) {
    b <- a + 1L
    while (b <= ncols) {
      sxy <- 0.0
      cnt <- 0L
      i <- 1L
      while (i <= nrows) {
        xa <- groups[i, a]
        xb <- groups[i, b]
        if (!is.na(xa) && !is.na(xb)) {
          sxy <- sxy + xa * xb
          cnt <- cnt + 1L
        }
        i <- i + 1L
      }
      cross_all[t] <- sxy / cnt
      t <- t + 1L
      b <- b + 1L
    }
    a <- a + 1L
  }

  # --------------------------------------------------------------------------
  # Per-group accumulators over the *assignment* implied by perm:
  #   slot s belongs to group blocks[s], but uses row r=perm[s] from X
  # We accumulate:
  #   sum_g[g,j], sum2_g[g,j], cnt_g[g,j]
  #   sumc_g[g,t], cntc_g[g,t]
  # Flattened as 1D arrays for speed: idx = (g-1)*stride + offset
  # --------------------------------------------------------------------------
  sum_g  <- numeric(n_groups * ncols)
  sum2_g <- numeric(n_groups * ncols)
  cnt_g  <- integer(n_groups * ncols)
  sumc_g <- numeric(n_groups * n_pairs)
  cntc_g <- integer(n_groups * n_pairs)

  sidx <- 1L
  while (sidx <= nrows) {
    g <- blocks[sidx]   # 1..n_groups
    r <- perm[sidx]     # 1..N (row in groups)

    # per-column sums
    j <- 1L
    while (j <= ncols) {
      x <- groups[r, j]
      if (!is.na(x)) {
        idx <- (g - 1L) * ncols + j
        sum_g[idx]  <- sum_g[idx]  + x
        sum2_g[idx] <- sum2_g[idx] + x * x
        cnt_g[idx]  <- cnt_g[idx]  + 1L
      }
      j <- j + 1L
    }

    # per-pair cross sums
    t <- 1L
    a <- 1L
    while (a <= (ncols - 1L)) {
      b <- a + 1L
      xa <- groups[r, a]
      while (b <= ncols) {
        xb <- groups[r, b]
        if (!is.na(xa) && !is.na(xb)) {
          idx <- (g - 1L) * n_pairs + t
          sumc_g[idx] <- sumc_g[idx] + xa * xb
          cntc_g[idx] <- cntc_g[idx] + 1L
        }
        t <- t + 1L
        b <- b + 1L
      }
      a <- a + 1L
    }
    sidx <- sidx + 1L
  }

  # --------------------------------------------------------------------------
  # Loss: sum over groups of squared residuals to global targets
  # --------------------------------------------------------------------------
  L <- 0.0
  g <- 1L
  while (g <= n_groups) {

    if (g != UNUSED) {

      # mean + m2 terms
      j <- 1L
      while (j <= ncols) {
        idx <- (g - 1L) * ncols + j
        c <- cnt_g[idx]
        if (c > 0L) {
          mu_g <- sum_g[idx] / c
          m2_g <- sum2_g[idx] / c
          dmu <- mu_g - mu_all[j]
          dm2 <- m2_g - m2_all[j]
          L <- L + dmu * dmu + lambda_m2 * (dm2 * dm2)
        }
        j <- j + 1L
      }

      # cross term
      t <- 1L
      while (t <= n_pairs) {
        idx <- (g - 1L) * n_pairs + t
        c <- cntc_g[idx]
        if (c > 0L) {
          cg <- sumc_g[idx] / c
          dc <- cg - cross_all[t]
          L <- L + lambda_cov * (dc * dc)
        }
        t <- t + 1L
      }

    }
      g <- g + 1L
  }

  return(L)
}

loss_m2cov_args <- function(groups, blocks, perm, lambda_m2, lambda_cov, UNUSED) {
  groups |> type(mat(double))
  blocks |> type(vec(int))
  perm |> type(vec(int))
  lambda_m2 |> type(double)
  lambda_cov |> type(double)
  UNUSED |> type(int)
}

one_swap <- function(swaps, perm, best_loss, X, blocks_i,
                     tol = 1e-12, lambda_m2 = 1.0, lambda_cov = 1.0, UNUSED, loss_ast) {
  best_k <- NA_integer_
  for (k in seq_len(nrow(swaps))) {
    i <- swaps[k, 1L]
    j <- swaps[k, 2L]
    perm2 <- perm
    perm2[c(i, j)] <- perm2[c(j, i)]
    L <- loss_ast(X, blocks_i, perm2, lambda_m2, lambda_cov, UNUSED)
    if (L < (best_loss - tol)) {
      best_loss <- L
      best_k <- k
    }
  }
  list(best_loss = best_loss, best_k = best_k)
}

# Optimizer
# --------------------------------------------------------------------------------------------
greedy_optimize <- function(groups_inp, blocks, perm_init, max_iter = 200L, tol = 1e-12, verbose = TRUE) {
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
  loss_ast <- ast2ast::translate(loss_m2cov_vs_all_perm, loss_m2cov_args)
  X <- as.matrix(normalize_df(groups_inp))

  blocks_i <- as.integer(factor(blocks))
  UNUSED <- -1L
  if (any(blocks == "UNUSED")) {
    UNUSED <- blocks_i[which(blocks == "UNUSED")[1L]]
  }

  swaps <- as.matrix(calc_possible_swaps(blocks_i))
  storage.mode(swaps) <- "integer"

  perm <- as.integer(perm_init)
  lambda_m2 <- 1.0
  lambda_cov <- 1.0
  cur <- loss_ast(X, blocks_i, perm, lambda_m2, lambda_cov, UNUSED)

  for (it in seq_len(max_iter)) {
    step <- one_swap(swaps, perm, cur, X, blocks_i, tol, lambda_m2, lambda_cov, UNUSED, loss_ast)
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
    loss = cur
  )
}

random_finite_assign <- function(seed, groups, design, max_iter = 50L) {
  stopifnot(
    is.numeric(seed),
    length(seed) == 1L,
    is.data.frame(groups),
    is.data.frame(design),
    is.numeric(max_iter),
    length(max_iter) == 1L
  )
  blocks <- interaction(design) |> as.factor()
  set.seed(seed)
  N <- nrow(groups)
  if (N > length(blocks)) { # surplus
    stopifnot("You cannot use UNUSED as blocks id" = !("UNUSED" %in% unique(blocks)))
    unused <- rep("UNUSED", N - length(blocks))
    blocks <- c(blocks, unused)
  }
  perms <- sample.int(N)
  res <- greedy_optimize(groups, blocks, perms, max_iter = max_iter)
  keep <- which(blocks != "UNUSED")
  res$perm <- res$perm[keep]
  res$UNUSED <- res$assigned[which(blocks == "UNUSED"), , drop = FALSE]
  res$assigned <- res$assigned[keep, , drop = FALSE]
  res$blocks <- res$blocks[keep]
  res
}
