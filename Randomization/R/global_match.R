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
loss_m2cov_vs_all_perm <- function(groups, blocks, perm, lambda_m2, lambda_cov, UNUSED) {
  # groups |> type(mat(double)) |> ref()
  # blocks |> type(vec(int))    |> ref()
  # perm   |> type(vec(int))    |> ref()
  # UNUSED |> type(int)

  nrows <- length(perm)
  ncols <- ncol(groups)

  # ---------------- Helpers -----------------------------------
  calc_n_groups <- fn(
    f_args = function(nrows, blocks) {
      nrows  |> type(int)
      blocks |> type(vec(int)) |> ref()
    },
    return_value = type(int),
    block = function(nrows, blocks) {
      n_groups <- 0L
      for (row in seq_len(nrows)) {
        bi <- blocks[row]
        if (bi > n_groups) n_groups <- bi
      }
      return(n_groups)
    }
  )
  n_groups <- calc_n_groups(nrows, blocks)

  calc_n_pairs <- fn(
    f_args = function(ncols) {
      ncols |> type(int)
    },
    return_value = type(int),
    block = function(ncols) {
      n_pairs |> type(int) <- (ncols * (ncols - 1L)) / 2L
      if (n_pairs == 0L) n_pairs <- 1L
      return(n_pairs)
    }
  )
  n_pairs <- calc_n_pairs(ncols)

  safe_mean <- fn(
    f_args = function(s, cnt) {
      s   |> type(double)
      cnt |> type(int)
    },
    return_value = type(double),
    block = function(s, cnt) {
      if (cnt > 0L) return(s / cnt)
      return(0.0)
    }
  )

  calc_mu_and_mu2 <- fn(
    f_args = function(nrows, ncols, groups) {
      nrows  |> type(int)
      ncols  |> type(int)
      groups |> type(mat(double)) |> ref()
    },
    return_value = type(mat(double)),
    block = function(nrows, ncols, groups) {
      res <- matrix(0.0, ncols, 2L)
      for (col in seq_len(ncols)) {
        s <- 0.0
        s2 <- 0.0
        cnt <- 0L
        for (row in seq_len(nrows)) {
          x <- groups[row, col]
          if (!is.na(x)) {
            s <- s + x
            s2 <- s2 + x * x
            cnt <- cnt + 1L
          }
        }
        res[col, 1L] <- safe_mean(s,  cnt)
        res[col, 2L] <- safe_mean(s2, cnt)
      }
      return(res)
    }
  )
  mus <- calc_mu_and_mu2(nrows, ncols, groups)
  mu_all <- c(mus[, 1L])
  m2_all <- c(mus[, 2L])

  calc_cross_all <- fn(
    f_args = function(nrows, ncols, n_pairs, groups) {
      nrows   |> type(int)
      ncols   |> type(int)
      n_pairs |> type(int)
      groups  |> type(mat(double)) |> ref()
    },
    return_value = type(vec(double)),
    block = function(nrows, ncols, n_pairs, groups) {
      cross_all <- numeric(n_pairs)
      t <- 1L
      a <- 1L
      while (a <= (ncols - 1L)) {
        b <- a + 1L
        while (b <= ncols) {
          sxy <- 0.0
          cnt <- 0L
          for (row in seq_len(nrows)) {
            xa <- groups[row, a]
            xb <- groups[row, b]
            if (!is.na(xa) && !is.na(xb)) {
              sxy <- sxy + xa * xb
              cnt <- cnt + 1L
            }
          }
          cross_all[t] <- safe_mean(sxy, cnt)
          t <- t + 1L
          b <- b + 1L
        }
        a <- a + 1L
      }
      return(cross_all)
    }
  )
  cross_all <- calc_cross_all(nrows, ncols, n_pairs, groups)

  # ---------------- Accumulators --------------------------------------
  accumulate_one_ref <- fn(
    f_args = function(nrows, ncols, blocks, groups, perm, sum_g, sum2_g, cnt_g) {
      nrows  |> type(int)
      ncols  |> type(int)
      blocks |> type(vec(int))    |> ref()
      groups |> type(mat(double)) |> ref()
      perm   |> type(vec(int))    |> ref()
      sum_g  |> type(vec(double)) |> ref()
      sum2_g |> type(vec(double)) |> ref()
      cnt_g  |> type(vec(int))    |> ref()
    },
    return_value = type(void),
    block = function(nrows, ncols, blocks, groups, perm, sum_g, sum2_g, cnt_g) {
      for (sidx in seq_len(nrows)) {
        g <- blocks[sidx]
        r <- perm[sidx]
        g0 <- (g - 1L) * ncols
        for (col in seq_len(ncols)) {
          x <- groups[r, col]
          if (!is.na(x)) {
            idx <- g0 + col
            sum_g[idx]  <- sum_g[idx]  + x
            sum2_g[idx] <- sum2_g[idx] + x * x
            cnt_g[idx]  <- cnt_g[idx]  + 1L
          }
        }
      }
    }
  )

  accumulate_two_ref <- fn(
    f_args = function(nrows, ncols, n_pairs, blocks, groups, perm, sumc_g, cntc_g) {
      nrows   |> type(int)
      ncols   |> type(int)
      n_pairs |> type(int)
      blocks  |> type(vec(int))    |> ref()
      groups  |> type(mat(double)) |> ref()
      perm    |> type(vec(int))    |> ref()
      sumc_g  |> type(vec(double)) |> ref()
      cntc_g  |> type(vec(int))    |> ref()
    },
    return_value = type(void),
    block = function(nrows, ncols, n_pairs, blocks, groups, perm, sumc_g, cntc_g) {
      for (sidx in seq_len(nrows)) {
        g <- blocks[sidx]
        r <- perm[sidx]
        g0 <- (g - 1L) * n_pairs

        t <- 1L
        a <- 1L
        while (a <= (ncols - 1L)) {
          b <- a + 1L
          xa <- groups[r, a]
          while (b <= ncols) {
            xb <- groups[r, b]
            if (!is.na(xa) && !is.na(xb)) {
              idx <- g0 + t
              sumc_g[idx] <- sumc_g[idx] + xa * xb
              cntc_g[idx] <- cntc_g[idx] + 1L
            }
            t <- t + 1L
            b <- b + 1L
          }
          a <- a + 1L
        }
      }
    }
  )

  # allocate typed accumulators
  sum_g  <- numeric(n_groups * ncols)
  sum2_g <- numeric(n_groups * ncols)
  cnt_g  <- integer(n_groups * ncols)

  sumc_g <- numeric(n_groups * n_pairs)
  cntc_g <- integer(n_groups * n_pairs)

  accumulate_one_ref(nrows, ncols, blocks, groups, perm, sum_g, sum2_g, cnt_g)
  accumulate_two_ref(nrows, ncols, n_pairs, blocks, groups, perm, sumc_g, cntc_g)

  # ---------------- Loss (separate) ---------------------------
  calc_loss_from_accs <- fn(
    f_args = function(n_groups, ncols, n_pairs,
                      UNUSED, lambda_m2, lambda_cov,
                      sum_g, sum2_g, cnt_g,
                      sumc_g, cntc_g,
                      mu_all, m2_all, cross_all) {
      n_groups   |> type(int)
      ncols      |> type(int)
      n_pairs    |> type(int)
      UNUSED     |> type(int)
      lambda_m2  |> type(double)
      lambda_cov |> type(double)

      sum_g   |> type(vec(double)) |> ref()
      sum2_g  |> type(vec(double)) |> ref()
      cnt_g   |> type(vec(int))    |> ref()

      sumc_g  |> type(vec(double)) |> ref()
      cntc_g  |> type(vec(int))    |> ref()

      mu_all    |> type(vec(double)) |> ref()
      m2_all    |> type(vec(double)) |> ref()
      cross_all |> type(vec(double)) |> ref()
    },
    return_value = type(double),
    block = function(n_groups, ncols, n_pairs,
                     UNUSED, lambda_m2, lambda_cov,
                     sum_g, sum2_g, cnt_g,
                     sumc_g, cntc_g,
                     mu_all, m2_all, cross_all) {
      L <- 0.0
      for (g in seq_len(n_groups)) {
        if (g != UNUSED) {
          # mean + m2
          for (col in seq_len(ncols)) {
            idx <- (g - 1L) * ncols + col
            c <- cnt_g[idx]
            if (c > 0L) {
              mu_g <- sum_g[idx] / c
              m2_g <- sum2_g[idx] / c
              dmu <- mu_g - mu_all[col]
              dm2 <- m2_g - m2_all[col]
              L <- L + dmu * dmu + lambda_m2 * (dm2 * dm2)
            }
          }

          # cross
          for (p in seq_len(n_pairs)) {
            idx <- (g - 1L) * n_pairs + p
            c <- cntc_g[idx]
            if (c > 0L) {
              cg <- sumc_g[idx] / c
              dc <- cg - cross_all[p]
              L <- L + lambda_cov * (dc * dc)
            }
          }
        }
      }
      return(L)
    }
  )

  L <- calc_loss_from_accs(
    n_groups, ncols, n_pairs,
    UNUSED, lambda_m2, lambda_cov,
    sum_g, sum2_g, cnt_g,
    sumc_g, cntc_g,
    mu_all, m2_all, cross_all
  )

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

# ---------------------------------------------------------------------
# Mahalanobis loss (perm-based)
# ---------------------------------------------------------------------
loss_mahal_perm <- function(groups, blocks, perm, P, UNUSED) {
  # groups |> type(mat(double)) |> ref()
  # blocks |> type(vec(int))    |> ref()
  # perm   |> type(vec(int))    |> ref()
  # P      |> type(mat(double)) |> ref()
  # UNUSED |> type(int)

  nrows <- length(perm)
  ncols <- ncol(groups)

  # ---------------- Helpers -----------------------------------
  calc_n_groups <- fn(
    f_args = function(nrows, blocks) {
      nrows  |> type(int)
      blocks |> type(vec(int)) |> ref()
    },
    return_value = type(int),
    block = function(nrows, blocks) {
      n_groups <- 0L
      for (row in seq_len(nrows)) {
        bi <- blocks[row]
        if (bi > n_groups) n_groups <- bi
      }
      return(n_groups)
    }
  )
  n_groups <- calc_n_groups(nrows, blocks)

  safe_mean <- fn(
    f_args = function(s, cnt) {
      s   |> type(double)
      cnt |> type(int)
    },
    return_value = type(double),
    block = function(s, cnt) {
      if (cnt > 0L) return(s / cnt)
      return(0.0)
    }
  )

  calc_mu_all <- fn(
    f_args = function(nrows, ncols, groups) {
      nrows  |> type(int)
      ncols  |> type(int)
      groups |> type(mat(double)) |> ref()
    },
    return_value = type(vec(double)),
    block = function(nrows, ncols, groups) {
      mu_all <- numeric(ncols)
      for (col in seq_len(ncols)) {
        s <- 0.0
        cnt <- 0L
        for (row in seq_len(nrows)) {
          x <- groups[row, col]
          if (!is.na(x)) {
            s <- s + x
            cnt <- cnt + 1L
          }
        }
        mu_all[col] <- safe_mean(s, cnt)
      }
      return(mu_all)
    }
  )
  mu_all <- calc_mu_all(nrows, ncols, groups)

  # ---------------- Accumulator ----------------------
  accumulate_sum_cnt_ref <- fn(
    f_args = function(nrows, ncols, blocks, groups, perm, sum_g, cnt_g) {
      nrows  |> type(int)
      ncols  |> type(int)
      blocks |> type(vec(int))    |> ref()
      groups |> type(mat(double)) |> ref()
      perm   |> type(vec(int))    |> ref()
      sum_g  |> type(vec(double)) |> ref()
      cnt_g  |> type(vec(int))    |> ref()
    },
    return_value = type(int),
    block = function(nrows, ncols, blocks, groups, perm, sum_g, cnt_g) {
      sidx <- 1L
      while (sidx <= nrows) {
        g <- blocks[sidx]
        r <- perm[sidx]
        g0 <- (g - 1L) * ncols
        for (col in seq_len(ncols)) {
          x <- groups[r, col]
          if (!is.na(x)) {
            idx <- g0 + col
            sum_g[idx] <- sum_g[idx] + x
            cnt_g[idx] <- cnt_g[idx] + 1L
          }
        }
        sidx <- sidx + 1L
      }
      return(0L)
    }
  )

  sum_g <- numeric(n_groups * ncols)
  cnt_g <- integer(n_groups * ncols)
  dummy <- accumulate_sum_cnt_ref(nrows, ncols, blocks, groups, perm, sum_g, cnt_g)

  # ---------------- Loss ---------------------------
  calc_loss_mahal_from_accs <- fn(
    f_args = function(n_groups, ncols, UNUSED, sum_g, cnt_g, mu_all, P) {
      n_groups |> type(int)
      ncols    |> type(int)
      UNUSED   |> type(int)

      sum_g  |> type(vec(double)) |> ref()
      cnt_g  |> type(vec(int))    |> ref()
      mu_all |> type(vec(double)) |> ref()
      P      |> type(mat(double)) |> ref()
    },
    return_value = type(double),
    block = function(n_groups, ncols, UNUSED, sum_g, cnt_g, mu_all, P) {
      L <- 0.0

      for (g in seq_len(n_groups)) {
        if (g != UNUSED) {
          # z = mu_g - mu_all
          z <- numeric(ncols)
          for (col in seq_len(ncols)) {
            idx <- (g - 1L) * ncols + col
            c <- cnt_g[idx]
            mu_gj <- 0.0
            if (c > 0L) mu_gj <- sum_g[idx] / c
            z[col] <- mu_gj - mu_all[col]
          }

          # w = P %*% z
          w <- numeric(ncols)
          for (i in seq_len(ncols)) {
            acc <- 0.0
            for (j in seq_len(ncols)) {
              acc <- acc + P[i, j] * z[j]
            }
            w[i] <- acc
          }

          # d2 = z^T w
          d2 <- 0.0
          for (j in seq_len(ncols)) {
            d2 <- d2 + z[j] * w[j]
          }

          L <- L + d2
        }
      }

      return(L)
    }
  )

  L <- calc_loss_mahal_from_accs(n_groups, ncols, UNUSED, sum_g, cnt_g, mu_all, P)
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
                            max_iter = 200L, tol = 1e-12, verbose = TRUE,
                            ridge = 1e-8, loss_ast, loss_function) {

  X <- as.matrix(normalize_df(groups_inp))

  C <- stats::cov(X)
  P <- solve(C + diag(ridge, ncol(X)))

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
  if (loss_function == "Default") {
    cur <- loss_ast(X, blocks_i, perm, lambda_m2, lambda_cov, UNUSED)
  } else if (loss_function == "Mahalanobis") {
    cur <- loss_ast(X, blocks_i, perm, P, UNUSED)
  }

  for (it in seq_len(max_iter)) {
    if (loss_function == "Default") {
      step <- one_swap(swaps, perm, cur, X, blocks_i, tol, lambda_m2, lambda_cov, UNUSED, loss_ast)
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

random_finite_assign <- function(seed, groups, design, max_iter = 50L, ridge = 1e-8, loss_function = "Default", verbose = TRUE, ids = NULL) {
  stopifnot(
    is.numeric(seed), length(seed) == 1L,
    is.data.frame(groups),
    is.data.frame(design),
    is.numeric(max_iter), length(max_iter) == 1L,
    is.null(ids) || (length(ids) == nrow(groups)),
    is.numeric(ridge), length(ridge) == 1L,
    loss_function %in% c("Default", "Mahalanobis")
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
    max_iter = max_iter, tol = 1e-12, verbose = verbose, ridge = ridge, loss_ast, loss_function)

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
