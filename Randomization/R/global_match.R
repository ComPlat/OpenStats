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

# ---------------------------------------------------------------------
# Optimizer
# loss_m2cov_vs_all_perm, loss_mahal_perm, best_swap_m2cov and
# best_swap_mahal are compiled from inst/tools/global_match.R
# ---------------------------------------------------------------------
greedy_optimize <- function(groups_inp, blocks, perm_init,
                            max_iter = 200L, tol = 1e-12, verbose = FALSE,
                            ridge = 1e-8, loss_function, w = NULL,
                            lambda_m2 = 1.0, lambda_cov = 1.0) {

  X <- as.matrix(normalize_df(groups_inp))

  C <- stats::cov(X)
  P <- solve(C + diag(ridge, ncol(X)))

  # NOTE: borrow_vec(double) in the compiled code needs a double vector, no integer
  w <- if (is.null(w)) rep(1.0, ncol(X)) else as.double(w)

  blocks_i <- as.integer(factor(blocks))
  UNUSED <- -1L
  if (any(blocks == "UNUSED")) {
    UNUSED <- blocks_i[which(blocks == "UNUSED")[1L]]
  }

  swaps <- as.matrix(calc_possible_swaps(blocks_i))
  storage.mode(swaps) <- "integer"

  perm <- as.integer(perm_init)
  if (loss_function == "Default") {
    cur <- loss_m2cov_vs_all_perm(X, blocks_i, perm, lambda_m2, lambda_cov, w, UNUSED)
  } else if (loss_function == "Mahalanobis") {
    cur <- loss_mahal_perm(X, blocks_i, perm, P, UNUSED)
  }

  for (it in seq_len(max_iter)) {
    if (loss_function == "Default") {
      step <- best_swap_m2cov(X, blocks_i, perm, swaps, cur, tol, lambda_m2, lambda_cov, w, UNUSED)
    } else if (loss_function == "Mahalanobis") {
      step <- best_swap_mahal(X, blocks_i, perm, swaps, cur, tol, P, UNUSED)
    }
    if (step$best_k == 0L) {
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
    loss_function = loss_function, w = w,
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
