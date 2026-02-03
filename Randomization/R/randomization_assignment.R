# -------------------------------------------------------------------------------
# Draw from one finite groups
# -------------------------------------------------------------------------------
calc_stratum <- function(groups, n_quantiles) {
  breaks <- quantile(groups, probs = seq(0, 1, length.out = n_quantiles)) |> unique()
  stratum <- cut(
    groups,
    breaks = breaks,
    include.lowest = TRUE,
    labels = FALSE
  )
  stratum
}
calc_ratios <- function(groups, n_quantiles) {
  stratum <- calc_stratum(groups, n_quantiles)
  tbl_str <- table(stratum)
  res <- vapply(tbl_str, \(g) g / sum(tbl_str), numeric(1))
  res
}

groups_new <- function(groups, n_quantiles, ids) {
  if (!is.numeric(groups)) stop("Finite groups must be numeric")
  self <- new.env(parent = emptyenv())
  self$groups <- groups
  self$stratum <- calc_stratum(self$groups, n_quantiles)
  self$stratum_groups <- sort(unique(self$stratum))
  self$ids <- ids
  G <- new.env(parent = self)
  G$draw <- function(n_per_group) {
    if (is.null(names(n_per_group))) stop("n per group has to be a named vector")
    res <- c()
    for (s in self$stratum_groups) {
      sub_indices <- which(self$stratum == s)
      sub <- self$groups[sub_indices]
      n <- n_per_group[[as.character(s)]]
      if (length(sub) < n) stop("Not enough elements in stratum ", s)
      if (n > 0) {
        local_indices <- sample(seq_len(length(sub)), n, replace = FALSE)
        global_indices <- sub_indices[local_indices]
        res <- c(res, self$ids[global_indices])
        self$groups <- self$groups[-global_indices]
        self$stratum <- self$stratum[-global_indices]
        self$ids <- self$ids[-global_indices]
      }
    }
    return(res)
  }
  G
}

# -------------------------------------------------------------------------------
# Draw from multiple finite groups at the same time
# -------------------------------------------------------------------------------
calc_stratum_nd <- function(groups, n_quantiles, min_cell_size = 2L, min_bins = 2L) {
  X <- as.data.frame(groups)
  if (!all(vapply(X, is.numeric, logical(1)))) {
    stop("All finite-group covariates must be numeric")
  }

  p <- ncol(X)
  if (length(n_quantiles) == 1L) n_quantiles <- rep.int(n_quantiles, p)
  if (length(n_quantiles) != p) stop("n_quantiles must be length 1 or ncol(groups)")
  n_quantiles <- as.integer(n_quantiles)

  bin_1d <- function(covariate, n_bins) {
    n_bins <- max(as.integer(n_bins), 1L)
    probs <- seq(0, 1, length.out =  n_bins + 1L)
    break_points <- unique(quantile(covariate, probs = probs, na.rm = TRUE, type = 7))
    while (length(break_points) < 2L && n_bins > 1L) {
      n_bins <-  n_bins - 1L
      probs <- seq(0, 1, length.out = n_bins + 1L)
      break_points <- unique(quantile(covariate, probs = probs, na.rm = TRUE, type = 7))
    }
    if (length(break_points) < 2L) return(rep.int(1L, length(covariate)))
    cut(covariate, breaks = break_points, include.lowest = TRUE, labels = FALSE)
  }

  repeat {
    bins <- lapply(seq_len(p), function(j) bin_1d(X[[j]], n_quantiles[[j]]))
    stratum <- do.call(interaction, c(bins, list(drop = TRUE, lex.order = TRUE)))
    cell_sizes <- as.integer(table(stratum))
    if (length(cell_sizes) == 0L) return(factor(rep.int(1L, nrow(X))))
    if (min(cell_sizes) >= min_cell_size) return(stratum)
    if (all(n_quantiles <= min_bins)) return(stratum)
    j <- which.max(n_quantiles)
    n_quantiles[[j]] <- max(min_bins, n_quantiles[[j]] - 1L)
  }
}
calc_ratios_nd <- function(groups, n_quantiles) {
  stratum <- calc_stratum_nd(groups, n_quantiles)
  tbl <- table(stratum)
  vapply(tbl, \(g) g / sum(tbl), numeric(1))
}
groups_new_nd <- function(groups, n_quantiles, ids) {
  X <- as.data.frame(groups)
  self <- new.env(parent = emptyenv())
  self$X <- X
  self$stratum <- calc_stratum_nd(self$X, n_quantiles)
  self$stratum_groups <- levels(self$stratum)
  self$ids <- ids
  G <- new.env(parent = self)

  G$draw <- function(n_per_group) {
    plan_strata <- names(n_per_group)
    if (is.null(plan_strata)) stop("n_per_group must be named")

    res_idx <- integer(0)

    for (s in plan_strata) {
      n <- n_per_group[[s]]
      if (is.null(n)) stop("Missing n_per_group entry for stratum: ", s)

      sub_indices <- which(self$stratum == s)

      if (length(sub_indices) == 0 && n > 0) {
        stop("Planned stratum ", s, " needs n=", n,
          " but there are 0 elements left in that stratum.")
      }
      if (length(sub_indices) < n) {
        stop("Not enough elements in stratum ", s,
          ": need ", n, ", have ", length(sub_indices))
      }

      if (n > 0) {
        picked <- sample(sub_indices, n, replace = FALSE)
        res_idx <- c(res_idx, picked)
      }
    }

    res <- self$ids[res_idx]
    keep <- setdiff(seq_len(nrow(self$X)), res_idx)
    self$X <- self$X[keep, , drop = FALSE]
    self$stratum <- self$stratum[keep]
    self$stratum_groups <- levels(droplevels(self$stratum))
    self$ids <- self$ids[keep]
    res
  }
  G
}

# -------------------------------------------------------------------------------
# create N different random groups
# -------------------------------------------------------------------------------
fix_diff <- function(target, N) {
  diff <- N - sum(target)
  if (diff > 0L) {
    idx <- order(target, decreasing = TRUE)
    target[idx[seq_len(diff)]] <- pmax(0, target[idx[seq_len(abs(diff))]] + sign(diff))
  } else if (diff < 0L) {
    pos <- which(target > 0L)
    if (length(pos) < (-diff)) stop("fix_diff: cannot reduce target enough (too many zeros).")
    idx <- pos[order(target[pos], decreasing = FALSE)]
    target[idx[seq_len(abs(diff))]] <- target[idx[seq_len(-diff)]] + sign(diff)
  }
  target
}

target_randomization_finite <- function(proportional_to_ratio, N, G) {
  n_per_group <- fix_diff(round(proportional_to_ratio * N), N)
  drawn <- G$draw(n_per_group)
  if (is.data.frame(drawn)) {
    labels <- drawn[sample(nrow(drawn)), ]
  } else {
    labels <- sample(drawn)
  }
  labels
}

target_randomization <- function(proportional_to_ratio, N, groups) {
  target <- round(proportional_to_ratio * N)
  target <- fix_diff(target, N)
  labels <- rep(groups, times = target)
  sample(labels)
}

# -------------------------------------------------------------------------------
# Random assignments
# -------------------------------------------------------------------------------
simple_random_assign <- function(df, groups, ratios, col, seed, group_type) {
  set.seed(seed)
  N_total <- nrow(df)
  df <- df[sample.int(N_total), , drop = FALSE]
  proportional_to_ratio <- ratios / sum(ratios)
  if (group_type %in% c("finite", "finite_nd")) {
    temp <- target_randomization_finite(proportional_to_ratio, N_total, groups)
    df[[col]] <- temp
  } else {
    df[[col]] <- target_randomization(proportional_to_ratio, N_total, groups)
  }
  df
}

block_assign_internally <- function(df, block_col, groups, ratios, seed, group_type) {
  set.seed(seed)
  assignment <- character(nrow(df))
  split_idx <- split(seq_len(nrow(df)), df[[block_col]])
  proportional_to_ratio <- ratios / sum(ratios)
  for (b in seq_along(split_idx)) {
    idx <- split_idx[[b]]
    N <- length(idx)
    if (group_type %in% c("finite", "finite_nd")) {
      temp <- target_randomization_finite(proportional_to_ratio, N, groups)
      if (length(temp) != length(assignment[idx])) stop("Not enough elements available")
      assignment[idx] <- temp
    } else {
      assignment[idx] <- target_randomization(proportional_to_ratio, N, groups)
    }
  }
  assignment
}

block_assign <- function(df, block_col, groups, ratios, col, seed, group_type) {
  df[[col]] <- block_assign_internally(df, block_col, groups, ratios, seed, group_type)
  df
}

stratified_assign <- function(df, strata_cols, groups, ratios, col, seed, group_type) {
  stopifnot(!("STRATA_INTERACTION" %in% names(df)))
  df$STRATA_INTERACTION <- interaction(df[strata_cols], drop = TRUE)
  out <- block_assign(
    df = df, block_col = "STRATA_INTERACTION",
    groups = groups, ratios = ratios,
    col = col, seed = seed, group_type = group_type
  )
  out$STRATA_INTERACTION <- NULL
  out
}

shuffle <- function(df) {
  df[sample(seq_len(nrow(df))), , drop = FALSE]
}

random_assign <- function(df, groups, group_type, ratios, col, block_col, strata_cols,
                          randomization_method, n_quantiles, seed) {
  stopifnot(
    is.data.frame(df),
    group_type %in% c("finite", "finite_nd", "infinite"),
    length(groups) >= 1L,
    is.character(col) && !(col %in% names(df)),
    randomization_method %in% c("simple", "block", "block_stratified"),
    is.numeric(seed), length(seed) == 1L
  )
  G <- NULL
  if (group_type == "infinite") {
    stopifnot(
      nrow(df) >= length(groups),
      is.character(groups),
      is.numeric(ratios),
      length(ratios) == length(groups)
    )
    G <- groups
  } else if (group_type == "finite"){
    stopifnot(
      is.numeric(groups), is.null(ratios),
      nrow(df) <= length(groups),
      is.numeric(n_quantiles)
    )
    ratios <- calc_ratios(groups, n_quantiles)
    G <- groups_new(groups, n_quantiles, seq_len(length(groups)))
  } else if (group_type == "finite_nd") {
    stopifnot(
      is.data.frame(groups) || is.matrix(groups),
      is.null(ratios),
      nrow(df) <= nrow(as.data.frame(groups)),
      is.numeric(n_quantiles)
    )
    ratios <- calc_ratios_nd(groups, n_quantiles)
    G <- groups_new_nd(groups, n_quantiles, seq_len(nrow(groups)))
  }

  if (randomization_method == "simple") {
    simple_random_assign(df, G, ratios, col, seed, group_type) |> shuffle()
  } else if (randomization_method == "block") {
    stopifnot(is.character(block_col) && block_col %in% names(df))
    block_assign(df, block_col, G, ratios, col, seed, group_type) |> shuffle()
  } else if (randomization_method == "block_stratified") {
    stopifnot(
      !is.null(strata_cols),
      is.character(strata_cols) && all(strata_cols %in% names(df))
    )
    stratified_assign(df, strata_cols, G, ratios, col, seed, group_type) |> shuffle()
  }
}


set.seed(42)
predictors <- list(
  cellLines = c("HeLa", "Hek"),
  Treatment = LETTERS[1:4]
)
design <- Randomization:::completely_randomised_design(predictors, 10)
groups <- data.frame(
  weights = rweibull(120, 300, 50),
  blood = rnorm(120, 10, 1)
)
res <- random_assign(
  design,
  groups = groups,
  group_type = "finite_nd", ratios = NULL,
  "Group", strata_cols = c("Treatment", "cellLines"),
  randomization_method = "block_stratified",
  n_quantiles = 2L, seed = 1234
)
stopifnot(length(unique(res$Group)) == nrow(design))
res$weights <- groups[as.integer(res$Group), "weights"]
res$blood <- groups[as.integer(res$Group), "blood"]
par(mfrow = c(1, 2))
boxplot(res$weights ~ res$cellLines*res$Treatment)
boxplot(res$blood ~ res$cellLines*res$Treatment)
