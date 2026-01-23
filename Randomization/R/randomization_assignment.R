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
  vapply(tbl_str, \(g) g / sum(tbl_str), numeric(1))
}

groups_new <- function(groups, n_quantiles) {
  if (!is.numeric(groups)) stop("Finite groups must be numeric")
  self <- new.env(parent = emptyenv())
  self$groups <- groups
  self$stratum <- calc_stratum(self$groups, n_quantiles)
  self$stratum_groups <- sort(unique(self$stratum))
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
        res <- c(res, self$groups[global_indices])
        self$groups <- self$groups[-global_indices]
        self$stratum <- self$stratum[-global_indices]
      }
    }
    return(res)
  }
  G
}

fix_diff <- function(target, N) {
  diff <- N - sum(target)
  if (diff != 0) {
    idx <- order(target, decreasing = (diff > 0))
    target[idx[seq_len(abs(diff))]] <- 
      pmax(0, target[idx[seq_len(abs(diff))]] + sign(diff))
  }
  target
}

target_randomization_finite <- function(proportional_to_ratio, N, G) {
  n_per_group <- fix_diff(round(proportional_to_ratio * N), N)
  labels <- G$draw(n_per_group)
  sample(labels)
}
target_randomization <- function(proportional_to_ratio, N, groups) {
  target <- round(proportional_to_ratio * N)
  target <- fix_diff(target, N)
  labels <- rep(groups, times = target)
  sample(labels)
}

simple_random_assign <- function(df, groups, ratios, col, seed, group_type) {
  set.seed(seed)
  N_total <- nrow(df)
  df <- df[sample.int(N_total), , drop = FALSE]
  proportional_to_ratio <- ratios / sum(ratios)
  if (group_type == "finite") {
    df[[col]] <- target_randomization_finite(proportional_to_ratio, N_total, groups)
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
    if (group_type == "finite") {
      assignment[idx] <- target_randomization_finite(proportional_to_ratio, N, groups)
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
    group_type %in% c("finite", "infinite"),
    length(groups) >= 1L,
    is.character(col) && !(col %in% names(df)),
    randomization_method %in% c("simple", "block", "block_stratified"),
    is.numeric(seed), length(seed) == 1L
  )
  if (group_type == "infinite") {
    stopifnot(
      nrow(df) >= length(groups),
      is.character(groups),
      is.numeric(ratios),
      length(ratios) == length(groups)
    )
  } else {
    stopifnot(
      is.numeric(groups), is.null(ratios),
      nrow(df) <= length(groups),
      is.numeric(n_quantiles)
    )
    ratios <- calc_ratios(groups, n_quantiles)
  }
  G <- groups
  if (group_type == "finite") G <- groups_new(groups, n_quantiles)

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
