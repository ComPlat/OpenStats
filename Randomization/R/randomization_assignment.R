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

target_randomization <- function(proportional_to_ratio, N, groups) {
  target <- round(proportional_to_ratio * N)
  target <- fix_diff(target, N)
  labels <- rep(groups, times = target)
  sample(labels)
}

# -------------------------------------------------------------------------------
# Random assignments
# -------------------------------------------------------------------------------
simple_random_assign <- function(df, groups, ratios, col, seed) {
  set.seed(seed)
  N_total <- nrow(df)
  df <- df[sample.int(N_total), , drop = FALSE]
  proportional_to_ratio <- ratios / sum(ratios)
  df[[col]] <- target_randomization(proportional_to_ratio, N_total, groups)
  df
}

block_assign_internally <- function(df, block_col, groups, ratios, seed) {
  set.seed(seed)
  assignment <- character(nrow(df))
  split_idx <- split(seq_len(nrow(df)), df[[block_col]])
  proportional_to_ratio <- ratios / sum(ratios)
  for (b in seq_along(split_idx)) {
    idx <- split_idx[[b]]
    N <- length(idx)
    assignment[idx] <- target_randomization(proportional_to_ratio, N, groups)
  }
  assignment
}

block_assign <- function(df, block_col, groups, ratios, col, seed, group_type) {
  df[[col]] <- block_assign_internally(df, block_col, groups, ratios, seed)
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

random_assign <- function(df, groups, ratios, col, block_col, strata_cols,
                          randomization_method, n_quantiles = 10L, seed) {
  stopifnot(
    is.data.frame(df),
    length(groups) >= 1L,
    is.character(col) && !(col %in% names(df)),
    randomization_method %in% c("simple", "block", "block_stratified"),
    is.numeric(seed), length(seed) == 1L
  )
  stopifnot(
    nrow(df) >= length(groups),
    is.character(groups),
    is.numeric(ratios),
    length(ratios) == length(groups)
  )
  G <- groups

  if (randomization_method == "simple") {
    simple_random_assign(df, G, ratios, col, seed) |> shuffle()
  } else if (randomization_method == "block") {
    stopifnot(is.character(block_col) && block_col %in% names(df))
    block_assign(df, block_col, G, ratios, col, seed) |> shuffle()
  } else if (randomization_method == "block_stratified") {
    stopifnot(
      !is.null(strata_cols),
      is.character(strata_cols) && all(strata_cols %in% names(df))
    )
    stratified_assign(df, strata_cols, G, ratios, col, seed) |> shuffle()
  }
}
