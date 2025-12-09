expand_design <- function(design, n_per_level) {
  l <- lapply(seq_len(nrow(design)), function(i) {
    row <- design[i, ]
    rows <- lapply(seq_len(n_per_level), function(r) row)
    do.call(rbind, rows)
  })
  do.call(rbind, l)
}

completely_randomised_design <- function(predictors, n_per_level) {
  design <- expand.grid(predictors, stringsAsFactors = FALSE)
  design$id <- seq_len(nrow(design))
  expand_design(design, n_per_level)
}

blocked_design <- function(predictors, block_var, n_rep_block = 1L) {
  stopifnot(block_var %in% names(predictors))

  block_levels <- predictors[[block_var]]
  others <- predictors[setdiff(names(predictors), block_var)]

  designs <- lapply(block_levels, function(b) {
    base <- expand.grid(others)
    base[[block_var]] <- b
    base[
      rep(seq_len(nrow(base)), each = n_rep_block),
      , drop = FALSE]
  })
  design <- do.call(rbind, designs)
  row.names(design) <- NULL
  design
}

latin_square_design <- function(rows, cols, treatments) {
  stopifnot(length(rows) == length(cols), length(cols) == length(treatments))
  k <- length(treatments)
  # canonical Latin square (permutation of 1:k)
  base <- outer(0:(k-1), 0:(k-1), function(i, j) ((i + j) %% k) + 1)
  df <- expand.grid(
    Row = rows,
    Col = cols
  )
  df$Treatment <- treatments[as.vector(base)]
  df
}

split_plot_design <- function(predictors, whole_plot_var,
                              n_rep_whole = 1L, n_rep_sub = 1L) {
  stopifnot(whole_plot_var %in% names(predictors))

  whole <- predictors[whole_plot_var]
  sub <- predictors[setdiff(names(predictors), whole_plot_var)]

  whole_df <- expand.grid(whole)
  whole_df <- whole_df[rep(seq_len(nrow(whole_df)), each = n_rep_whole), , drop = FALSE]
  whole_df$whole_id <- seq_len(nrow(whole_df))

  designs <- lapply(seq_len(nrow(whole_df)), function(i) {
    base <- expand.grid(sub)
    base <- base[rep(seq_len(nrow(base)), each = n_rep_sub), , drop = FALSE]
    cbind(whole_df[rep(i, nrow(base)), , drop = FALSE], base)
  })

  design <- do.call(rbind, designs)
  rownames(design) <- NULL
  design
}

split_block_design <- function(predictors, block_vars, n_rep_block = 1L) {
  stopifnot(all(block_vars %in% names(predictors)))

  blocks <- predictors[block_vars]
  others <- predictors[setdiff(names(predictors), block_vars)]

  block_combos <- expand.grid(blocks)

  designs <- lapply(seq_len(nrow(block_combos)), function(i) {
    base <- expand.grid(others)
    base[rep(seq_len(nrow(base)), each = n_rep_block), , drop = FALSE]
    cbind(block_combos[rep(i, nrow(base)), , drop = FALSE], base)
  })

  design <- do.call(rbind, designs)
  rownames(design) <- NULL
  design
}
