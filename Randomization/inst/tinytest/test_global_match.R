library(tinytest)
# --------------------------------------------------------------------------------------------
# Tests
# --------------------------------------------------------------------------------------------

# 1 finite groups but not normally distributed
# --------------------------------------------------------------------------------------------
test_1_finite_group_non_normal <- function() {
  n_blocks <- data.frame(treatment = rep(c("control", "one", "two"), each = 27L))
  groups <- data.frame(blood = c(rep(4000, 27L), rep(3500, 27L), rep(300, 27L)))
  correct <- function(df) {
    df <- df$assigned
    df$treatment <- n_blocks$treatment
    tbl <- with(df, table(treatment, blood))
    attr(tbl, "dimnames") <- NULL
    attr(tbl, "class") <- NULL
    identical(tbl, matrix(9L, 3L, 3L))
  }
  checks <- logical(3L)
  checks[[1L]] <- random_finite_assign(42, groups, n_blocks) |> correct()
  checks[[2L]] <- random_finite_assign(1234, groups, n_blocks) |> correct()
  checks[[3L]] <- random_finite_assign(3344, groups, n_blocks) |> correct()
  all(checks)
}
test_1_finite_group_non_normal() |> expect_true()

# 2 finite groups; linear independent
# --------------------------------------------------------------------------------------------
test_2_independent_groups <- function() {
  n_blocks <- data.frame(
    treatment = rep(c("control", "one", "two"), each = 36L),
    location = rep(rep(c("A", "B", "C", "D"), each = 9L), 3L)
  )
  set.seed(333)
  groups <- data.frame(
    weights = c(rep(325, 36L), rep(300, 36L), rep(275, 36L)),
    blood = rnorm(108, mean = 10, sd = 1)
  )
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
    checks[[2L]] <- sum(abs(b[, 1L] - rep(10, 3L))) < 0.5
    checks[[3L]] <- sum(abs(b[, 2L] - rep(1, 3L))) < 0.25
    all(checks)
  }
  res <- random_finite_assign(42, groups, n_blocks)
  checks_outer[[1L]] <- correct(res)
  all(checks_outer)
}
test_2_independent_groups() |> expect_true()

# 2 finite groups; linear dependent
# --------------------------------------------------------------------------------------------
test_2_dependent_groups <- function() {
  n_blocks <- data.frame(
    treatment = rep(c("control", "one", "two"), each = 36L),
    location = rep(rep(c("A", "B", "C", "D"), each = 9L), 3L)
  )
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
  checks <- logical(3L)
  checks[[1L]] <- random_finite_assign(42, groups, n_blocks) |> correct()
  checks[[2L]] <- random_finite_assign(1234, groups, n_blocks) |> correct()
  checks[[3L]] <- random_finite_assign(3344, groups, n_blocks) |> correct()
  all(checks)
}
test_2_dependent_groups() |> expect_true()

# 1 finite groups
# --------------------------------------------------------------------------------------------
test_1_finite_group <- function() {
  n_blocks <- data.frame(treatment = rep(c("control", "one", "two"), each = 27L))
  groups <- data.frame(weights = c(rep(325, 27L), rep(300, 27L), rep(275, 27L)))
  correct <- function(df) {
    df <- df$assigned
    df$treatment <- n_blocks$treatment
    tbl <- with(df, table(treatment, weights))
    attr(tbl, "dimnames") <- NULL
    attr(tbl, "class") <- NULL
    identical(tbl, matrix(9L, 3L, 3L))
  }
  checks <- logical(3L)
  checks[[1L]] <- random_finite_assign(42, groups, n_blocks) |> correct()
  checks[[2L]] <- random_finite_assign(1234, groups, n_blocks) |> correct()
  checks[[3L]] <- random_finite_assign(3344, groups, n_blocks) |> correct()
  all(checks)
}
test_1_finite_group() |> expect_true()

# No duplicates & no missing entries
# --------------------------------------------------------------------------------------------
test_perm_is_permutation <- function() {
  blocks <- data.frame(bla = factor(rep(1:3, each = 10)))
  groups <- data.frame(x = rnorm(30), y = rnorm(30))
  res <- random_finite_assign(1, groups, blocks)
  p <- res$perm
  stopifnot(length(p) == nrow(groups))
  stopifnot(identical(sort(p), seq_len(nrow(groups))))
  TRUE
}
test_perm_is_permutation() |> expect_true()

# 2 finite groups which are of different size and linear independent
# --------------------------------------------------------------------------------------------
test_2_independent_groups_different_size <- function() {
  n_blocks <- data.frame(
    treatment = rep(c("control", "one", "two"), each = 37L),
    location = rep(
      c(
        rep("A", 10L), rep("B", 11L), rep("C", 7L), rep("D", 9L)
      ), 3L
    )
  )
  set.seed(333)
  groups <- data.frame(
    weights = c(rep(325, 37L), rep(300, 37L), rep(275, 37L)),
    blood   = rnorm(111, mean = 10, sd = 1)
  )
  res <- random_finite_assign(42, groups, n_blocks, 150L)
  df <- res$assigned
  df$treatment <- n_blocks$treatment
  df$location  <- n_blocks$location
  #
  tbl <- with(df, table(interaction(treatment, location), weights))
  tbl <- as.data.frame(tbl)
  tbl$treatment <- sub("\\..*$", "", tbl$Var1)
  tbl$location  <- sub("^.*\\.", "", tbl$Var1)
  tbl$weights <- as.numeric(as.character(tbl$weights))
  tbl$Freq    <- as.integer(tbl$Freq)
  wmean <- function(sub) {
    sum(sub$Freq * sub$weights) / sum(sub$Freq)
  }
  totals <- tapply(tbl$Freq, tbl$location, sum)
  ok_totals <- identical(unname(as.integer(totals[c("A","B","C","D")])),
                         c(30L, 33L, 21L, 27L))
  target <- 300
  locs <- c("A","B","C","D")
  trts <- c("control","one","two")
  means <- matrix(NA_real_, nrow = length(locs), ncol = length(trts),
                  dimnames = list(locs, trts))
  for (L in locs) {
    for (T in trts) {
      sub <- tbl[tbl$location == L & tbl$treatment == T, , drop = FALSE]
      means[L, T] <- wmean(sub)
    }
  }
  tol_mean <- 8
  ok_means <- all(abs(means - target) <= tol_mean)
  tol_spread <- 8
  ok_spread <- all(apply(means, 1, function(x) (max(x) - min(x)) <= tol_spread))
  ok_totals && ok_means && ok_spread
}
test_2_independent_groups_different_size() |> expect_true()

# 1 finite groups; but not a continous variable
# --------------------------------------------------------------------------------------------
test_1_finite_factor_data_group <- function() {
  n_blocks <- data.frame(treatment = rep(c("control", "one", "two"), each = 27L))
  groups <- data.frame(experimental_blocks = c(rep("A", 27L), rep("B", 27L), rep("C", 27L)))
  groups$experimental_blocks <- as.factor(groups$experimental_blocks)
  groups$experimental_blocks <- match(groups$experimental_blocks, c("A","B","C"))
  correct <- function(df) {
    df <- df$assigned
    df$treatment <- n_blocks$treatment
    tbl <- with(df, table(treatment, experimental_blocks))
    attr(tbl, "dimnames") <- NULL
    attr(tbl, "class") <- NULL
    identical(tbl, matrix(9L, 3L, 3L))
  }
  checks <- logical(3L)
  checks[[1L]] <- random_finite_assign(42, groups, n_blocks) |> correct()
  checks[[2L]] <- random_finite_assign(1234, groups, n_blocks) |> correct()
  checks[[3L]] <- random_finite_assign(3344, groups, n_blocks) |> correct()
  all(checks)
}
test_1_finite_factor_data_group() |> expect_true()

# 1 finite groups; categorical / factor-like (batch IDs, cage IDs, plate IDs)
# --------------------------------------------------------------------------------------------
test_1_finite_factor_data_group <- function() {
  n_blocks <- data.frame(treatment = rep(c("control", "one", "two"), each = 27L))
  groups <- data.frame(experimental_blocks = c(rep("A", 27L), rep("B", 27L), rep("C", 27L)))
  # stable mapping A->1, B->2, C->3 (don’t rely on factor level order)
  groups$experimental_blocks <- match(groups$experimental_blocks, c("A", "B", "C"))
  correct <- function(res) {
    df <- res$assigned
    df$treatment <- n_blocks$treatment
    tbl <- with(df, table(treatment, experimental_blocks))
    attr(tbl, "dimnames") <- NULL
    attr(tbl, "class") <- NULL
    identical(tbl, matrix(9L, 3L, 3L))
  }
  checks <- logical(3L)
  checks[[1L]] <- random_finite_assign(42, groups, n_blocks) |> correct()
  checks[[2L]] <- random_finite_assign(1234, groups, n_blocks) |> correct()
  checks[[3L]] <- random_finite_assign(3344, groups, n_blocks) |> correct()
  all(checks)
}
test_1_finite_factor_data_group() |> expect_true()

# 1 finite factor group; unbalanced category sizes
# --------------------------------------------------------------------------------------------
test_1_finite_factor_unbalanced <- function() {
  n_blocks <- data.frame(treatment = rep(c("control", "one", "two"), each = 27L))
  groups <- data.frame(
    experimental_blocks = c(rep("A", 30L), rep("B", 25L), rep("C", 26L))
  )
  groups$experimental_blocks <- match(groups$experimental_blocks, c("A", "B", "C"))
  expected_range <- function(n_cat, T) {
    base <- n_cat %/% T
    rem  <- n_cat %% T
    if (rem == 0L) {
      c(min = base, max = base)
    } else {
      c(min = base, max = base + 1L)
    }
  }

  correct <- function(res) {
    df <- res$assigned
    df$treatment <- n_blocks$treatment
    tbl <- with(df, table(treatment, experimental_blocks))
    nA <- 30L; nB <- 25L; nC <- 26L
    T  <- 3L
    rngA <- expected_range(nA, T)
    rngB <- expected_range(nB, T)
    rngC <- expected_range(nC, T)
    ok <- TRUE
    ok <- ok && all(tbl[, 1L] >= rngA[["min"]] & tbl[, 1L] <= rngA[["max"]])
    ok <- ok && all(tbl[, 2L] >= rngB[["min"]] & tbl[, 2L] <= rngB[["max"]])
    ok <- ok && all(tbl[, 3L] >= rngC[["min"]] & tbl[, 3L] <= rngC[["max"]])
    ok <- ok && identical(as.integer(colSums(tbl)), c(nA, nB, nC))
    ok <- ok && identical(as.integer(rowSums(tbl)), rep(27L, 3L))
    ok
  }
  checks <- logical(3L)
  checks[[1L]] <- random_finite_assign(42, groups, n_blocks)   |> correct()
  checks[[2L]] <- random_finite_assign(1234, groups, n_blocks) |> correct()
  checks[[3L]] <- random_finite_assign(3344, groups, n_blocks) |> correct()
  all(checks)
}
test_1_finite_factor_unbalanced() |> expect_true()

# 1 finite group, and one block but there is a surplus for the 
# --------------------------------------------------------------------------------------------
test_1_finite_group_with_surplus <- function() {
  n_blocks <- data.frame(treatment = rep(c("control", "one", "two"), each = 9L))
  groups <- data.frame(weights = c(rep(325, 9L), rep(300, 9L), rep(275, 9L), c(300, 275, 325)))
  correct <- function(df) {
    df <- df$assigned
    df$treatment <- n_blocks$treatment
    tbl <- with(df, table(treatment, weights))
    attr(tbl, "dimnames") <- NULL
    attr(tbl, "class") <- NULL
    identical(tbl, matrix(3L, 3L, 3L))
  }
  checks <- logical(3L)
  checks[[1L]] <- random_finite_assign(42, groups, n_blocks) |> correct()
  checks[[2L]] <- random_finite_assign(1234, groups, n_blocks) |> correct()
  checks[[3L]] <- random_finite_assign(3344, groups, n_blocks) |> correct()
  all(checks)
}
test_1_finite_group_with_surplus() |> expect_true()
