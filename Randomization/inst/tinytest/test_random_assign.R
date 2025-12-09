library(tinytest)

error_trigger <- function() {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
  )
  design <- Randomization:::completely_randomised_design(predictors, 10)
  groups <- paste0("Day", 1:4)
  checks <- logical(13)

  checks[[1]] <- expect_error(Randomization::random_assign(
    c(1, 2, 3), groups = groups, group_type = "infinite",
    c(1, 1, 1, 1), "Group", randomization_method = "simple",
    seed = 1234
  ), class = "error", info = "df is not a data.frame")

  checks[[2]] <- expect_error(Randomization::random_assign(
    design, groups = groups, group_type = "invalid",
    c(1, 1, 1, 1), "Group", randomization_method = "simple",
    seed = 1234
  ), class = "error", info = "invalid group_type")

  checks[[3]] <- expect_error(Randomization::random_assign(
    design, groups = character(), group_type = "infinite",
    c(1, 1, 1, 1), "Group", randomization_method = "simple",
    seed = 1234
  ), class = "error", info = "group length is 0")

  checks[[4]] <- expect_error(Randomization::random_assign(
    design, groups = groups, group_type = "infinite",
    c("Bla", "Bla", "Bla", "Bla"), "Group", randomization_method = "simple",
    seed = 1234
  ), class = "error", info = "ratios not numeric")

  checks[[5]] <- expect_error(Randomization::random_assign(
    design, groups = groups, group_type = "infinite",
    c(1, 1, 1, 1), 3.14, randomization_method = "simple",
    seed = 1234
  ), class = "error", info = "col not character")

  checks[[6]] <- expect_error(Randomization::random_assign(
    design, groups = groups, group_type = "infinite",
    c(1, 1, 1, 1), names(design)[1], randomization_method = "simple",
    seed = 1234
  ), class = "error", info = "Invalid col name as already in use")

  checks[[7]] <- expect_error(Randomization::random_assign(
    design, groups = groups, group_type = "infinite",
    c(1, 1, 1, 1), "Group", randomization_method = "Invalid",
    seed = 1234
  ), class = "error", info = "Invalid randomization_method")

  checks[[8]] <- expect_error(Randomization::random_assign(
    design, groups = groups, group_type = "infinite",
    c(1, 1, 1, 1), "Group", randomization_method = "simple",
    seed = "Invalid seed"
  ), class = "error", info = "Invalid seed")

  checks[[9]] <- expect_error(Randomization::random_assign(
    design, groups = groups, group_type = "infinite",
    c(1, 1, 1, 1), "Group", randomization_method = "simple",
    seed = c(1, 2)
  ), class = "error", info = "Invalid seed")

  checks[[10]] <- expect_error(Randomization::random_assign(
    design, groups = groups, group_type = "finite",
    c(1, 1, 1, 1), "Group", randomization_method = "simple",
    seed = 1234
  ), class = "error", info = "finite requires numeric groups!")

  checks[[11]] <- expect_error(Randomization::random_assign(
    design, groups = c(1, 2, 3, 4), group_type = "infinite",
    c(1, 1, 1, 1), "Group", randomization_method = "simple",
    seed = 1234
  ), class = "error", info = "infinite requires character type for groups")

  checks[[12]] <- expect_error(Randomization::random_assign(
    design,
    groups = paste0("Day", 1:(nrow(design) + 1L)), group_type = "infinite",
    c(1, 1, 1, 1), "Group", randomization_method = "simple",
    seed = 1234
  ), class = "error", info = "infinite requires nrow(design) <= length(groups)")

  checks[[13]] <- expect_error(Randomization::random_assign(
    design, groups = groups, group_type = "infinite",
    c(1, 1, 1, 1, 1), "Group", randomization_method = "simple",
    seed = 1234
  ), class = "error", info = "infinite requires length(groups) == length(ratios)")

  expect_true(all(checks))
}
error_trigger()

simple_infinite <- function() {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
  )
  design <- Randomization:::completely_randomised_design(predictors, 10)
  groups <- paste0("Day", 1:4)
  checks <- logical(6L)
  res <- Randomization::random_assign(
    design,
    groups = groups,
    group_type = "infinite",
    c(1, 1, 1, 1),
    "Group",
    randomization_method = "simple",
    seed = 1234
  )
  res
  checks[[1]] <- expect_equal(nrow(res), nrow(design))
  checks[[2]] <- expect_equal(ncol(res), (ncol(design) + 1L))
  subs <- split(res, res$Group)
  rows <- vapply(subs, function(elem) {
    nrow(elem)
  }, numeric(1L))
  checks[[3]] <- expect_equal(rows[["Day1"]], 20)
  checks[[4]] <- expect_equal(rows[["Day2"]], 20)
  checks[[5]] <- expect_equal(rows[["Day3"]], 20)
  checks[[6]] <- expect_equal(rows[["Day4"]], 20)
  expect_true(all(checks))
}
simple_infinite()

simple_finite <- function() {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
  )
  design <- Randomization:::completely_randomised_design(predictors, 10)
  set.seed(42)
  design <- design[sample(1:nrow(design), nrow(design) * 0.75), ]
  groups <- rnorm(200, 300, 25)
  checks <- logical(2L)
  res <- Randomization::random_assign(
    design, groups = groups,
    group_type = "finite", ratios = NULL, "Group",
    randomization_method = "simple", seed = 1234
  )
  checks[[1]] <- expect_equal(nrow(res), nrow(design))
  checks[[2]] <- expect_equal(ncol(res), (ncol(design) + 1L))
  expect_true(all(checks))
}
simple_finite()

block_infinite <- function() {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
  )
  design <- Randomization:::completely_randomised_design(predictors, 10)
  groups <- paste0("Day", 1:4)
  checks <- logical(6L)
  res <- Randomization::random_assign(
    design,
    groups = groups,
    group_type = "infinite",
    c(1, 1, 1, 1),
    "Group", block_col = "Treatment",
    randomization_method = "block",
    seed = 1234
  )
  checks[[1]] <- expect_equal(nrow(res), nrow(design))
  checks[[2]] <- expect_equal(ncol(res), (ncol(design) + 1L))
  res <- res[, -3] # remove id
  rt <- table(res)
  n_per_treatment <- lapply(groups, function(d) {
    sub <- rt[, , d]
    setNames(list(colSums(sub)), d)
  })
  expected <- c(A = 5, B = 5, C = 5, D = 5)
  checks[[3]] <- expect_equal(n_per_treatment[[1]]$Day1, expected)
  checks[[4]] <- expect_equal(n_per_treatment[[2]]$Day2, expected)
  checks[[5]] <- expect_equal(n_per_treatment[[3]]$Day3, expected)
  checks[[6]] <- expect_equal(n_per_treatment[[4]]$Day4, expected)
  expect_true(all(checks))
}
block_infinite()

block_finite <- function(seed1, seed2) {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
  )
  design <- Randomization:::completely_randomised_design(predictors, 10)
  set.seed(seed1)
  design <- design[sample(1:nrow(design), nrow(design) * 0.75), ]
  groups <- rnorm(200, 300, 25)
  checks <- logical(7L)

  res <- Randomization::random_assign(
    design,
    groups = groups,
    group_type = "finite", ratios = NULL,
    "Group", block_col = "Treatment",
    randomization_method = "block",
    seed = seed2
  )
  checks[[1]] <- expect_equal(nrow(res), nrow(design))
  checks[[2]] <- expect_equal(ncol(res), (ncol(design) + 1L))

  subs <- split(res, res$Treatment)
  F_global <- ecdf(groups)
  xs_global <- sort(groups)

  D_vals <- vapply(subs, function(d) {
    x_block <- as.numeric(d$Group)
    F_block <- ecdf(x_block)
    xs <- sort(unique(c(xs_global, x_block)))
    max(abs(F_global(xs) - F_block(xs)))
  }, numeric(1L))

   checks[[3]] <- expect_true(D_vals["A"] < 0.25)
   checks[[4]] <- expect_true(D_vals["B"] < 0.25)
   checks[[5]] <- expect_true(D_vals["C"] < 0.25)
   checks[[6]] <- expect_true(D_vals["D"] < 0.25)
   checks[[7]] <- expect_equal(
     length(unique(res$Group)),
     length(res$Group)
   )

  expect_true(all(checks))
}
block_finite(1234, 42)
block_finite(454, 4235)
block_finite(235246, 12335)


stratum_block_infinite <- function() {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
  )
  design <- Randomization:::completely_randomised_design(predictors, 16)
  groups <- paste0("Day", 1:4)
  checks <- logical(6L)
  res <- Randomization::random_assign(
    design,
    groups = groups,
    group_type = "infinite",
    c(1, 1, 1, 1),
    col = "Group", strata_cols = c("Treatment", "cellLines"),
    randomization_method = "block_stratified",
    seed = 1234
  )
  checks[[1]] <- expect_equal(nrow(res), nrow(design))
  checks[[2]] <- expect_equal(ncol(res), (ncol(design) + 1L))
  res <- res[, -3] # remove id
  rt <- table(res)
  n_per_treatment <- lapply(groups, function(d) {
    sub <- rt[, , d]
    setNames(list(colSums(sub)), d)
  })
  expected <- c(A = 8, B = 8, C = 8, D = 8)
  checks[[3]] <- expect_equal(n_per_treatment[[1]]$Day1, expected)
  checks[[4]] <- expect_equal(n_per_treatment[[2]]$Day2, expected)
  checks[[5]] <- expect_equal(n_per_treatment[[3]]$Day3, expected)
  checks[[6]] <- expect_equal(n_per_treatment[[4]]$Day4, expected)
  expect_true(all(checks))
}
stratum_block_infinite()

stratum_block_finite <- function(seed1, seed2) {
  predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
  )
  design <- Randomization:::completely_randomised_design(predictors, 10)
  set.seed(seed1)
  groups <- rnorm(200, 300, 25)
  checks <- logical(7L)

  res <- Randomization::random_assign(
    design,
    groups = groups,
    group_type = "finite", ratios = NULL,
    "Group", strata_cols = c("Treatment", "cellLines"),
    randomization_method = "block_stratified",
    seed = seed2
  )
  checks[[1]] <- expect_equal(nrow(res), nrow(design))
  checks[[2]] <- expect_equal(ncol(res), (ncol(design) + 1L))

  subs <- split(res, res$Treatment)
  F_global <- ecdf(groups)
  xs_global <- sort(groups)

  D_vals <- vapply(subs, function(d) {
    x_block <- as.numeric(d$Group)
    F_block <- ecdf(x_block)
    xs <- sort(unique(c(xs_global, x_block)))
    max(abs(F_global(xs) - F_block(xs)))
  }, numeric(1L))

   checks[[3]] <- expect_true(D_vals["A"] < 0.25)
   checks[[4]] <- expect_true(D_vals["B"] < 0.25)
   checks[[5]] <- expect_true(D_vals["C"] < 0.25)
   checks[[6]] <- expect_true(D_vals["D"] < 0.25)
   checks[[7]] <- expect_equal(
     length(unique(res$Group)),
     length(res$Group)
   )

  expect_true(all(checks))
}
stratum_block_finite(1234, 42)
