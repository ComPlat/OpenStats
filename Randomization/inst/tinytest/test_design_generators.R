library(tinytest)

test_completely_randomised_design_single_predictor_stays_a_data_frame <- function() {
  # Regression: with one predictor, expand.grid() produces a single-column
  # design, and design[i, ] without drop = FALSE silently simplifies to an
  # atomic vector -- do.call(rbind, ...) on those then produces a matrix,
  # not a data.frame (surfaced as a `validObject` crash in OpenDOE, since
  # designResult's `df` slot requires a data.frame).
  design <- Randomization::completely_randomised_design(list(drug = c("A", "B", "C")), 3)
  checks <- logical(3)
  checks[[1]] <- expect_true(is.data.frame(design))
  checks[[2]] <- expect_equal(nrow(design), 9L)
  checks[[3]] <- expect_equal(as.character(design$drug), rep(c("A", "B", "C"), each = 3))
  expect_true(all(checks))
}
test_completely_randomised_design_single_predictor_stays_a_data_frame()

test_completely_randomised_design_multiple_predictors <- function() {
  design <- Randomization::completely_randomised_design(
    list(drug = c("A", "B"), dose = c("low", "high")), 2
  )
  checks <- logical(3)
  checks[[1]] <- expect_true(is.data.frame(design))
  checks[[2]] <- expect_equal(nrow(design), 8L)
  checks[[3]] <- expect_true(all(c("drug", "dose") %in% names(design)))
  expect_true(all(checks))
}
test_completely_randomised_design_multiple_predictors()
