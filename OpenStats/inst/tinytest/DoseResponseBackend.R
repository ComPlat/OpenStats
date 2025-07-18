library(tinytest)

# Test cases for errorClass
test_errorClass <- function() {
  checks <- list()
  err <- OpenStats:::errorClass$new("An error message")
  checks[[1]] <- expect_false(err$isNull())
  checks[[2]] <- expect_equal(err$error_message, "An error message")
  err_null <- OpenStats:::errorClass$new()
  checks[[3]] <- expect_true(err_null$isNull())
  all(unlist(checks))
}

# Test cases for shapenumber
test_shapenumber <- function() {
  checks <- list()
  checks[[1]] <- expect_equal(OpenStats:::shapenumber(123.456), signif(123.456))
  checks[[2]] <- expect_equal(OpenStats:::shapenumber(Inf), NA)
  checks[[3]] <- expect_equal(OpenStats:::shapenumber(-Inf), NA)
  checks[[4]] <- expect_equal(OpenStats:::shapenumber(NA), NA)
  all(unlist(checks))
}

# Test cases for robust_68_percentile
test_robust_68_percentile <- function() {
  checks <- list()
  residuals <- c(1, 2, 3, 4, 5)
  checks[[1]] <- expect_true(is.double(OpenStats:::robust_68_percentile(residuals)))

  # Test with Gaussian residuals
  residuals <- rnorm(100, mean = 0, sd = 1)
  p68 <- OpenStats:::robust_68_percentile(residuals)
  checks[[2]] <- expect_true( abs(p68 - quantile(abs(residuals), 0.6827)) < 0.1)

  # Test with outliers
  residuals_with_outliers <- c(rnorm(95, mean = 0, sd = 1), 10, 15, -20, 25)
  p68_robust <- OpenStats:::robust_68_percentile(residuals_with_outliers)
  checks[[3]] <- expect_true(p68_robust < sd(residuals_with_outliers)) # Robust measure

  # Test edge case: identical residuals
  identical_residuals <- rep(1, 10)
  p68_identical <- OpenStats:::robust_68_percentile(identical_residuals)
  checks[[4]] <- expect_true(abs(p68_identical - 1) < 0.01)
  all(unlist(checks))
}

# Test cases for rsdr
test_rsdr <- function() {
  checks <- list()
  # Basic functionality
  residuals <- c(1, 2, 3, 4, 5)
  checks[[1]] <- expect_true(is.double(OpenStats:::rsdr(residuals, 1)))

  # Test with Gaussian residuals
  residuals_gaussian <- rnorm(100, mean = 0, sd = 1)
  rsdr_value <- OpenStats:::rsdr(residuals_gaussian, 1)
  checks[[2]] <- expect_true(rsdr_value > 0)

  # Test with outliers
  residuals_with_outliers <- c(rnorm(95, mean = 0, sd = 1), 10, 15, -20, 25)
  rsdr_outliers <- OpenStats:::rsdr(residuals_with_outliers, 2)
  checks[[3]] <- expect_true(rsdr_outliers < sd(residuals_with_outliers)) # Robust measure

  # Test small sample sizes
  small_residuals <- c(-2, -1, 0, 1, 2)
  rsdr_small <- OpenStats:::rsdr(small_residuals, 1)
  checks[[4]] <- expect_true(rsdr_small > 0)
  all(unlist(checks))
}

# Test cases for false_discovery_rate
test_false_discovery_rate <- function() {
  checks <- list()
  residuals <- c(1, 2, 3, 4, 5)
  include <- OpenStats:::false_discovery_rate(residuals)
  checks[[1]] <- expect_true(is.logical(include))
  checks[[2]] <- expect_equal(length(include), length(residuals))
  all(unlist(checks))
}

# Test cases for ic50
test_ic50 <- function() {
  checks <- list()
  library(drc)
  data <- data.frame(
    abs = c(0.5, 0.6, 0.7, 0.8, 0.9),
    conc = c(1, 10, 100, 1000, 10000),
    names = c("A", "A", "A", "A", "A")
  )
  result <- OpenStats:::ic50(data, "abs", "conc", "names", NULL, FALSE, FALSE)
  checks[[1]] <- expect_true(is.list(result))
  checks[[2]] <- expect_true(is.data.frame(result[[1]][[1]]))
  all(unlist(checks))
}

# Run all tests
test_errorClass()
test_shapenumber()
test_robust_68_percentile()
test_rsdr()
test_false_discovery_rate()
test_ic50()
