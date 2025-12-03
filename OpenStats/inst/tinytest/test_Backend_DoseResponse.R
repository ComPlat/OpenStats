library(tinytest)

simulate <- function(name, slope, true_ic50) {
  # Define true parameters
  b  <- slope         # slope
  c  <- 0.05          # lower limit
  d  <- 1.1           # upper limit
  e  <- true_ic50     # IC50

  set.seed(42)
  conc_levels <- c(0.1, seq(2.5, 26, by = 2.5))
  conc <- rep(conc_levels, each = 5)

  # Generate response
  logistic_response <- function(conc, b, c, d, e) {
    c + (d - c) / (1 + (conc / e)^b)
  }
  abs <- logistic_response(conc, b, c, d, e) + rnorm(length(conc), sd = 0.05)

  data.frame(
    substance = rep(name, length(conc)), conc = conc, abs = abs
  )
}

# Test check fit
test_check_fit <- function(ic50_true) {
  df <- simulate("A", 7, ic50_true)
  model <- drc::drm(abs ~ conc,
    data = df, fct = drc::LL.4(),
    robust = "median"
  )
  valid_points <- OpenStats:::false_discovery_rate(residuals(model))
  model <- drc::drm(abs ~ conc,
    data = df,
    subset = valid_points,
    start = model$coefficients,
    fct = drc::LL.4(), robust = "mean",
  )
  conc <- "conc"
  abs <- "abs"
  title = "Bla"
  res <- OpenStats:::check_fit(
    model, min(df[, conc]),
    max(df[, conc]), min(df[, abs]), max(df[, abs]), title
  )
  expect_true(is.data.frame(res))
}
test_check_fit(10)

# Test ic50 internal
test_ic50_internal <- function(ic50_true) {
  data <- simulate("A", 7, ic50_true)
  res <- OpenStats:::ic50_internal(data, "abs", "conc", "substance", FALSE, FALSE)
  res_df <- res[[1]]
  tol_percentage <- 0.1
  rel_error <- function(a, b) {
    abs(b - a) / b
  }
  expect_true(rel_error(ic50_true, res_df$IC50_relative) < tol_percentage)
}
test_ic50_internal(0.6)
test_ic50_internal(5)
test_ic50_internal(10)
test_ic50_internal(20)

# Test cases for errorClass
test_errorClass <- function() {
  checks <- list()
  err <- OpenStats:::errorClass$new("An error message")
  checks[[1]] <- !err$isNull()
  checks[[2]] <- err$error_message == "An error message"
  err_null <- OpenStats:::errorClass$new()
  checks[[3]] <- err_null$isNull()
  all(unlist(checks))
}

# Test cases for env_lc_V1_2$shapenumber
test_shapenumber <- function() {
  checks <- list()
  checks[[1]] <- OpenStats:::shapenumber(123.456) == signif(123.456)
  checks[[2]] <- is.na(OpenStats:::shapenumber(Inf))
  checks[[3]] <- is.na(OpenStats:::shapenumber(-Inf))
  checks[[4]] <- is.na(OpenStats:::shapenumber(NA))
  all(unlist(checks))
}

# Test cases for env_lc_V1_2$robust_68_percentile
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

# Test cases for env_lc_V1_2$rsdr
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

# Test cases for env_lc_V1_2$false_discovery_rate
test_false_discovery_rate <- function() {
  checks <- list()
  residuals <- c(1, 2, 3, 4, 5)
  include <- OpenStats:::false_discovery_rate(residuals)
  checks[[1]] <- expect_true(is.logical(include))
  checks[[2]] <- expect_equal(length(include), length(residuals))
  all(unlist(checks))
}

# drawplot_only_raw_data
test_drawplot_only_raw_data <- function() {
  df <- simulate("A", 7, 2)
  p <- OpenStats:::drawplot_only_raw_data(df, "abs", "conc", "Bla")
  layers <- p$layers
  expect_true(inherits(layers[[1]]$geom, "GeomBoxplot"))
  expect_true(inherits(layers[[2]]$geom, "GeomPoint"))

  built <- ggplot2::ggplot_build(p)
  layers <- built$plot$layers[[1]]
  mapping <- layers$computed_mapping
  expect_equal(deparse(mapping$x), "~conc")
  expect_equal(deparse(mapping$y), "~abs")
  expect_equal(deparse(mapping$group), "~conc")

  built <- ggplot2::ggplot_build(p)
  layers <- built$plot$layers[[2]]
  mapping <- layers$computed_mapping
  expect_equal(deparse(mapping$x), "~conc")
  expect_equal(deparse(mapping$y), "~abs")
}
test_drawplot_only_raw_data()

# drawplot
test_drawplot <- function() {
  df <- simulate("A", 7, 11)
  model <- drc::drm(abs ~ conc,
    data = df, fct = drc::LL.4(),
    robust = "median"
  )
  valid_points <- OpenStats:::false_discovery_rate(residuals(model))
  model <- drc::drm(abs ~ conc,
    data = df,
    subset = valid_points,
    start = model$coefficients,
    fct = drc::LL.4(), robust = "mean",
  )
  conc <- "conc"
  abs <- "abs"
  title = "Bla"
  res <- OpenStats:::check_fit(
    model, min(df[, conc]),
    max(df[, conc]), min(df[, abs]), max(df[, abs]), title
  )
  p <- OpenStats:::drawplot(
    df, abs, conc, model, valid_points, title, res$IC50_relative,
    res$IC50_relative_lower, res$IC50_relative_higher,
    FALSE, FALSE
  )
  layers <- p$layers
  expect_true(inherits(layers[[1]]$geom, "GeomBoxplot"))
  expect_true(inherits(layers[[2]]$geom, "GeomPoint"))
  expect_true(inherits(layers[[3]]$geom, "GeomLine"))
  expect_true(inherits(layers[[4]]$geom, "GeomErrorbarh") || inherits(layers[[4]]$geom, "GeomErrorbar")) # GeomErrorbar for older ggplot2 versions

  built <- ggplot2::ggplot_build(p)
  layers <- built$plot$layers[[1]]
  mapping <- layers$computed_mapping
  expect_equal(deparse(mapping$x), "~conc")
  expect_equal(deparse(mapping$y), "~abs")
  expect_equal(deparse(mapping$group), "~conc")

  built <- ggplot2::ggplot_build(p)
  layers <- built$plot$layers[[2]]
  mapping <- layers$computed_mapping
  expect_equal(deparse(mapping$x), "~conc")
  expect_equal(deparse(mapping$y), "~abs")

  built <- ggplot2::ggplot_build(p)
  layers <- built$plot$layers[[3]]
  mapping <- layers$computed_mapping
  expect_equal(deparse(mapping$x), "~conc")
  expect_equal(deparse(mapping$y), "~abs")

  built <- ggplot2::ggplot_build(p)
  layers <- built$plot$layers[[4]]
  mapping <- layers$computed_mapping
  expect_equal(deparse(mapping$y), "~ymedian")
  expect_equal(deparse(mapping$xmin), "~xmin")
  expect_equal(deparse(mapping$xmax), "~xmax")
}
test_drawplot()

# check_dr_df
test_check_dr_df <- function() {
  df <- data.frame(abs = runif(10), conc = 1, substance = 1L)
  err <- OpenStats:::check_dr_df(df, "abs", "conc", "substance")
  expect_equal(err$error_message, "The substance names are not character")

  df <- data.frame(abs = "Bla", conc = 1, substance = "Bla")
  err <- OpenStats:::check_dr_df(df, "abs", "conc", "substance")
  expect_equal(err$error_message, "The absorbance data is not numerical")
}
test_check_dr_df()

# transform_conc_dr
transform_conc_dr <- function(conc_col) {
  temp_conc <- as.numeric(conc_col)
  if (all(is.na(temp_conc))) {
    return(errorClass$new(
      "The concentration data cannot be converted to numerical"
    ))
  }
  if (!is.numeric(temp_conc)) {
    return(errorClass$new("The concentration data is not numerical"))
  }
  return(temp_conc)
}
test_transform_conc_dr <- function() {
  df <- data.frame(abs = runif(10), conc = "Bla", substance = 1L)
  err <- OpenStats:::transform_conc_dr(df$conc)
  expect_equal(err$error_message, "The concentration data cannot be converted to numerical")

  df <- data.frame(abs = runif(10), conc = "1.2", substance = 1L)
  res <- OpenStats:::transform_conc_dr(df$conc)
  expect_true(is.numeric(res))
}
test_transform_conc_dr()

# Test cases for ic50
test_ic50 <- function() {
  checks <- list()
  data <- data.frame(
    abs = c(0.5, 0.6, 0.7, 0.8, 0.9),
    conc = c(1, 10, 100, 1000, 10000),
    names = c("A", "A", "A", "A", "A")
  )
  result <- OpenStats:::ic50(data, "abs", "conc", "names", FALSE, FALSE)
  checks[[1]] <- expect_true(is.list(result))
  checks[[2]] <- expect_true(is.data.frame(result[[1]][[1]]))
  all(unlist(checks))
}

# Run all tests
expect_true(test_errorClass())
expect_true(test_shapenumber())
expect_true(test_robust_68_percentile())
expect_true(test_rsdr())
expect_true(test_false_discovery_rate())
expect_true(test_ic50())
