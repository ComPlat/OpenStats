library(tinytest)
library(OpenStats)

# Residuals vs Leverage plots
# -----------------------------------------------------------------------
test_resids_vs_leverage_mapping <- function(p) {
  built <- ggplot2::ggplot_build(p)
  labels <- built$plot$labels
  expect_equal(labels$y, "Standardized residuals")
  expect_equal(labels$x, "Leverage")
  expect_equal(labels$title, "Residuals vs Leverage")

  layers <- built$plot$layers[[4]]
  mapping <- layers$computed_mapping
  expect_equal("~leverage", deparse(mapping$x))
  expect_equal("~residuals", deparse(mapping$y))
}
test_resids_vs_leverage_plot_layers <- function(p) {
  layers <- p$layers
  expect_equal(length(layers), 4L)
  expect_true(inherits(layers[[1]]$geom, "GeomPoint"))
  expect_true(inherits(layers[[2]]$geom, "GeomLine"))
  expect_true(inherits(layers[[3]]$geom, "GeomHline"))
  expect_true(inherits(layers[[4]]$geom, "GeomText"))
}
test_resids_vs_leverage_plot <- function() {
  df <- CO2
  formula <- new("LinearFormula", formula = uptake ~ conc)
    model <- lm(formula@formula, data = df)
  resids <- residuals(model)
  fitted <- fitted(model)

  # Identify influential points
  p <- length(coef(model))
  n <- length(resids)
  leverage <- hatvalues(model)
  cooks_dist <- cooks.distance(model)
  high_leverage_threshold <- (2 * (p + 1)) / n
  high_cooks_threshold <- 4 / n
  influential_points <- which(leverage > high_leverage_threshold | cooks_dist > high_cooks_threshold)
  pl <- OpenStats:::residuals_vs_leverage_plot(leverage, resids, n, formula, influential_points)
  test_resids_vs_leverage_plot_layers(pl)
  test_resids_vs_leverage_mapping(pl)
  invisible(NULL)
}
test_resids_vs_leverage_plot()

# Manual scale loaction
# -----------------------------------------------------------------------
test_manual_scale_location_plot_mapping <- function(p) {
  built <- ggplot2::ggplot_build(p)
  labels <- built$plot$labels
  expect_equal(as.character(labels$y), "sqrt(\"Standardized residuals\")") # TODO: check why this is wrapped in expression
  expect_equal(labels$x, "Fitted values")
  expect_equal(labels$title, "Scale-Location")

  layers <- built$plot$layers[[3]]
  mapping <- layers$computed_mapping
  expect_equal(deparse(mapping$label), "~index")
  expect_equal(deparse(mapping$x), "~fitted")
  expect_equal(deparse(mapping$y), "~residuals")
}
test_manual_scale_location_plot_layers <- function(p) {
  layers <- p$layers
  expect_equal(length(layers), 3L)
  expect_true(inherits(layers[[1]]$geom, "GeomPoint"))
  expect_true(inherits(layers[[2]]$geom, "GeomLine"))
  expect_true(inherits(layers[[3]]$geom, "GeomText"))
}
test_manual_scale_location_plot <- function() {
  df <- CO2
  formula <- new("LinearFormula", formula = uptake ~ conc)
  model <- lm(formula@formula, data = df)
  resids <- residuals(model)
  fitted <- fitted(model)

  # Identify influential points
  p <- length(coef(model))
  n <- length(resids)
  leverage <- hatvalues(model)
  cooks_dist <- cooks.distance(model)
  high_leverage_threshold <- (2 * (p + 1)) / n
  high_cooks_threshold <- 4 / n
  influential_points <- which(leverage > high_leverage_threshold | cooks_dist > high_cooks_threshold)
  pl <- OpenStats:::manual_scale_location_plot(fitted, resids, n, formula, influential_points)
  test_manual_scale_location_plot_mapping(pl)
  test_manual_scale_location_plot_layers(pl)
}
test_manual_scale_location_plot()

# QQ norm plot
# -----------------------------------------------------------------------
test_qq_norm_mapping <- function(p) {
  built <- ggplot2::ggplot_build(p)
  labels <- built$plot$labels
  expect_equal(labels$y, "Standardized residuals")
  expect_equal(labels$x, "Theoretical Quantiles")
  expect_equal(labels$title, "Q-Q Residuals")

  layers <- built$plot$layers[[3]]
  mapping <- layers$computed_mapping
  expect_equal(deparse(mapping$label), "~index")
  expect_equal(deparse(mapping$x), "~quantiles")
  expect_equal(deparse(mapping$y), "~residuals")
}
test_qq_norm_layers <- function(p) {
  layers <- p$layers
  expect_equal(length(layers), 3L)
  expect_true(inherits(layers[[1]]$geom, "GeomPoint"))
  expect_true(inherits(layers[[2]]$geom, "GeomAbline"))
  expect_true(inherits(layers[[3]]$geom, "GeomText"))
}
test_qq_norm_plot <- function() {
  df <- CO2
  formula <- new("LinearFormula", formula = uptake ~ conc)
    model <- lm(formula@formula, data = df)
  resids <- residuals(model)
  fitted <- fitted(model)

  # Identify influential points
  p <- length(coef(model))
  n <- length(resids)
  leverage <- hatvalues(model)
  cooks_dist <- cooks.distance(model)
  high_leverage_threshold <- (2 * (p + 1)) / n
  high_cooks_threshold <- 4 / n
  influential_points <- which(leverage > high_leverage_threshold | cooks_dist > high_cooks_threshold)
  pl <- OpenStats:::qq_norm_plot(resids, n, formula, influential_points)
  test_qq_norm_mapping(pl)
  test_qq_norm_layers(pl)
}
test_qq_norm_plot()

# Residuals vs Fitted
# -----------------------------------------------------------------------
test_resids_vs_fitted_mapping <- function(p) {
  built <- ggplot2::ggplot_build(p)
  labels <- built$plot$labels
  expect_equal(labels$y, "Residuals")
  expect_equal(labels$x, "Fitted values")
  expect_equal(labels$title, "Residuals vs Fitted values")

  layers <- built$plot$layers[[4]]
  mapping <- layers$computed_mapping
  expect_equal("~fitted", deparse(mapping$x))
  expect_equal("~residuals", deparse(mapping$y))
}
test_resids_vs_fitted_plot_layers <- function(p) {
  layers <- p$layers
  expect_equal(length(layers), 4L)
  expect_true(inherits(layers[[1]]$geom, "GeomPoint"))
  expect_true(inherits(layers[[2]]$geom, "GeomHline"))
  expect_true(inherits(layers[[3]]$geom, "GeomLine"))
  expect_true(inherits(layers[[4]]$geom, "GeomText"))
}

test_resids_vs_fitted_plot <- function() {
  df <- CO2
  formula <- new("LinearFormula", formula = uptake ~ conc)
    model <- lm(formula@formula, data = df)
  resids <- residuals(model)
  fitted <- fitted(model)

  # Identify influential points
  p <- length(coef(model))
  n <- length(resids)
  leverage <- hatvalues(model)
  cooks_dist <- cooks.distance(model)
  high_leverage_threshold <- (2 * (p + 1)) / n
  high_cooks_threshold <- 4 / n
  influential_points <- which(leverage > high_leverage_threshold | cooks_dist > high_cooks_threshold)
  pl <- OpenStats:::resids_vs_fitted_plot(fitted, resids, n, formula, influential_points)
  test_resids_vs_fitted_plot_layers(pl)
  test_resids_vs_fitted_mapping(pl)
}
test_resids_vs_fitted_plot()

# cooks distance plot
# -----------------------------------------------------------------------
test_cooks_distance_mapping <- function() {
  formula <- new("LinearFormula", formula = uptake ~ conc)
  p <- OpenStats:::cooks_distance_plot(CO2, formula)
  built <- ggplot2::ggplot_build(p)
  labels <- built$plot$labels
  expect_equal(labels$y, "CooksDistance")
  expect_equal(labels$x, "Index")

  layers <- built$plot$layers[[4]]
  mapping <- layers$computed_mapping
  expect_equal("~Index", deparse(mapping$x))
  expect_equal("~CooksDistance", deparse(mapping$y))
}
test_cooks_distance_mapping()

test_cooks_distance_cutoff <- function() {
  df <- CO2
  formula <- new("LinearFormula", formula = uptake ~ conc)
  p <- OpenStats:::cooks_distance_plot(df, formula)
  built <- ggplot2::ggplot_build(p)
  hline_layer <- built$plot$layers[[3]]
  hline_data <- ggplot2::layer_data(built, 3)
  n <- nrow(df)
  model <- lm(uptake ~ conc, data = df)
  k <- length(coef(model)) - 1
  cutoff <- 4 / (n - k - 1)
  expect_equal(hline_data$yintercept, cutoff)
  expect_equal(hline_layer$aes_params$linetype, "dashed")
}
test_cooks_distance_cutoff()

test_cooks_distance_labels_filtered <- function() {
  df <- CO2
  model <- lm(uptake ~ conc, data = df)
  k <- length(coef(model)) - 1
  cutoff <- 4 / (nrow(df) - k - 1)

  cd <- cooks.distance(model)
  cd <- data.frame(CooksDistance = cd,
    Index = 1:length(cd))
  cutted_off_data <- cd[cd$CooksDistance > cutoff, ]

  formula <- new("LinearFormula", formula = uptake ~ conc)
  p <- OpenStats:::cooks_distance_plot(df, formula)
  built <- ggplot2::ggplot_build(p)
  text_data <- ggplot2::layer_data(built, 4)
  cutted_off_from_plot <- ggplot2::layer_data(built, 4)

  expect_equal(cutted_off_data$Index, cutted_off_from_plot$label)
  expect_equal(cutted_off_data$CooksDistance, cutted_off_from_plot$y)
}
test_cooks_distance_labels_filtered()

test_cooks_distance_caption_linear_only <- function() {
  p1 <- OpenStats:::cooks_distance_plot(CO2,
    new("LinearFormula", formula = uptake ~ conc)
  )
  p2 <- OpenStats:::cooks_distance_plot(CO2,
    new("GeneralisedLinearFormula", formula = uptake ~ conc,
        family = "gaussian", link_fct = "identity")
  )

  built1 <- ggplot2::ggplot_build(p1)
  built2 <- ggplot2::ggplot_build(p2)
  expect_false(is.null(built1$plot$labels$caption))
  expect_true(is.null(built2$plot$labels$caption))
}
test_cooks_distance_caption_linear_only()

test_cooks_distance_layers_structure <- function() {
  p <- OpenStats:::cooks_distance_plot(CO2,
    new("LinearFormula", formula = uptake ~ conc)
  )
  layers <- p$layers
  expect_equal(length(layers), 4L)
  expect_true(inherits(layers[[1]]$geom, "GeomPoint"))
  expect_true(inherits(layers[[2]]$geom, "GeomPath"))
  expect_true(inherits(layers[[3]]$geom, "GeomHline"))
  expect_true(inherits(layers[[4]]$geom, "GeomText"))
}
test_cooks_distance_layers_structure()

test_cooks_distance_mapping_glm <- function() {
  formula <- new("GeneralisedLinearFormula", formula = uptake ~ conc, family = "gaussian", link_fct = "identity")
  p <- OpenStats:::cooks_distance_plot(CO2, formula)
  built <- ggplot2::ggplot_build(p)
  labels <- built$plot$labels
  expect_equal(labels$y, "CooksDistance")
  expect_equal(labels$x, "Index")

  layers <- built$plot$layers[[4]]
  mapping <- layers$computed_mapping
  expect_equal("~Index", deparse(mapping$x))
  expect_equal("~CooksDistance", deparse(mapping$y))
}
test_cooks_distance_mapping_glm()

test_synthetic_influential_dataset <- function() {
  df <- data.frame(
    y = c(1, 2, 3, 100),
    x = c(1, 2, 3, 4)
  )
  f <- new("LinearFormula", formula = y ~ x)
  p <- OpenStats:::cooks_distance_plot(df, f)
  built <- ggplot2::ggplot_build(p)
  text_data <- ggplot2::layer_data(built, 4)
  expect_true(nrow(text_data) >= 1)
  expect_true(4 %in% text_data$label)
}
test_synthetic_influential_dataset()
