library(tinytest)
library(OpenStats)

# Dummy data
df <- data.frame(
  y = rnorm(10),
  x1 = rnorm(10),
  x2 = factor(sample(letters[1:3], 10, replace = TRUE))
)
rhs_vars <- c("x1", "x2")

# ---- Test env_summarising_model_V1_2$determine_types ----
types <- OpenStats:::determine_types(rhs_vars, df)
expect_equal(types, c(x1 = "numeric", x2 = "factor"))

# ---- Test env_summarising_model_V1_2$create_new_numeric ----
num_seq <- OpenStats:::create_new_numeric("x1", df, n = 5)
expect_equal(length(num_seq), 5)
expect_true(all(num_seq >= min(df$x1)) && all(num_seq <= max(df$x1)))

num_quant <- OpenStats:::create_new_numeric("x1", df, slicing = TRUE)
expect_equal(names(num_quant), c("10%", "50%", "90%"))

# ---- Test env_summarising_model_V1_2$create_new_non_numeric ----
non_num_levels <- OpenStats:::create_new_non_numeric("x2", df)
expect_true(all(non_num_levels %in% levels(df$x2)))

# ---- Test env_summarising_model_V1_2$create_new_data ----
new_data <- OpenStats:::create_new_data(f, df, c("x1", "x2"), n = 5)
expect_true(all(c("x1", "x2") %in% names(new_data)))

# ---- Test env_summarising_model_V1_2$get_predictions ----
model <- lm(y ~ x1 + x2, data = df)
pred_df <- OpenStats:::get_predictions(model, new_data)
expect_true(all(c("predicted", "conf.low", "conf.high") %in% names(pred_df)))
expect_equal(nrow(pred_df), nrow(new_data))

#plot_one_pred
test_plot_one_pred <- function() {
  set.seed(42)
  df <- data.frame(
    y = rnorm(10),
    x1 = rnorm(10)
  )
  rhs_vars <- c("x1")
  model <- lm(y ~ x1, data = df)
  new_data <- OpenStats:::create_new_data(f, df, "x1", n = 5)
  pred_df <- OpenStats:::get_predictions(model, new_data)
  types <- OpenStats:::determine_types(rhs_vars, df)
  p <- OpenStats:::plot_one_pred(pred_df, types[1L], "x1", "y")
  layers <- p$layers
  expect_true(inherits(layers[[1]]$geom, "GeomLine"))
  expect_true(inherits(layers[[2]]$geom, "GeomRibbon"))

  built <- ggplot2::ggplot_build(p)
  layers <- built$plot$layers[[1]]
  mapping <- layers$computed_mapping
  expect_equal("~.data[[\"x1\"]]", deparse(mapping$x))
  expect_equal("~predicted", deparse(mapping$y))
}
test_plot_one_pred()

#plot_two_pred
test_plot_two_pred <- function() {
  set.seed(42)
  df <- data.frame(
    y = rnorm(10),
    x1 = rnorm(10),
    x2 = factor(sample(letters[1:3], 10, replace = TRUE))
  )
  rhs_vars <- c("x1", "x2")
  model <- lm(y ~ x1 + x2, data = df)
  new_data <- OpenStats:::create_new_data(f, df, c("x1", "x2"), n = 5)
  pred_df <- OpenStats:::get_predictions(model, new_data)
  types <- OpenStats:::determine_types(rhs_vars, df)
  p <- OpenStats:::plot_two_pred(pred_df, types, c("x1", "x2"), "y")
  layers <- p$layers
  expect_true(inherits(layers[[1]]$geom, "GeomLine"))
  expect_true(inherits(layers[[2]]$geom, "GeomRibbon"))

  built <- ggplot2::ggplot_build(p)
  layers <- built$plot$layers[[1]]
  mapping <- layers$computed_mapping
  expect_equal("~.data[[\"x1\"]]", deparse(mapping$x))
  expect_equal("~.data[[\"x2\"]]", deparse(mapping$colour))
  expect_equal("~predicted", deparse(mapping$y))
}
test_plot_two_pred()

# plot_three_pred
test_plot_three_pred <- function() {
  set.seed(42)
  df <- data.frame(
    y = rnorm(10),
    x1 = rnorm(10),
    x2 = rnorm(10),
    x3 = factor(sample(letters[1:3], 10, replace = TRUE))
  )
  rhs_vars <- c("x1", "x2", "x3")
  model <- lm(y ~ x1 + x2 + x3, data = df)
  new_data <- OpenStats:::create_new_data(f, df, c("x1", "x2", "x3"), n = 5)
  pred_df <- OpenStats:::get_predictions(model, new_data)
  types <- OpenStats:::determine_types(rhs_vars, df)
  p <- OpenStats:::plot_three_pred(pred_df, types, c("x1", "x2", "x3"), "y")
  layers <- p$layers
  expect_true(inherits(layers[[1]]$geom, "GeomLine"))
  expect_true(inherits(layers[[2]]$geom, "GeomRibbon"))

  built <- ggplot2::ggplot_build(p)
  layers <- built$plot$layers[[1]]
  mapping <- layers$computed_mapping
  expect_equal("~.data[[\"x1\"]]", deparse(mapping$x))
  expect_equal("~.data[[\"x2\"]]", deparse(mapping$colour))
  expect_equal("~predicted", deparse(mapping$y))

  bl <- built$layout
  expect_equal(as.character(bl$layout[1, 4]), "a")
  expect_equal(as.character(bl$layout[2, 4]), "b")
  expect_equal(as.character(bl$layout[3, 4]), "c")
}
test_plot_three_pred()

# plot_four_pred
test_plot_four_pred <- function() {
  set.seed(42)
  df <- data.frame(
    y = rnorm(10),
    x1 = rnorm(10),
    x2 = rnorm(10),
    x3 = factor(sample(letters[1:3], 10, replace = TRUE)),
    x4 = factor(sample(letters[4:6], 10, replace = TRUE))
  )
  rhs_vars <- c("x1", "x2", "x3", "x4")
  model <- lm(y ~ x1 + x2 + x3, data = df)
  new_data <- OpenStats:::create_new_data(f, df, c("x1", "x2", "x3", "x4"), n = 5)
  pred_df <- OpenStats:::get_predictions(model, new_data)
  types <- OpenStats:::determine_types(rhs_vars, df)
  p <- OpenStats:::plot_four_pred(pred_df, types, c("x1", "x2", "x3", "x4"), "y")
  layers <- p$layers
  expect_true(inherits(layers[[1]]$geom, "GeomLine"))
  expect_true(inherits(layers[[2]]$geom, "GeomRibbon"))

  built <- ggplot2::ggplot_build(p)
  layers <- built$plot$layers[[1]]
  mapping <- layers$computed_mapping
  expect_equal("~.data[[\"x1\"]]", deparse(mapping$x))
  expect_equal("~.data[[\"x2\"]]", deparse(mapping$colour))
  expect_equal("~predicted", deparse(mapping$y))

  bl <- built$layout
  expect_equal(as.character(bl$layout[1:3, "x4"]), rep("d", 3))
  expect_equal(as.character(bl$layout[4:6, "x4"]), rep("e", 3))
  expect_equal(as.character(bl$layout[7:9, "x4"]), rep("f", 3))
  expect_equal(as.character(bl$layout[c(1, 4, 7), "x3"]), rep("a", 3))
  expect_equal(as.character(bl$layout[c(2, 5, 8), "x3"]), rep("b", 3))
  expect_equal(as.character(bl$layout[c(3, 6, 9), "x3"]), rep("c", 3))
}
test_plot_four_pred()

# test_trim predictors
test_trim_predictors <- function() {
  f <- y ~ x1 + x2
  out <- OpenStats:::trim_formula_predictors(f, max_predictors = 4)
  expect_equal(out, f)

  f <- y ~ x1 + x2 + x3 + x4 + x5 + x6
  out <- OpenStats:::trim_formula_predictors(f)  # max=4
  expect_true(inherits(out, "formula"))
  expect_equal(out, y ~ x1 + x2 + x3 + x4)

  f <- y ~ a + b + c + d + e
  out <- OpenStats:::trim_formula_predictors(f, max_predictors = 2)
  expect_equal(out, y ~ a + b)

  f <- y ~ z + x + w + v
  out <- OpenStats:::trim_formula_predictors(f, max_predictors = 3)
  expect_equal(out, y ~ z + x + w)

  f <- y ~ x1 + x2 + x3 + x4
  out <- OpenStats:::trim_formula_predictors(f, max_predictors = 4)
  expect_equal(out, f)
}
test_trim_predictors()

# plot pred lm
test_plot_pred_lm <- function() {
  formula <- new("LinearFormula", formula = uptake ~ conc)
  p <- OpenStats:::plot_pred_lm(CO2, formula)
  expect_true(inherits(p, "ggplot")) # the internals are already tested elsewhere
}
test_plot_pred_lm()

# plot pred glm
test_plot_pred_glm <- function() {
  formula <- new("GeneralisedLinearFormula", formula = uptake ~ conc, family = "gaussian", link_fct = "identity")
  p <- OpenStats:::plot_pred_glm(CO2, formula)
  expect_true(inherits(p, "ggplot")) # the internals are already tested elsewhere
}
test_plot_pred_glm()

# test information criterions
test_create_information_criterions_lm <- function() {
  m <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  out <- OpenStats:::create_information_criterions(m)

  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 1L)
  expect_equal(colnames(out), c("AIC", "BIC"))

  expect_equal(out$AIC, AIC(m))
  expect_equal(out$BIC, BIC(m))
}
test_create_information_criterions_lm()

test_create_information_criterions_glm <- function() {
  m <- glm(Species == "setosa" ~ Sepal.Length + Sepal.Width,
           data = iris,
           family = binomial())
  out <- OpenStats:::create_information_criterions(m)

  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 1L)
  expect_equal(colnames(out), c("AIC", "BIC", "Dispersion"))

  expect_equal(out$AIC, AIC(m))
  expect_equal(out$BIC, BIC(m))
  expect_equal(out$Dispersion, summary(m)$dispersion)
}
test_create_information_criterions_glm()

# testing plotting model on orginail data
test_plot_one <- function() {
  set.seed(42)
  df <- data.frame(
    y = rnorm(10),
    x1 = rnorm(10)
  )
  rhs_vars <- "x1"
  types <- OpenStats:::determine_types(rhs_vars, df)
  p <- OpenStats:::plot_one(df, types[1L], rhs_vars, "y", "dot")
  layers <- p$layers
  expect_true(inherits(layers[[1]]$geom, "GeomPoint"))

  built <- ggplot2::ggplot_build(p)
  layers <- built$plot$layers[[1]]
  mapping <- layers$computed_mapping
  expect_equal("~.data[[\"x1\"]]", deparse(mapping$x))
  expect_equal("~.data[[\"y\"]]", deparse(mapping$y))
}
test_plot_one()

test_plot_two <- function() {
  set.seed(42)
  df <- data.frame(
    y = rnorm(10),
    x1 = rnorm(10),
    x2 = factor(sample(letters[1:3], 10, replace = TRUE))
  )
  rhs_vars <- c("x1", "x2")
  types <- OpenStats:::determine_types(rhs_vars, df)
  p <- OpenStats:::plot_two(df, types[1L], rhs_vars, "y", "dot")
  layers <- p$layers
  expect_true(inherits(layers[[1]]$geom, "GeomPoint"))

  built <- ggplot2::ggplot_build(p)
  layers <- built$plot$layers[[1]]
  mapping <- layers$computed_mapping
  expect_equal("~.data[[\"x1\"]]", deparse(mapping$x))
  expect_equal("~.data[[\"y\"]]", deparse(mapping$y))
  expect_equal("~.data[[\"x2\"]]", deparse(mapping$colour))
}
test_plot_two()

test_plot_three <- function() {
  set.seed(42)
  df <- data.frame(
    y = rnorm(10),
    x1 = rnorm(10),
    x2 = factor(sample(letters[1:3], 10, replace = TRUE)),
    x3 = factor(sample(letters[4:6], 10, replace = TRUE))
  )
  rhs_vars <- c("x1", "x2", "x3")
  types <- OpenStats:::determine_types(rhs_vars, df)
  p <- OpenStats:::plot_three(df, types[1L], rhs_vars, "y", "dot")
  layers <- p$layers
  expect_true(inherits(layers[[1]]$geom, "GeomPoint"))

  built <- ggplot2::ggplot_build(p)
  layers <- built$plot$layers[[1]]
  mapping <- layers$computed_mapping
  expect_equal("~.data[[\"x1\"]]", deparse(mapping$x))
  expect_equal("~.data[[\"y\"]]", deparse(mapping$y))
  expect_equal("~.data[[\"x2\"]]", deparse(mapping$colour))

  bl <- built$layout
  expect_equal(as.character(bl$layout[1, 4]), "d")
  expect_equal(as.character(bl$layout[2, 4]), "e")
  expect_equal(as.character(bl$layout[3, 4]), "f")
}
test_plot_three()

test_plot_four <- function() {
  set.seed(42)
  df <- data.frame(
    y = rnorm(10),
    x1 = rnorm(10),
    x2 = factor(sample(letters[1:3], 10, replace = TRUE)),
    x3 = factor(sample(letters[4:6], 10, replace = TRUE)),
    x4 = rnorm(10)
  )
  rhs_vars <- c("x1", "x2", "x3", "x4")
  types <- OpenStats:::determine_types(rhs_vars, df)
  p <- OpenStats:::plot_four(df, types[1L], rhs_vars, "y", "dot")
  layers <- p$layers
  expect_true(inherits(layers[[1]]$geom, "GeomPoint"))

  built <- ggplot2::ggplot_build(p)
  layers <- built$plot$layers[[1]]
  mapping <- layers$computed_mapping
  expect_equal("~.data[[\"x1\"]]", deparse(mapping$x))
  expect_equal("~.data[[\"y\"]]", deparse(mapping$y))
  expect_equal("~.data[[\"x2\"]]", deparse(mapping$colour))

  bl <- built$layout
  expect_equal(names(bl$layout)[4], "x4")
  expect_equal(names(bl$layout)[5], "x3")
}
test_plot_four()
