env_summarising_model_V1_2 <- new.env(parent = getNamespace("OpenStats"))

determine_types <- function(vars, df) {
  vapply(vars, function(var) {
    classes <- class(df[, var])
    paste(classes, collapse = "")
  }, character(1))
}
env_summarising_model_V1_2$determine_types <- determine_types

create_new_numeric <- function(var, df, n = 100L, slicing = FALSE) {
  data <- df[, var]
  data <- data[!is.na(data)]
  if (slicing) {
    quantile(data, probs = c(0.1, 0.5, 0.9))
  } else {
    seq(min(data), max(data), length.out = n)
  }
}
env_summarising_model_V1_2$create_new_numeric <- create_new_numeric

create_new_non_numeric <- function(var, df) {
  unique(df[, var])
}
env_summarising_model_V1_2$create_new_non_numeric <- create_new_non_numeric

create_new_data <- function(formula, df, vars, n = 100L) {
  types <- env_summarising_model_V1_2$determine_types(vars, df)
  data <- Map(function(var, type, idx) {
    if (type == "numeric" || type == "integer") {
      if (types[1] == "numeric" && length(unique(df[, var])) > 10) {
        return(env_summarising_model_V1_2$create_new_numeric(var, df, n, idx > 1))
      } else {
        return(env_summarising_model_V1_2$create_new_non_numeric(var, df))
      }
    }
    env_summarising_model_V1_2$create_new_non_numeric(var, df)
  }, vars, types, seq_along(types))
  expand.grid(data, stringsAsFactors = FALSE)
}
env_summarising_model_V1_2$create_new_data <- create_new_data

get_predictions <- function(model, newdata, level = 0.95) {
  pred <- predict(model, newdata, se.fit = TRUE)

  alpha <- 1 - level
  z <- qnorm(1 - alpha / 2)  # 1.96 for 95% CI

  data.frame(
    newdata,
    predicted = pred$fit,
    conf.low = pred$fit - z * pred$se.fit,
    conf.high = pred$fit + z * pred$se.fit
  )
}
env_summarising_model_V1_2$get_predictions <- get_predictions

# nocov start plotting
add_y_lab_title <- function(p, ytitle) {
  p + labs(y = paste0("Predicted ", ytitle))
}
env_summarising_model_V1_2$add_y_lab_title <- add_y_lab_title
# nocov end plotting

plot_one_pred <- function(pred_df, type, pred, response) {
  predicted <- function() stop("Should never be called") # Please R CMD check
  conf.low <- function() stop("Should never be called") # Please R CMD check
  conf.high <- function() stop("Should never be called") # Please R CMD check

  aes_one <- aes(x = .data[[pred[1]]], y = predicted)
  p <- NULL
  if (type == "numeric") {
    p <- ggplot(data = pred_df) +
      geom_line(aes(!!!aes_one)) +
      geom_ribbon(
        aes(!!!aes_one, ymin = conf.low, ymax = conf.high),
        alpha = 0.2,
        color = NA
      )
  } else {
    p <- ggplot(data = pred_df) +
      geom_point(aes(!!!aes_one)) +
      geom_errorbar(aes(!!!aes_one, ymin = conf.low, ymax = conf.high), width = 0)
  }
  return(env_summarising_model_V1_2$add_y_lab_title(p, response))
}
env_summarising_model_V1_2$plot_one_pred <- plot_one_pred

plot_two_pred <- function(pred_df, types, preds, response) {
  predicted <- function() stop("Should never be called") # Please R CMD check
  conf.low <- function() stop("Should never be called") # Please R CMD check
  conf.high <- function() stop("Should never be called") # Please R CMD check

  aes_one <- aes(x = .data[[preds[1]]], y = predicted)
  p <- NULL
  if (types[1] == "numeric" && types[2] != "numeric") {
    aes_two <- aes(colour = .data[[preds[2]]])
    p <- ggplot(data = pred_df) + geom_line(aes(!!!aes_one, !!!aes_two)) +
      geom_ribbon(
        aes(!!!aes_one, fill = .data[[preds[2]]], group = .data[[preds[2]]], ymin = conf.low, ymax = conf.high),
        alpha = 0.2,
        color = NA
      )
  } else if (types[1] == "numeric" && types[2] == "numeric") {
    pred_df[, preds[2]] <- as.factor(pred_df[, preds[2]])
    aes_two <- aes(colour = .data[[preds[2]]])
    p <- ggplot(data = pred_df) + geom_line(aes(!!!aes_one, !!!aes_two)) +
      geom_ribbon(
        aes(!!!aes_one, fill = .data[[preds[2]]], group = .data[[preds[2]]], ymin = conf.low, ymax = conf.high),
        alpha = 0.2,
        color = NA
      )
  } else if (types[1] != "numeric" && types[2] == "numeric") {
    pred_df[, preds[2]] <- as.factor(pred_df[, preds[2]])
    aes_two <- aes(colour = .data[[preds[2]]], group = .data[[preds[2]]])
    p <- ggplot(data = pred_df) +
      geom_errorbar(aes(!!!aes_one, !!!aes_two, ymin = conf.low, ymax = conf.high),
        position = position_dodge(width = 0.9), width = 0) +
      geom_point(aes(!!!aes_one, !!!aes_two),
        position = position_dodge(width = 0.9))
  } else if (types[1] != "numeric" && types[2] != "numeric") {
    aes_two <- aes(colour = .data[[preds[2]]])
    p <- ggplot(data = pred_df) +
      geom_point(aes(!!!aes_one, !!!aes_two), position = position_dodge(width = 0.9)) +
      geom_errorbar(aes(!!!aes_one, !!!aes_two, ymin = conf.low, ymax = conf.high),
        position = position_dodge(width = 0.9), width = 0)
  }
  return(env_summarising_model_V1_2$add_y_lab_title(p, response))
}
env_summarising_model_V1_2$plot_two_pred <- plot_two_pred

plot_three_pred <- function(pred_df, types, preds, response) {
  p <- env_summarising_model_V1_2$plot_two_pred(pred_df, types, preds, response)
  custom_labels <- function(value) {
    paste(preds[3], " = ", value)
  }
  p + facet_wrap(~ .data[[preds[3]]], labeller = as_labeller(custom_labels))
}
env_summarising_model_V1_2$plot_three_pred <- plot_three_pred

plot_four_pred <- function(pred_df, types, preds, response) {
  p <- env_summarising_model_V1_2$plot_two_pred(pred_df, types, preds, response)
  pred_df[, preds[3]] <- as.factor(pred_df[, preds[3]])
  pred_df[, preds[4]] <- as.factor(pred_df[, preds[4]])
  p + facet_grid(
    rows = vars(.data[[preds[4]]]),
    cols = vars(.data[[preds[3]]]),
    labeller = label_both
  )
}
env_summarising_model_V1_2$plot_four_pred <- plot_four_pred

trim_formula_predictors <- function(formula, max_predictors = 4) {
  f_split <- env_utils_V1_2$split_formula(formula)
  rhs_vars <- env_utils_V1_2$vars_rhs(f_split$right_site)
  if (length(rhs_vars) > max_predictors) {
    rhs_vars <- rhs_vars[1:max_predictors]
    rhs_expr <- Reduce(function(x, y) call("+", x, y), lapply(rhs_vars, as.symbol))
    new_formula <- as.formula(call("~", f_split$response, rhs_expr))
    return(new_formula)
  }
  return(formula)
}
env_summarising_model_V1_2$trim_formula_predictors <- trim_formula_predictors

# nocov start plotting
add_theme_model_plot <- function(p) {
  p + theme(text = element_text(size = 20))
}
env_summarising_model_V1_2$add_theme_model_plot <- add_theme_model_plot

create_model_plot <- function(pred_df, types, predictors, response, r2_label) {
  if(length(types) == 1) {
    return(
      env_summarising_model_V1_2$plot_one_pred(pred_df, types, predictors, response) + labs(caption = r2_label) |> env_summarising_model_V1_2$add_theme_model_plot()
    )
  } else if (length(types) == 2) {
    return(
      env_summarising_model_V1_2$plot_two_pred(pred_df, types, predictors, response) + labs(caption = r2_label) |> env_summarising_model_V1_2$add_theme_model_plot()
    )
  } else if (length(types) == 3) {
    return(
      env_summarising_model_V1_2$plot_three_pred(pred_df, types, predictors, response) + labs(caption = r2_label) |> env_summarising_model_V1_2$add_theme_model_plot()
    )
  } else if (length(types) == 4) {
    return(
      env_summarising_model_V1_2$plot_four_pred(pred_df, types, predictors, response) + labs(caption = r2_label) |> env_summarising_model_V1_2$add_theme_model_plot()
    )
  } else {
    warning("Plotted only the first four effects")
    return(
      env_summarising_model_V1_2$plot_four_pred(pred_df, types[1:4], predictors[1:4], response) + labs(caption = r2_label) |> env_summarising_model_V1_2$add_theme_model_plot()
    )
  }
}
env_summarising_model_V1_2$create_model_plot <- create_model_plot
# nocov end plotting

plot_pred_lm <- function(data, formula) {
  # How different types are handeled:
  # 1. First type defines the x axis:
  #   a. factor --> geom_point
  #   b. numeric --> geom_line
  # 2. Second type
  #   If 1.a --> factor:
  #     a. factor --> geom_point as colour
  #     b. numeric --> all levels as factors as colour
  #   If 1.b --> numeric:
  #     c. factor --> geom_point as colour
  #     d. numeric --> quantile(0.1, 0.5, 0.9) as colour

  pred_df <- NULL
  types <- NULL
  predictors <- NULL
  response <- NULL
  r2_label <- NULL

  formula <- formula@formula
  f_split <- env_utils_V1_2$split_formula(formula)
  predictors <- env_utils_V1_2$vars_rhs(f_split$right_site)
  response <- all.vars(f_split$response)
  if (length(predictors) > 4) {
    formula <- env_summarising_model_V1_2$trim_formula_predictors(formula)
  }
  model <- lm(formula, data)
  # R²
  r2 <- summary(model)$r.squared
  r2_label <- sprintf("R^2 = %.3f", r2)
  n <- 100
  new_data <- env_summarising_model_V1_2$create_new_data(formula, data, predictors, n)
  types <- env_summarising_model_V1_2$determine_types(predictors, data)
  pred_df <- env_summarising_model_V1_2$get_predictions(model, new_data)
  env_summarising_model_V1_2$create_model_plot(pred_df, types, predictors, response, r2_label)
}
env_summarising_model_V1_2$plot_pred_lm <- plot_pred_lm

plot_pred_glm <- function(data, formula) {
  # How different types are handeled:
  # 1. First type defines the x axis:
  #   a. factor --> geom_point
  #   b. numeric --> geom_line
  # 2. Second type
  #   If 1.a --> factor:
  #     a. factor --> geom_point as colour
  #     b. numeric --> all levels as factors as colour
  #   If 1.b --> numeric:
  #     c. factor --> geom_point as colour
  #     d. numeric --> quantile(0.1, 0.5, 0.9) as colour

  pred_df <- NULL
  types <- NULL
  predictors <- NULL
  response <- NULL
  r2_label <- NULL

  family <- formula@family
  link_fct <- formula@link_fct
  formula <- formula@formula
  f_split <- env_utils_V1_2$split_formula(formula)
  predictors <- env_utils_V1_2$vars_rhs(f_split$right_site)
  response <- all.vars(f_split$response)
  if (length(predictors) > 4) {
    formula <- env_summarising_model_V1_2$trim_formula_predictors(formula)
  }
  family <- str2lang(paste0("stats::", family, "(\"", link_fct, "\")"))
  model <- glm(formula, data = data, family = eval(family))
  # R²
  r2 <- summary(model)$r.squared
  r2_label <- sprintf("R^2 = %.3f", r2)
  n <- 100
  new_data <- env_summarising_model_V1_2$create_new_data(formula, data, predictors, n)
  types <- env_summarising_model_V1_2$determine_types(predictors, data)
  pred_df <- env_summarising_model_V1_2$get_predictions(model, new_data)
  env_summarising_model_V1_2$create_model_plot(pred_df, types, predictors, response, r2_label)
}
env_summarising_model_V1_2$plot_pred_glm <- plot_pred_glm

create_information_criterions <- function(model) {
  out <- data.frame(AIC = AIC(model), BIC = BIC(model))
  if (inherits(model, "glm")) {
    dispersion <- summary(model)$dispersion
    out$Dispersion <- dispersion
  }
  return(out)
}
env_summarising_model_V1_2$create_information_criterions <- create_information_criterions


# This offers the user the option to directly visualise the data based on a model
# =================================================================================================
# nocov start plotting
add_desired_layer <- function(p, layer) {
  if (layer == "box") {
    p + geom_boxplot()
  } else if (layer == "dot") {
    p + geom_point()
  } else if (layer == "line") {
    p + geom_line()
  }
}
env_summarising_model_V1_2$add_desired_layer <- add_desired_layer
# nocov end plotting

plot_one <- function(df, type, pred, response, layer) {
  aes <- NULL
  if (layer == "box") {
    aes <- aes(x = .data[[pred[1]]], y = .data[[response]], group = .data[[pred[1]]])
  } else {
    aes <- aes(x = .data[[pred[1]]], y = .data[[response]])
  }
  p <- ggplot(data = df, aes(!!!aes))
  env_summarising_model_V1_2$add_desired_layer(p, layer)
}
env_summarising_model_V1_2$plot_one <- plot_one

plot_two <- function(df, types, preds, response, layer) {
  aes <- NULL
  if (layer == "box") {
    aes <- aes(x = .data[[preds[1]]], y = .data[[response]],
      fill = .data[[preds[2]]],
      group = interaction(.data[[preds[1]]], .data[[preds[2]]])
    )
  } else {
    aes <- aes(x = .data[[preds[1]]], y = .data[[response]], colour = .data[[preds[2]]])
  }
  p <- ggplot(data = df, aes(!!!aes))
  env_summarising_model_V1_2$add_desired_layer(p, layer)
}
env_summarising_model_V1_2$plot_two <- plot_two

plot_three <- function(df, types, preds, response, layer) {
  p <- env_summarising_model_V1_2$plot_two(df, types, preds, response, layer)
  custom_labels <- function(value) {
    paste(preds[3], " = ", value)
  }
  p + facet_wrap(~ .data[[preds[3]]], labeller = as_labeller(custom_labels))
}
env_summarising_model_V1_2$plot_three <- plot_three

plot_four <- function(df, types, preds, response, layer) {
  p <- env_summarising_model_V1_2$plot_two(df, types, preds, response, layer)
  df[, preds[3]] <- as.factor(df[, preds[3]])
  df[, preds[4]] <- as.factor(df[, preds[4]])
  p + facet_grid(
    rows = vars(.data[[preds[4]]]),
    cols = vars(.data[[preds[3]]]),
    labeller = label_both
  )
}
env_summarising_model_V1_2$plot_four <- plot_four

# nocov start plotting
plot_model <- function(data, formula, layer) {
  formula <- formula@formula
  f_split <- env_utils_V1_2$split_formula(formula)
  predictors <- env_utils_V1_2$vars_rhs(f_split$right_site)
  response <- all.vars(f_split$response)
  types <- env_summarising_model_V1_2$determine_types(predictors, data)
  if (length(predictors) > 4) {
    formula <- env_summarising_model_V1_2$trim_formula_predictors(formula)
  }
  if(length(types) == 1) {
    return(
      env_summarising_model_V1_2$plot_one(data, types, predictors, response, layer) |> env_summarising_model_V1_2$add_theme_model_plot()
    )
  } else if (length(types) == 2) {
    return(
      env_summarising_model_V1_2$plot_two(data, types, predictors, response, layer) |> env_summarising_model_V1_2$add_theme_model_plot()
    )
  } else if (length(types) == 3) {
    return(
      env_summarising_model_V1_2$plot_three(data, types, predictors, response, layer) |> env_summarising_model_V1_2$add_theme_model_plot()
    )
  } else if (length(types) == 4) {
    return(
      env_summarising_model_V1_2$plot_four(data, types, predictors, response, layer) |> env_summarising_model_V1_2$add_theme_model_plot()
    )
  } else {
    warning("Plotted only the first four effects")
    return(
      env_summarising_model_V1_2$plot_four(data, types[1:4], predictors[1:4], response, layer) |> env_summarising_model_V1_2$add_theme_model_plot()
    )
  }
}
env_summarising_model_V1_2$plot_model <- plot_model
# nocov end plotting
