split_formula <- function(formula) {
  f <- as.character(formula)
  list(
    response = str2lang(f[2]),
    right_site = str2lang(f[3])
  )
}

vars_rhs <- function(rhs) {
 all.vars(rhs)
}

determine_types <- function(vars, df) {
  vapply(vars, function(var) {
    classes <- class(df[, var])
    paste(classes, collapse = "")
  }, character(1))
}

create_new_numeric <- function(var, df, n = 100L, slicing = FALSE) {
  data <- df[, var]
  data <- data[!is.na(data)]
  if (slicing) {
    quantile(data, probs = c(0.1, 0.5, 0.9))
  } else {
    seq(min(data), max(data), length.out = n)
  }
}

create_new_non_numeric <- function(var, df) {
  unique(df[, var])
}

create_new_data <- function(formula, df, vars, n = 100L) {
  types <- determine_types(vars, df)
  data <- Map(function(var, type, idx) {
    if (type == "numeric" || type == "integer") {
      if (types[1] == "numeric" && length(unique(df[, var])) > 10) {
        return(create_new_numeric(var, df, n, idx > 1))
      } else {
        return(create_new_non_numeric(var, df))
      }
    }
    create_new_non_numeric(var, df)
  }, vars, types, seq_along(types))
  expand.grid(data, stringsAsFactors = FALSE)
}

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

add_y_lab_title <- function(p, ytitle) {
  p + labs(y = paste0("Predicted ", ytitle))
}

plot_one_pred <- function(pred_df, type, pred, response) {
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
  return(add_y_lab_title(p, response))
}
plot_two_pred <- function(pred_df, types, preds, response) {
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
  return(add_y_lab_title(p, response))
}
plot_three_pred <- function(pred_df, types, preds, response) {
  p <- plot_two_pred(pred_df, types, preds, response)
  custom_labels <- function(value) {
    paste(preds[3], " = ", value)
  }
  p + facet_wrap(~ .data[[preds[3]]], labeller = as_labeller(custom_labels))
}
plot_four_pred <- function(pred_df, types, preds, response) {
  p <- plot_two_pred(pred_df, types, preds, response)
  pred_df[, preds[3]] <- as.factor(pred_df[, preds[3]])
  pred_df[, preds[4]] <- as.factor(pred_df[, preds[4]])
  p + facet_grid(
    rows = vars(.data[[preds[4]]]),
    cols = vars(.data[[preds[3]]]),
    labeller = label_both
  )
}

trim_formula_predictors <- function(formula, max_predictors = 4) {
  f_split <- split_formula(formula)
  rhs_vars <- vars_rhs(f_split$right_site)
  if (length(rhs_vars) > max_predictors) {
    rhs_vars <- rhs_vars[1:max_predictors]
    rhs_expr <- Reduce(function(x, y) call("+", x, y), lapply(rhs_vars, as.symbol))
    new_formula <- as.formula(call("~", f_split$response, rhs_expr))
    return(new_formula)
  }
  return(formula)
}

add_theme_model_plot <- function(p) {
  p + theme(text = element_text(size = 20))
}

create_model_plot <- function(pred_df, types, predictors, response, r2_label) {
  if(length(types) == 1) {
    return(
      plot_one_pred(pred_df, types, predictors, response) + labs(caption = r2_label) |> add_theme_model_plot()
    )
  } else if (length(types) == 2) {
    return(
      plot_two_pred(pred_df, types, predictors, response) + labs(caption = r2_label) |> add_theme_model_plot()
    )
  } else if (length(types) == 3) {
    return(
      plot_three_pred(pred_df, types, predictors, response) + labs(caption = r2_label) |> add_theme_model_plot()
    )
  } else if (length(types) == 4) {
    return(
      plot_four_pred(pred_df, types, predictors, response) + labs(caption = r2_label) |> add_theme_model_plot()
    )
  } else {
    warning("Plotted only the first four effects")
    return(
      plot_four_pred(pred_df, types[1:4], predictors[1:4], response) + labs(caption = r2_label) |> add_theme_model_plot()
    )
  }
}

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
  f_split <- split_formula(formula)
  predictors <- vars_rhs(f_split$right_site)
  response <- all.vars(f_split$response)
  if (length(predictors) > 4) {
    formula <- trim_formula_predictors(formula)
  }
  model <- lm(formula, data)
  # R²
  r2 <- summary(model)$r.squared
  r2_label <- sprintf("R² = %.3f", r2)
  n <- 100
  new_data <- create_new_data(formula, data, predictors, n)
  types <- determine_types(predictors, data)
  pred_df <- get_predictions(model, new_data)
  create_model_plot(pred_df, types, predictors, response, r2_label)
}

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
  f_split <- split_formula(formula)
  predictors <- vars_rhs(f_split$right_site)
  response <- all.vars(f_split$response)
  if (length(predictors) > 4) {
    formula <- trim_formula_predictors(formula)
  }
  family <- str2lang(paste0("stats::", family, "(\"", link_fct, "\")"))
  model <- glm(formula, data = data, family = eval(family))
  # R²
  r2 <- summary(model)$r.squared
  r2_label <- sprintf("R² = %.3f", r2)
  n <- 100
  new_data <- create_new_data(formula, data, predictors, n)
  types <- determine_types(predictors, data)
  pred_df <- get_predictions(model, new_data)
  create_model_plot(pred_df, types, predictors, response, r2_label)
}

create_information_criterions <- function(model) {
  out <- data.frame(AIC = AIC(model), BIC = BIC(model))
  if (inherits(model, "glm")) {
    dispersion <- summary(model)$dispersion
    out$Dispersion <- dispersion
  }
  return(out)
}


# This offers the user the option to directly visualise the data based on a model
# =================================================================================================
add_desired_layer <- function(p, layer) {
  if (layer == "box") {
    p + geom_boxplot()
  } else if (layer == "dot") {
    p + geom_point()
  } else if (layer == "line") {
    p + geom_line()
  }
}

plot_one <- function(df, type, pred, response, layer) {
  aes <- NULL
  if (layer == "box") {
    aes <- aes(x = .data[[pred[1]]], y = .data[[response]], group = .data[[pred[1]]])
  } else {
    aes <- aes(x = .data[[pred[1]]], y = .data[[response]])
  }
  p <- ggplot(data = df, aes(!!!aes))
  add_desired_layer(p, layer)
}
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
  add_desired_layer(p, layer)
}
plot_three <- function(df, types, preds, response, layer) {
  p <- plot_two(df, types, preds, response, layer)
  custom_labels <- function(value) {
    paste(preds[3], " = ", value)
  }
  p + facet_wrap(~ .data[[preds[3]]], labeller = as_labeller(custom_labels))
}
plot_four <- function(df, types, preds, response, layer) {
  p <- plot_two(df, types, preds, response, layer)
  df[, preds[3]] <- as.factor(df[, preds[3]])
  df[, preds[4]] <- as.factor(df[, preds[4]])
  p + facet_grid(
    rows = vars(.data[[preds[4]]]),
    cols = vars(.data[[preds[3]]]),
    labeller = label_both
  )
}

plot_model <- function(data, formula, layer) {
  formula <- formula@formula
  f_split <- split_formula(formula)
  predictors <- vars_rhs(f_split$right_site)
  response <- all.vars(f_split$response)
  types <- determine_types(predictors, data)
  if (length(predictors) > 4) {
    formula <- trim_formula_predictors(formula)
  }
  if(length(types) == 1) {
    return(
      plot_one(data, types, predictors, response, layer) |> add_theme_model_plot()
    )
  } else if (length(types) == 2) {
    return(
      plot_two(data, types, predictors, response, layer) |> add_theme_model_plot()
    )
  } else if (length(types) == 3) {
    return(
      plot_three(data, types, predictors, response, layer) |> add_theme_model_plot()
    )
  } else if (length(types) == 4) {
    return(
      plot_four(data, types, predictors, response, layer) |> add_theme_model_plot()
    )
  } else {
    warning("Plotted only the first four effects")
    return(
      plot_four(data, types[1:4], predictors[1:4], response, layer) |> add_theme_model_plot()
    )
  }
}

