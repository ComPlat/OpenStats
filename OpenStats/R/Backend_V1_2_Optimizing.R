env_optim_V1_2 <- new.env(parent = getNamespace("OpenStats"))
# TODO: due to conversion to characters ggplot sorts the x axis labels not in increasing order

# Run optimization
# ====================================================================================
env_optim_V1_2$add_theme_optim <- function(p) {
  p + theme(
    plot.caption = element_text(hjust = 0),
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14)
  )
}

env_optim_V1_2$create_formula_optim <- function(formula, df, lower, upper, seed) {
  rhs <- as.character(formula)[3] |> str2lang()
  lhs <- as.character(formula)[2]
  vars <- all.vars(rhs)
  env_check_ast_V1_2$check_ast(formula, c(lhs, vars))
  stopifnot("Couldn't find the response variable" = lhs %in% names(df))
  params <- setdiff(vars, names(df))
  params <- as.character(params)
  stopifnot("No unknown parameters to optimize." = length(params) > 0)
  new(
    "OptimFormula", formula = formula, parameter = params, lhs = lhs, rhs = rhs,
    lower = lower, upper = upper, seed = seed
  )
}
env_optim_V1_2$make_loss_fn_optim <- function(formula, df) {
  params <- formula@parameter
  lhs <- formula@lhs
  rhs <- formula@rhs

  function(par, error_calc = TRUE) {
    eval_env <- new.env()
    list2env(df, envir = eval_env)
    list2env(as.list(setNames(par, params)), envir = eval_env)
    pred <- eval(rhs, envir = eval_env)
    if (!error_calc) {
      return(pred)
    }
    actual <- df[[lhs]]
    sum((actual - pred)^2)
  }
}

env_optim_V1_2$find_start_optim <- function(loss_fn, params_len, lower_boundary, upper_boundary) {
  space <- matrix(0, ncol = params_len, nrow = 400) # NOTE: adapt if required
  for (i in seq_len(nrow(space))) {
    space[i, ] <- runif(ncol(space), min = lower_boundary, max = upper_boundary)
  }
  space[which.min(apply(space, 1, loss_fn)), ]
}

env_optim_V1_2$determine_pred_variable_optim <- function(formula, df) {
  rhs_vars <- all.vars(formula@rhs)
  x_candidates <- intersect(rhs_vars, names(df))
  setdiff(x_candidates, formula@lhs)
}

env_optim_V1_2$predict_optim <- function(opti_params, loss_fn, df, x_vars, y_var) {
  pred <- loss_fn(opti_params$par, error_calc = FALSE)
  xdata <- do.call(paste, c(
    lapply(df[x_vars], function(v) {
      if (is.numeric(v)) round(v, 3) else v
    }),
    sep = "-"
  ))
  data.frame(
    i = rep(1:length(xdata), 2), x = rep(xdata, 2), y = c(df[[y_var]], pred),
    group = c(rep("Original", nrow(df)), rep("Predicted", nrow(df)))
  )
}

env_optim_V1_2$calc_r2_optim <- function(df, lhs) {
  pred <- df[df$group == "Predicted", "y"]
  actual <- df[df$group == "Original", "y"]
  r2 <- 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)
  r2_label <- sprintf("R^2 = %.3f", r2)
  r2_label
}

env_optim_V1_2$calc_formula_optim <- function(params, formula) {
  f <- formula@formula
  names <- formula@parameter
  params_list <- as.list(params)
  names(params_list) <- names
  f <- eval(substitute(substitute(FORM, params_list), list(FORM = f)))
  deparse(f)
}

env_optim_V1_2$check_seed_optim <- function(seed) {
  if (!is.numeric(seed)) stop("Seed has to be numeric")
  if (floor(seed) != seed) stop("Seed has to be an integer")
}

env_optim_V1_2$optimize <- function(formula, df) {
  lhs <- formula@lhs
  params <- formula@parameter
  env_optim_V1_2$check_seed_optim(formula@seed)
  set.seed(formula@seed)
  loss_fn <- env_optim_V1_2$make_loss_fn_optim(formula, df)
  start_params <- env_optim_V1_2$find_start_optim(loss_fn, length(params), formula@lower, formula@upper)
  opti_params <- optim(par = start_params, fn = loss_fn)
  if (is.null(opti_params$message)) opti_params$message <- ""
  x_vars <- env_optim_V1_2$determine_pred_variable_optim(formula, df)
  data <- env_optim_V1_2$predict_optim(opti_params, loss_fn, df, x_vars, lhs)
  new("optimResult",
    parameter = opti_params$par,
    error = opti_params$value,
    convergence = opti_params$convergence == 0,
    message = opti_params$message,
    predicted_df = data,
    x_vars = x_vars # Needed for latter plotting
  )
}

# Summary of "model"
# ====================================================================================
env_optim_V1_2$plot_model_optim <- function(formula_optim, result_optim) {
  y <- function() stop("Should never be called") # Please R CMD check
  i <- function() stop("Should never be called") # Please R CMD check

  df <- result_optim@predicted_df
  lhs <- formula_optim@lhs
  r2_label <- env_optim_V1_2$calc_r2_optim(df, lhs)
  formula_label <- env_optim_V1_2$calc_formula_optim(result_optim@parameter, formula_optim)
  formula_label <- gsub("~", "=", formula_label)
  x_vars <- result_optim@x_vars
  caption <- paste0(r2_label, ";  ", formula_label)
  p <- ggplot() +
    geom_point(data = df[df$group == "Original", ], aes(y = y, x = i)) +
    geom_line(data = df[df$group == "Predicted", ], aes(y = y, x = i, group = 1)) +
    labs(y = lhs, x = paste(x_vars, collapse = "-"), caption = caption)
  p <- env_optim_V1_2$add_theme_optim(p)
  new("plot", p = p, width = 10, height = 10, resolution = 600)
}

env_optim_V1_2$summary_model_optim <- function(formula_optim, result_optim) {
  parameter <- result_optim@parameter
  names <- formula_optim@parameter
  l <- setNames(parameter, names)
  as.data.frame(t(l))
}

env_optim_V1_2$information_criterion_optim <- function(result_optim) {
  df <- data.frame(
    result_optim@error
  )
  names(df) <- "Sum of Squared Errors (SSE)"
  df
}

# Assumptions for optim (Not used yet)
# ====================================================================================
env_optim_V1_2$plot_pred_optim <- function(result) {
  df <- result@predicted_df
  pred <- df[df$group == "Predicted", "y"]
  actual <- df[df$group == "Original", "y"]
  residuals <- actual - pred
  p <- ggplot(data.frame(pred = pred, resid = residuals), aes(x = pred, y = resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Predicted", y = "Residual", title = "Residuals vs Predicted")
  env_optim_V1_2$add_theme_optim(p)
}
env_optim_V1_2$hist_pred_optim <- function(result) {
  df <- result@predicted_df
  pred <- df[df$group == "Predicted", "y"]
  actual <- df[df$group == "Original", "y"]
  residuals <- actual - pred
  p <- ggplot(data.frame(resid = residuals), aes(x = resid)) +
    geom_histogram(bins = 30, color = "black", fill = "steelblue", alpha = 0.8) +
    labs(
      x = "Residuals",
      y = "Count",
      title = "Histogram of Residuals"
    )
  env_optim_V1_2$add_theme_optim(p)
}
env_optim_V1_2$plot_qq_optim <- function(result) {
  df <- result@predicted_df
  pred <- df[df$group == "Predicted", "y"]
  actual <- df[df$group == "Original", "y"]
  residuals <- actual - pred
  p <- ggplot(data.frame(resid = residuals), aes(sample = resid)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(
      x = "Theoretical Quantiles",
      y = "Sample Quantiles",
      title = "QQ Plot of Residuals"
    )
  env_optim_V1_2$add_theme_optim(p)
}
env_optim_V1_2$assumptions_optim <- function(result) {
  p <- cowplot::plot_grid(
    env_optim_V1_2$plot_pred_optim(result), env_optim_V1_2$hist_pred_optim(result), env_optim_V1_2$plot_qq_optim(result),
    ncol = 2
  )
  new("plot", p = p, width = 10, height = 10, resolution = 600)
}

