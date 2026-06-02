files <- list.files("./OpenStats/R", full.names = TRUE)
trash <- lapply(files, source)

kon1 <- 8e5
koff1 <- 2e-3
Bmax1 <- 120
kon2 <- 5e4
koff2 <- 3e-4
Bmax2 <- 60

lower <- 0.0
upper <- 10e6
formula <- y ~
  (conc_nM * 1e-9 / (conc_nM * 1e-9 + koff1 / kon1)) *
    Bmax1 *
    (1 - exp(-(kon1 * conc_nM * 1e-9 + koff1) * time_s)) +
    (conc_nM * 1e-9 / (conc_nM * 1e-9 + koff2 / kon2)) *
      Bmax2 *
      (1 - exp(-(kon2 * conc_nM * 1e-9 + koff2) * time_s))

df <- read.csv("./test_data/two_hot_binding_sim.csv")
formula <- env_optim_V1_2$create_formula_optim(formula, df, "nonlinear least squares", lower, upper, 1234)

optimize <- function(formula, df) {
  lhs <- formula@lhs
  params <- formula@parameter
  env_optim_V1_2$check_seed_optim(formula@seed)
  set.seed(formula@seed)
  params_len <- length(params)
  start <- setNames(as.list(runif(params_len, min = formula@lower, max = formula@upper)), params)
  start <- list(
    koff1 = 2e-3, kon1 = 8e5, Bmax1 = 120, koff2 = 3e-4, kon2 = 5e5, Bmax2 = 60
  )
  model <- nls(
    formula@formula,
    df,
    start = start,
    lower = setNames(rep(formula@lower, params_len), params),
    upper = setNames(rep(formula@upper , params_len), params),
    algorithm = "port"
  )
  par <- broom::tidy(model) |> as.data.frame()
  par <- par$estimate
  pred <- predict(model)
  error <- sum((df[[formula@lhs]] - pred)^2)
  x_vars <- env_optim_V1_2$determine_pred_variable_optim(formula, df)

  predict_optim_nls <- function(model, df, x_vars, y_var) {
    pred <- predict(model)
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
  data <- predict_optim_nls(model, df, x_vars, formula@lhs)
  new("optimResult",
    parameter = par,
    error = error,
    convergence = model$convergence == 0,
    message = model$message,
    predicted_df = data,
    x_vars = x_vars # Needed for latter plotting
  )
}
res <- optimize(formula, df)
str(res)
