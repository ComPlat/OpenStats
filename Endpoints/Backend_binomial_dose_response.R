env_dose_response_binomial_V1_2 <- new.env(parent = getNamespace("OpenStats"))

create_model_binomial <- function(df, formula) {
  f <- as.character(formula)
  dep <- f[[2L]]
  indep <- f[[3L]]
  stopifnot(
    dep %in% names(df), indep %in% names(df),
    is.numeric(df[[dep]]), is.numeric(df[[indep]])
  )
  if (any(!df[[dep]] %in% c(0, 1))) {
    stop("The dependent variable must contain only 0/1 values.")
  }
  if (any(df[[indep]] <= 0, na.rm = TRUE)) {
    stop("All dose values must be > 0 because log10(dose) is used.")
  }
  # Model:
  # logit(p) = β0 + β1 * log10(dose)
  # where logit(p) = log(p / (1 - p))
  formula_glm <- stats::as.formula(
    paste(dep, "~ log10(", indep, ")", sep = "")
  )
  glm(formula_glm, family = binomial, data = df)
}
env_dose_response_binomial_V1_2$create_model_binomial <- create_model_binomial

uncertainty_dose_response_binomial <- function(model, b0, b1, conf_level, logit_target, log10_dose) {
  # ------------------------------------------------------------------
  # THEORY: Delta Method
  #
  # Let β̂ = (β̂0, β̂1) be the MLE from the GLM.
  # Under standard regularity conditions:
  #
  #   β̂ ~ N(β, Σ)
  #
  # where Σ = vcov(model).
  # vcov = variance-convariance matrix
  # Var(ß_hat) = Sigma = [
  #   Var(ß_hat_0)                Cov(ß_hat_0, ß_hat_1)
  #   Cov(ß_hat_1, ß_hat_0)       Var(ß_hat_1)
  # ]
  #
  # We are interested in:
  #   g(β0, β1) = (logit(p_target) - β0) / β1
  #
  # which gives log10(dose).
  #
  # The delta method gives:
  #
  #   Var(g(β̂)) ≈ ∇g(β)^T Σ ∇g(β)
  #
  # ------------------------------------------------------------------
  V <- vcov(model)

  # ------------------------------------------------------------------
  # Gradient of g(β0, β1)
  #
  # g(β0, β1) = (logit_target - β0) / β1
  #
  # ∂g/∂β0 = -1 / β1
  # ∂g/∂β1 = -(logit_target - β0) / β1^2
  # ------------------------------------------------------------------
  grad <- c(
    -1 / b1,
    -(logit_target - b0) / (b1^2)
  )

  # Variance propagation
  # Var(g(ß_hat)) = g_gradient^T %*% Sigma %*% g_gradient
  var_log10_dose <- as.numeric(t(grad) %*% V %*% grad)
  if (!is.finite(var_log10_dose) || var_log10_dose < 0) {
    stop("Failed to compute a valid variance for the estimated dose.")
  }
  se_log10_dose <- sqrt(var_log10_dose)

  # ------------------------------------------------------------------
  # Asymptotic normality:
  #
  # log10(dosê) ≈ Normal(mean, SE^2)
  #
  # → Wald-type confidence interval on log10 scale
  # ------------------------------------------------------------------
  alpha <- 1 - conf_level
  z <- qnorm(1 - alpha / 2)
  lower_log10 <- log10_dose - z * se_log10_dose
  upper_log10 <- log10_dose + z * se_log10_dose

  # Back-transform:
  # This yields a multiplicative (log-normal type) CI on the dose scale
  lower_dose <- 10^lower_log10
  upper_dose <- 10^upper_log10
  c(lower_dose, upper_dose)
}
env_dose_response_binomial_V1_2$uncertainty_dose_response_binomial <- uncertainty_dose_response_binomial

dose_response_binomial <- function(df, formula, target = 0.5, conf_level = 0.95) {
  stopifnot(
    is.data.frame(df),
    inherits(formula, "formula"),
    is.numeric(target), length(target) == 1L, target > 0, target < 1,
    is.numeric(conf_level), length(conf_level) == 1L,
    conf_level > 0, conf_level < 1
  )
  model <- env_dose_response_binomial_V1_2$create_model_binomial(df, formula)

  coefs <- coef(model)
  b0 <- unname(coefs[[1L]])
  b1 <- unname(coefs[[2L]])
  if (!is.finite(b1) || b1 == 0) {
    stop("The fitted slope is zero or non-finite; target dose cannot be computed.")
  }
  logit_target <- qlogis(target)
  log10_dose <- (logit_target - b0) / b1
  dose <- 10^log10_dose

  interval <- env_dose_response_binomial_V1_2$uncertainty_dose_response_binomial(
    model, b0, b1, conf_level, logit_target, log10_dose
  )

  m <- broom::tidy(model)
  estimate_table <- data.frame(
    Estimate = dose,
    Lower = interval[[1L]],
    Upper = interval[[2L]],
    ß_0 = m$estimate[[1L]],
    ß_1 = m$estimate[[2L]],
    ß_0_std_error = m$std.error[[1L]],
    ß_1_std_error = m$std.error[[2L]],
    ß_0_t_statistic = m$statistic[[1L]],
    ß_1_t_statistic = m$statistic[[2L]],
    ß_0_p_val = m$p.value[[1L]],
    ß_1_p_val = m$p.value[[2L]],
    row.names = NULL
  )
  names(estimate_table)[[1L]] <- paste0("IC_", target)

  estimate_table
}
env_dose_response_binomial_V1_2$dose_response_binomial <- dose_response_binomial

set.seed(1234)
df <- data.frame(
  conc = c(
    rep(0.5, 5L),
    rep(2.5, 5L),
    rep(5, 5L),
    rep(10, 5L),
    rep(15, 5L),
    rep(20, 5L),
    rep(25, 5L)
  ),
  values = c(
    rbinom(5, 1, 0.85),
    rbinom(5, 1, 0.75),
    rbinom(5, 1, 0.65),
    rbinom(5, 1, 0.55),
    rbinom(5, 1, 0.35),
    rbinom(5, 1, 0.25),
    rbinom(5, 1, 0.15)
  )
)

formula <- values ~ conc
env_dose_response_binomial_V1_2$dose_response_binomial(df, formula)
