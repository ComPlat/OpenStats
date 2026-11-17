library(nls2)

fit_nls_model <- function(formula, data, start_values) {
  nls_model <- nls(formula, data = data, start = start_values)
  model_summary <- summary(nls_model)
  coefficients <- coef(nls_model)
  standard_errors <- summary(nls_model)$coefficients[, 2]
  residuals <- residuals(nls_model)
  r_squared <- 1 - sum(residuals^2) / sum((data[[as.character(formula[[2]])]] - mean(data[[as.character(formula[[2]])]]))^2)
  aic <- AIC(nls_model)
  results <- list(
    coefficients = coefficients,
    standard_errors = standard_errors,
    residuals = residuals,
    r_squared = r_squared,
    aic = aic,
    model_summary = model_summary
  )
  return(results)
}

data(airquality)
nls_formula <- Ozone ~ a * exp(b * Temp)
start_vals <- list(a = 20, b = 0.1)
nls_results <- fit_nls_model(nls_formula, data = airquality, start_values = start_vals)
print(nls_results$model_summary)
cat("Coefficients:\n")
print(nls_results$coefficients)
cat("Standard Errors:\n")
print(nls_results$standard_errors)
cat("R-squared:", nls_results$r_squared, "\n")
cat("AIC:", nls_results$aic, "\n")

