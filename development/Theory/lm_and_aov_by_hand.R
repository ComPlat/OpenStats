predictor1 <- CO2[["conc"]]
temp <- CO2[["Treatment"]]
predictor2 <- numeric(length(temp))
predictor2[temp == "chilled"] <- 1
response <- CO2[["uptake"]]
n <- length(response)
df_residual <- n - 4 # 4 parameters
res <- list()
ll <- function(ßs) {
  sum(
    dnorm(
      response,
      mean = ßs[1] + ßs[2]*predictor1 + ßs[3]*predictor2 + ßs[4]*(predictor1 * predictor2),
      log = TRUE
    )
  )
}
op <- optim(par = c(1, 0.1, 0.1, 0.01), fn = ll, method = "BFGS", control = list(fnscale = -1, reltol = 1e-12), hessian = TRUE)
Estimate <- op$par
predicted <- Estimate[1] + Estimate[2] * predictor1 + Estimate[3]*predictor2 + Estimate[4]*(predictor1 * predictor2)
residuals <- response - predicted
res[["Residuals"]] <- summary(residuals)

# Hessian from optim assumes σ² = 1 (standard normal errors),
# but in reality the residual variance is estimated from the data.
var_covar_mat <- solve(-op$hessian)
sigma2_hat <- sum(residuals^2) / df_residual # = residual_standard_error^2
var_covar_mat_scaled <- sigma2_hat * var_covar_mat
std_error <- sqrt(diag(var_covar_mat_scaled))

# test whether each β is significantly different from 0. Each estimate is investigated independently.
# If ß is 0 the predictor has no effect as the Estimate has no effect
# ßhat ~ N(ß, std_error^2)
# t = (ßhat - ß) / std_error with the null hypothesis H0: ß = 0 => t = ßhat / std_error
t_values <- Estimate / std_error
# Is the effect of the predictor statistically significant
p_values <- 2 * pt(-abs(t_values), df = df_residual)

Coefficients <- do.call(cbind, list(Estimate, std_error, t_values, p_values)) |> as.data.frame()
names(Coefficients) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
row.names(Coefficients) <- c("Intercept", "conc", "Treatment", "conc:Treatment")
res[["Coefficients"]] <- Coefficients

# How much do my predictions typically deviate from the observed values
residual_standard_error <- sqrt(sum(residuals^2) / df_residual)
res[["Residual standard error"]] <- c(residual_standard_error, df_residual)

sum_sq_total <- sum((response - mean(response))^2) # total variance relative to the mean
sum_sq_res <- sum(residuals^2) # measure for the distance between observed and predicted response values
r_squared <- 1 - sum_sq_res / sum_sq_total
adj_r_squared <- 1 - (1 - r_squared) * ((n - 1) / df_residual)
res[["R^2"]] <- c(r_squared, adj_r_squared)

# Does the full model explain significantly more variance than a model with no predictors (only the intercept)
df_model <- 3  # only conc, Treatment and interaction
ms_model <- (sum_sq_total - sum_sq_res) / df_model
ms_res <- sum_sq_res / df_residual
f_statistic <- ms_model / ms_res
f_p_value <- pf(f_statistic, df_model, df_residual, lower.tail = FALSE)

res[["F-statistic"]] <- c(f_statistic, df_model, df_residual, f_p_value)
res

# NOTE: if only one predictor is investigated lm and aov result in the same F/p value
summary(lm(uptake ~ conc*Treatment, data = CO2))

model <- lm(uptake ~ conc*Treatment, data = CO2)
summary(model)
summary(aov(model))

# ANOVA (Type I) for the entire model
# total variance relative to the mean
sum_sq_total <- sum((response - mean(response))^2)
df_total <- n - 1
# Residual variation => unexplained variation — distance between observed and predicted values
sum_sq_res <- sum(residuals^2)
p <- length(Estimate)
df_residual <- n - p  # p = number of parameters/estimates
# Model sum of sq => variation explained by the model
sum_sq_model <- sum_sq_total - sum_sq_res
df_model <- p - 1  # number of predictors
# Mean squares => normalisation
ms_model <- sum_sq_model / df_model
ms_res <- sum_sq_res / df_residual
# F stats:  ratio of explained to unexplained variance per degree of freedom
f_value <- ms_model / ms_res
p_value <- pf(f_value, df_model, df_residual, lower.tail = FALSE)
f_value
p_value
