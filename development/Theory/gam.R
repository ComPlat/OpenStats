# NOTE: Documentation for gam
# Arguments:
# 1. formula: Include smooth terms, e.g., s(x) or s(x, y)
# 2. family: Distribution family with default link functions

# Common families and their default link functions for gam (mgcv package):
# - binomial: link = logit
# - gaussian: link = identity
# - Gamma: link = inverse
# - poisson: link = log
# - quasibinomial: link = logit
# - quasipoisson: link = log

# Link functions for each family in mgcv
link_functions_gam <- list(
  binomial = c("logit" = "link = 'logit'", "probit" = "link = 'probit'", "cloglog" = "link = 'cloglog'"),
  gaussian = c("identity" = "link = 'identity'", "log" = "link = 'log'", "inverse" = "link = 'inverse'"),
  gamma = c("inverse" = "link = 'inverse'", "log" = "link = 'log'"),
  poisson = c("log" = "link = 'log'", "identity" = "link = 'identity'"),
  quasibinomial = c("logit" = "link = 'logit'", "probit" = "link = 'probit'"),
  quasipoisson = c("log" = "link = 'log'")
)

# Function to fit a GAM model
gam_internally <- function(formula, data, family, link) {
  if (!(family %in% names(link_functions_gam))) {
    stop("Invalid family provided.")
  }
  if (!(link %in% names(link_functions_gam[[family]]))) {
    stop("Invalid link function for the specified family.")
  }
  call <- paste0("mgcv::gam(", formula, ", data = ", deparse(substitute(data)), 
                 ", family = ", family, "(link = '", link, "'))")
  model <- eval(parse(text = call))
  return(model)
}

# Example usage
library(mgcv)
f <- "Ozone ~ s(Temp)"
family <- "poisson"
link <- "log"
model <- gam_internally(f,airquality, family, link)

# Posthoc and ANOVA analysis
library(emmeans)
dependent_var <- "Ozone"
independent_var <- "s(Temp) + Month"
formula <- as.formula(paste(dependent_var, "~", independent_var))

# Run GAM model and anova
model <- gam(formula, data =airquality, family = gaussian())
anova_results <- anova(model)
anova_results

# Posthoc analysis with emmeans
posthoc_results <- emmeans(model, specs = ~ Month) # Does not work for s(terms)
summary(posthoc_results)
pairwise_results <- pairs(posthoc_results)
pairwise_results <- as.data.frame(pairwise_results)
pairwise_results <- pairwise_results[pairwise_results$p.value <= 0.05, ]

temp_vals <- data.frame(Temp = seq(min(airquality$Temp), max(airquality$Temp), length.out = 100),
  Month = seq(min(airquality$Month), max(airquality$Month), length.out = 100))
predictions <- predict(model, newdata = temp_vals, se.fit = TRUE)
pred_df <- cbind(temp_vals, fit = predictions$fit, se = predictions$se.fit)
library(ggplot2)
ggplot(pred_df, aes(x = Temp, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se), alpha = 0.2) +
  labs(title = "Predicted Ozone Levels by Temperature", y = "Predicted Ozone")

