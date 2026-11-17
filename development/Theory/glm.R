
# NOTE: Documentation for glm
# Arguments:
# 1. formula
# 2. family
# Common families and their default link functions:
# - binomial: link = logit
# - gaussian: link = identity
# - Gamma: link = inverse
# - inverse.gaussian: link = 1/mu^2
# - poisson: link = log
# - quasi: link = identity, variance = constant
# - quasibinomial: link = logit
# - quasipoisson: link = log

# Link functions for each family
link_functions <- list(
  binomial = c("logit" = "link = 'logit'", "probit" = "link = 'probit'", 
               "cloglog" = "link = 'cloglog'", "cauchit" = "link = 'cauchit'", 
               "log" = "link = 'log'"),
  
  gaussian = c("identity" = "link = 'identity'", "log" = "link = 'log'", 
               "inverse" = "link = 'inverse'", "sqrt" = "link = 'sqrt'"),
  
  gamma = c("inverse" = "link = 'inverse'", "log" = "link = 'log'", 
             "identity" = "link = 'identity'", "1/mu^2" = "link = '1/mu^2'"),
  
  inverse_gaussian = c("1/mu^2" = "link = '1/mu^2'", "log" = "link = 'log'"),
  
  poisson = c("log" = "link = 'log'", "identity" = "link = 'identity'", 
              "sqrt" = "link = 'sqrt'"),
  
  quasi = c("identity" = "link = 'identity'", "log" = "link = 'log'", 
            "inverse" = "link = 'inverse'"),
  
  quasibinomial = c("logit" = "link = 'logit'", "probit" = "link = 'probit'", 
                    "cloglog" = "link = 'cloglog'", "cauchit" = "link = 'cauchit'"),
  
  quasipoisson = c("log" = "link = 'log'", "identity" = "link = 'identity'")
)

glm_internally <- function(formula, data, family, link) {
  if (!(family %in% names(link_functions))) {
    stop("Invalid family provided.")
  }
  if (!(link %in% names(link_functions[[family]]))) {
    stop("Invalid link function for the specified family.")
  }
  call <- paste0("glm(", formula, ", data = ", deparse(substitute(data)), 
                 ", family = ", family, "(link = '", link, "'))")
  model <- eval(parse(text = call))
  return(model)
}

f <- "uptake ~ conc"
family <- "poisson"
link <- "sqrt"
model <- glm_internally(f, CO2, family, link)

library(emmeans)
CO2$conc <- as.factor(CO2$conc)
dependent_var <- "uptake"
independent_var <- "conc * Type"
formula <- as.formula(paste(dependent_var, "~", independent_var))
model <- glm(formula, data = CO2, family = gaussian())
anova_results <- anova(model, test = "Chisq")
posthoc_results <- emmeans(model, specs = ~ conc * Type)
summary(posthoc_results)
pairwise_results <- pairs(posthoc_results)
pairwise_results <- as.data.frame(pairwise_results)
pairwise_results <- pairwise_results[pairwise_results$p.value <= 0.05, ]
pairwise_results
