# One Way ANOVA
summary(aov(uptake ~ Treatment, data = CO2))

# Factorial N Way ANOVA
summary(aov(uptake ~ Treatment * Type, data = CO2))

# linear regression
summary(aov(uptake ~ conc, data = CO2))

# ANCOVA (factor + covariate)
summary(aov(uptake ~ Treatment + conc, data = CO2))

# Repeated measurement ANOVA 
df <- data.frame(
  subject = rep(1:10, 3),
  Compounds = c(rep("A", 10), rep("B", 10), rep("C", 10)),
  time = rep(c(rep("Morning", 5), rep("Evening", 5)), 3),
  values = c(
    rnorm(5, 0, 1), rnorm(5, 10, 1),
    rnorm(5, 0.5, 1), rnorm(5, 20, 1),
    rnorm(5, 10, 1), rnorm(5, 30, 1)
  )
)
summary(aov(values ~ Compounds + Error(subject), data = df))
summary(aov(values ~ Compounds + Error(subject/time), data = df))
summary(aov(values ~ Compounds + Error(subject + subject:time), data = df))
