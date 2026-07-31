# -------------------------------------------------------------------------------
# Core idea:
# - already fitted lme4 model
# - 1. simulate new response data using that model
# - 2. refit model
# - 3. test the effect of interest
# - 4. report the proportion of simulations where the effect was significant (aka the power)
# -------------------------------------------------------------------------------

library(lme4)
library(simr)

# 1. Start from a fitted model (real or hypothetical)
m <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)

# 2. Power for the fixed effect "Days", at the sample size you already have
power_days <- powerSim(m, nsim = 180)

# 3. increase/decrease number of subjects
m_ext <- extend(m, along = "Subject", n = 20)
power_curve <- powerCurve(m_ext, along = "Subject", breaks = c(6, 10, 14, 18, 20))
plot(power_curve)


# 4. GLMM example: binary outcome
m_glmm <- glmer(
  cbind(incidence, size - incidence) ~ period + (1 | herd),
  family = binomial,
  data = cbpp
)
powerSim(m_glmm, nsim = 200)
