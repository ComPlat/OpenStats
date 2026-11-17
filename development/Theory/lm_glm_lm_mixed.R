library(gamm4)
library(mgcv)
library(multcomp)
library(emmeans)
library(lme4)
library(lmerTest)
df <- CO2

# ANOVA
# Linear Model
lm <- lm(uptake ~ Treatment, data = df)
summary(aov(lm))
# Generalized Linear Model
glm <- glm(uptake ~ Treatment, data = df)
anova(lm)
anova(glm)
# Mixed Linear Model
lmm <- lmer(uptake ~ Treatment + (1 | Type), data = df)
anova(lmm)
# GAM Model
gam_model <- gam(uptake ~ s(conc, k = 5), data = df)
summary(gam_model)
plot(gam_model, residuals = TRUE, pch = 16, rug = TRUE)
# Fit a GAMM with a smooth term for conc and a random effect for Type
gamm_model <- gamm4(uptake ~ s(conc, k = 5), random = ~(1 | Type), data = df)
summary(gamm_model$gam)
summary(gamm_model$mer)
# Fit a GAM with a smooth term for conc and a categorical predictor
gam_model <- gam(uptake ~ s(conc, k = 5) + Treatment, data = df)

# POSTHOC TESTS
emmeans_result <- emmeans(glm, pairwise ~ Treatment)
summary(emmeans_result)
glht_result <- glht(glm, linfct = mcp(Treatment = "Tukey"))
summary(glht_result)
glht_result <- glht(lmm, linfct = mcp(Treatment = "Tukey"))
summary(glht_result)


# Alternative for Continuous Predictor (e.g., conc)
lm_cont <- lm(uptake ~ conc, data = df)
summary(lm_cont)
glm_cont <- glm(uptake ~ conc, data = df)
summary(glm_cont)
lmm_cont <- lmer(uptake ~ conc + (1 | Type), data = df)
summary(lmm_cont)
# Post hoc comparisons for Treatment levels with gam
emmeans_result <- emmeans(gam_model, pairwise ~ Treatment)
summary(emmeans_result)

