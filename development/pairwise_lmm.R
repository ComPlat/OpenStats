
library(lmerTest)
library(emmeans)
fixed_formula <- reformulas::nobars(formula)

head(mtcars)

df <- mtcars
df$cyl <- factor(df$cyl)
df$gear <- factor(df$gear)
model <- lmerTest::lmer(
  mpg ~ wt * cyl + (1 | gear),
  data = df
)
trends <- emtrends(model, ~ cyl, var = "wt")
pairs(trends, adjust = "tukey")
pairs(emmeans(model, ~ cyl), adjust = "tukey")


df <- sleepstudy
df$Group <- ifelse(as.integer(df$Subject) %% 2 == 0, "A", "B")
df$Group <- factor(df$Group)
model <- lmerTest::lmer(
  Reaction ~ Days * Group + (Days | Subject),
  data = df
)
anova(model)
trends <- emtrends(model, ~ Group, var = "Days")
trends
pairs(trends)
