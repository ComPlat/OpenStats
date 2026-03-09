library(Randomization)
set.seed(1234)
treatments <- c("control", "ibo")
locations <- c("M", "C", "A")
n <- 16L
n_per_location <- n * 2L
df <- Randomization:::completely_randomised_design(
  predictors = list(treatments = treatments, locations = locations), n_per_level = n
)
dfs <- split(df, df$locations)
df <- dfs[[3L]]
knitr::kable(head(df))


set.seed(1234)
groups <- data.frame(
  weights = rnorm(n_per_location, 300, 40),
  blood_value = rnorm(n_per_location, 10, 2.5)
)
result <- Randomization::random_finite_assign(seed = 1234, groups = groups, design = df)
df$weights <- result$assigned$weights
df$blood_value <- result$assigned$blood_value
df$animal_ids <- result$assigned$unit_index
knitr::kable(head(df))


library(ggplot2)
library(cowplot)
l <- length(groups$weights)
d <- data.frame(
  treatments = rep(df$treatments, 2L),
  weights = c(df$weights, groups$weights),
  groups = c(rep("Optimized", l), rep("Original", l))
)
p1 <- ggplot(data = d,
  aes(x = treatments, y = weights, fill = groups, groups = groups)) +
  geom_boxplot() +
  stat_summary(
    fun = mean, geom = "point", position = position_dodge(width = 0.8),
    size = 2, color = "black"
  ) +
  stat_summary(
    fun = mean,
    geom = "line", aes(group = groups),
    position = position_dodge(width = 0.8), color = "black"
  )
p1
d <- data.frame(
  treatments = rep(df$treatments, 2L),
  blood_value = c(df$blood_value, groups$blood_value),
  groups = c(rep("Optimized", l), rep("Original", l))
)
p2 <- ggplot(data = d, aes(x = treatments, y = blood_value, fill = groups, groups = groups)) +
  geom_boxplot() +
  stat_summary(
    fun = mean, geom = "point", position = position_dodge(width = 0.8),
    size = 2, color = "black"
  ) +
  stat_summary(
    fun = mean, geom = "line", aes(group = groups),
    position = position_dodge(width = 0.8), color = "black"
  )
plot_grid(p1, p2)
