library(ggplot2)
library(Randomization)
groups <- read.csv("./Randomization/development/tesstSaAH.csv", sep = ";", header = TRUE)
names(groups)[[1L]] <- "ids"
groups[, 2:6] <- apply(groups[, 2:6], 2, as.numeric)

# Divide it on 3 groups
# 11, 11, 10
n_blocks <- data.frame(
  treatment = c(
    rep("mCherry", 11L),
    rep("Gq", 11L),
    rep("Gi", 10L)
  )
)

result_mahalanobis <- random_finite_assign(
  seed = 42L,
  groups = groups[, 2:6],
  design = n_blocks,
  max_iter = 1000L,
  loss_function = "Mahalanobis",
  ids = groups$ids
)
groups_optim_mahalanobis <- groups[result_mahalanobis$unit_id, 2L:6L]
df_optim_mahalanobis <- data.frame(
  stack(groups_optim_mahalanobis),
  treatment = rep(n_blocks$treatment, 5L),
  group = "Mahalanobis"
)

result_default <- random_finite_assign(
  seed = 42L,
  groups = groups[, 2:6],
  design = n_blocks,
  max_iter = 1000L,
  ids = groups$ids
)
groups_optim_default <- groups[result_default$unit_id, 2L:6L]
df_optim_default <- data.frame(
  stack(groups_optim_default),
  treatment = rep(n_blocks$treatment, 5L),
  group = "Default"
)

df_orig <- data.frame(
  stack(groups[, 2L:6L]),
  treatment = rep(n_blocks$treatment, 5L),
  group = "origin"
)

df <- Reduce(rbind, list(df_optim_mahalanobis, df_optim_default))

ggplot(data = df, aes(x = treatment, y = values)) +
  stat_summary(
    data = df, aes(group = group, linetype = group, colour = group),
    fun = mean, geom = "line",
    position = position_dodge(width = 0.8), color = "black"
  ) +
  stat_summary(
    aes(group = group, colour = group),
    fun.data = mean_sdl,
    fun.args = list(mult = 1), # 1 SD
    geom = "errorbar",
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  facet_wrap(~ ind, scales = "free")


summary <- function(df, fct) {
  res <- list()
  for (g in unique(df$group)) {
    sub1 <- df[df$group == g, ]
    temp1 <- c()
    for (i in unique(sub1$ind)) {
      sub2 <- sub1[sub1$ind == i, ]
      temp2 <- c()
      for (t in unique(sub2$treatment)) {
        sub3 <- sub2[sub2$treatment == t, ]
        temp2[[t]] <- fct(sub3$values)
      }
      temp1[[i]] <- temp2 |> unlist()
    }
    res[[g]] <- Reduce(rbind, temp1)
  }
  res
}
summary(df, mean)
summary(df, sd)
