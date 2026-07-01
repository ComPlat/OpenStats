library(Randomization)
groups <- read.csv("./Randomization/development/Salman/2403.csv", sep = ";", header = TRUE)
# Two groups: mCherry and Gi-DREADD
treatment_groups <- c("mCherry", "Gi")

# Divide it on 2 groups
# 11, 11
n_blocks <- data.frame(treatment = rep(treatment_groups, each = 11L))

run <- function(groups, treatment_groups, n_blocks, is_post) {
  indices <- 2L:6L
  if (is_post) indices <- 7L:11L

  result_default <- random_finite_assign(
    seed = 42L,
    groups = groups[, indices],
    design = n_blocks,
    max_iter = 1000L,
    ids = groups$Animal.ID
  )
  df <- groups[result_default$unit_id, indices]
  df$treatment <- n_blocks

  summary <- function(df, treatment_groups, fct) {
    res <- lapply(treatment_groups, function(t) {
      g <- df[df$treatment == t, ]
      vapply(seq_len(5L), function(i) {
        values <- g[, i]
        fct(values)
      }, numeric(1L))
    })
    res <- setNames(res, treatment_groups)
    data.frame(res)
  }
  res <- list()
  res[["mean"]] <- summary(df, treatment_groups, mean)
  res[["sd"]] <- summary(df, treatment_groups, sd)
  res
}

res <- list()
res[["pre"]] <- run(groups, treatment_groups, n_blocks, FALSE)
res[["post"]] <- run(groups, treatment_groups, n_blocks, TRUE)

write.csv(res, file = "./Randomization/development/Salman/2403_result.csv", quote = FALSE)
