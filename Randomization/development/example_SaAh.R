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
n_blocks

result <- Randomization::random_finite_assign(
  seed = 1234,
  groups = groups[, 2:6],
  design = n_blocks,
  max_iter = 1000L,
  ids = groups$ids,
)
result

df <- data.frame(
  treatment = n_blocks$treatment,
  groups[result$unit_id, 2:6]
)
means <- tapply(df, df$treatment, function(b) {
  b <- c(b[, 2:6]) |> unlist()
  mean(b)
})
sds <- tapply(df, df$treatment, function(b) {
  b <- c(b[, 2:6]) |> unlist()
  sd(b)
})
means
sds
