
df <- data.frame(
  substances = c(
    rep("S1", 5L),
    rep("S2", 5L),
    rep("S3", 5L),
    rep("neg", 5L),
    rep("pos", 5L)
  ),
  values = c(
    rnorm(5, 1, 0.1),
    rnorm(5, 0.25, 0.05),
    rnorm(5, 0.85, 0.1),
    rnorm(5, 1, 0.05),
    rnorm(5, 0.01, 0.0005)
  )
)
df
write.csv(df, "./test_data/primary_data.csv", quote = FALSE, row.names = FALSE)
