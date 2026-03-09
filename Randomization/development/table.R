library(Randomization)
set.seed(1234)
treatments <- c("control", "Compound1", "Compound2")
locations <- c("M", "C", "A")
n <- 16L
n_per_location <- n * length(treatments)
df <- Randomization:::completely_randomised_design(
  predictors = list(treatments = treatments, locations = locations), n_per_level = n
)
dfs <- split(df, df$locations)
df <- dfs[[3L]]
knitr::kable(head(df))
table(df)
table(df$treatments, df$locations)
