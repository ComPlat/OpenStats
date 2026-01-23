treatments <- c("control", "ibo")
locations <- c("M", "C", "A")
n <- 16L # per location treatment combination
blocks_per_location <- 8L # Consisting of 2 animals from control and 2 from ibo respectively
n_per_block <- 4L
n_per_location <- 32L # assign the 32 animals based on their weight to one of the two treatment blocks

# Create the design matrix
df <- Randomization:::completely_randomised_design(
  predictors = list(treatments = treatments, locations = locations),
  n_per_level = n
)
# Split the df based on locations
dfs <- split(df, df$locations)
# Lets proceed with Mannheim
df <- dfs[[3L]]
# Assign the animals based on their weights
weights <- rnorm(n_per_location, 300, 25)
randomization_method <- "block_stratified"
strata_cols <- "treatments"
df <- Randomization::random_assign(
  df = df, groups = weights, ratios = NULL, group_type = "finite", col = "weights", strata_cols = strata_cols,
  randomization_method = randomization_method, n_quantiles = 3L, seed = 1234
)
df$weights <- as.numeric(df$weights)
# Control
with(df, tapply(weights, treatments, mean))
with(df, tapply(weights, treatments, sd))
with(df, table(treatments, cut(weights,
  quantile(weights, probs = 0:3/3),
  include.lowest = TRUE
)))

# Assign the experimental blocks
experimental_blocks <- as.character(1L:8L)
ratios <- rep(1, 8)
df <- Randomization::random_assign(
  df = df, groups = experimental_blocks, ratios = ratios, group_type = "infinite", col = "block", strata_cols = strata_cols,
  randomization_method = randomization_method, seed = 1234
)
df
split(df, df$block)
