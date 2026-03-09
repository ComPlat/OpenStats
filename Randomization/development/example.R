# random_assign <- function(df, groups, ratios, col, block_col, strata_cols, randomization_method, n_quantiles = 10L, seed)

# -------------------------------------------------------------
# ------- Create design matrix --------------------------------
# -------------------------------------------------------------
set.seed(1234)
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

# -------------------------------------------------------------
# ------ Assign animals ---------------------------------------
# -------------------------------------------------------------
groups <- data.frame(
  weights = rnorm(n_per_location, 300, 25),
  blood_value = rnorm(n_per_location, 10, 1)
)
result <- Randomization::random_finite_assign(1234, groups, df)
df$weights <- result$assigned$weights
df$blood_value <- result$assigned$blood_value
df$animal_ids <- result$assigned$unit_index
head(df)
boxplot(df$weights ~ df$treatments)
boxplot(df$blood_value ~ df$treatments)

# -------------------------------------------------------------
# ------ Assign experimental blocks ---------------------------
# -------------------------------------------------------------
experimental_blocks <- as.character(1L:8L)
ratios <- rep(1, 8)
df <- Randomization::random_assign(
  df = df, groups = experimental_blocks, ratios = ratios,
  col = "block", strata_cols = c("treatments", "locations"),
  randomization_method = "block_stratified", seed = 1234
)
df
boxplot(df$weights ~ df$block)
boxplot(df$blood_value ~ df$block)
