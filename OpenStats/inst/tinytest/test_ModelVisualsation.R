library(OpenStats)
library(tinytest)

# Dummy data
df <- data.frame(
  y = rnorm(10),
  x1 = rnorm(10),
  x2 = factor(sample(letters[1:3], 10, replace = TRUE))
)

# ---- Test env_utils_V1_2$split_formula ----
f <- y ~ x1 + x2
res <- OpenStats:::env_utils_V1_2$split_formula(f)

expect_identical(res$response, quote(y))
expect_identical(res$right_site, quote(x1 + x2))

# ---- Test env_utils_V1_2$vars_rhs ----
rhs_vars <- OpenStats:::env_utils_V1_2$vars_rhs(res$right_site)
expect_identical(rhs_vars, c("x1", "x2"))

# ---- Test env_summarising_model_V1_2$determine_types ----
types <- OpenStats:::env_summarising_model_V1_2$determine_types(rhs_vars, df)
expect_equal(types, c(x1 = "numeric", x2 = "factor"))

# ---- Test env_summarising_model_V1_2$create_new_numeric ----
num_seq <- OpenStats:::env_summarising_model_V1_2$create_new_numeric("x1", df, n = 5)
expect_equal(length(num_seq), 5)
expect_true(all(num_seq >= min(df$x1)) && all(num_seq <= max(df$x1)))

num_quant <- OpenStats:::env_summarising_model_V1_2$create_new_numeric("x1", df, slicing = TRUE)
expect_equal(names(num_quant), c("10%", "50%", "90%"))

# ---- Test env_summarising_model_V1_2$create_new_non_numeric ----
non_num_levels <- OpenStats:::env_summarising_model_V1_2$create_new_non_numeric("x2", df)
expect_true(all(non_num_levels %in% levels(df$x2)))

# ---- Test env_summarising_model_V1_2$create_new_data ----
new_data <- OpenStats:::env_summarising_model_V1_2$create_new_data(f, df, c("x1", "x2"), n = 5)
expect_true(all(c("x1", "x2") %in% names(new_data)))

# ---- Test env_summarising_model_V1_2$get_predictions ----
model <- lm(y ~ x1 + x2, data = df)
pred_df <- OpenStats:::env_summarising_model_V1_2$get_predictions(model, new_data)
expect_true(all(c("predicted", "conf.low", "conf.high") %in% names(pred_df)))
expect_equal(nrow(pred_df), nrow(new_data))

