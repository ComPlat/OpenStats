env_primary_assay_V1_2 <- new.env(parent = getNamespace("OpenStats"))

pos_norm <- function(df, dep, indep, pos_control_name) {
  if (pos_control_name != "") {
    mean_pos <- mean(df[df[, indep] == pos_control_name, dep], na.rm = TRUE)
    df[, dep] <- df[, dep] - mean_pos
    df <- df[df[, indep] != pos_control_name, , drop = FALSE] # Remove pos control
  } else {
    warning("No positive control is specified the respective normalization will not be conducted")
  }
  df
}
env_primary_assay_V1_2$pos_norm <- pos_norm

check_mapping <- function(df, indep, name_column) {
  tab <- tapply(df[[indep]], df[[name_column]], function(x) length(unique(x)))
  if (any(tab > 1)) {
    warning("A substance maps to multiple concentration levels. Fold change may be inconsistent with model.")
  }
}
env_primary_assay_V1_2$check_mapping <- check_mapping

pa_continous_internal <- function(formula, df, name_column, neg_control_name, pval_adj_method) {
  fit <- lm(formula, data = df)
  emm <- emmeans::emmeans(fit, name_column)
  res <- emmeans::contrast(emm, method = "trt.vs.ctrl", ref = neg_control_name, adjust = pval_adj_method)
  res <- broom::tidy(res) |> as.data.frame()
  res <- res[, c(2, 4, 8)]
  names(res) <- c("name", "Standard Value", "adj. p value")
  pattern <- paste0("\\s*-\\s*", neg_control_name, "$")
  res$name <- gsub(pattern, "", res$name)
  res
}
env_primary_assay_V1_2$pa_continous_internal <- pa_continous_internal

add_fold_change_and_percentage <- function(res, df, dep, indep, neg_control_name, name_column) {
  if (neg_control_name == "") {
    stop("You have to define a name for the negative control")
  }
  # Fold change
  mean_neg <- mean(df[df[, indep] == neg_control_name, dep], na.rm = TRUE)
  means <- vapply(res$name, function(x) {
    mean(df[df[, name_column] == x, dep], na.rm = TRUE)
  }, numeric(1L))
  res$fold_change <- means / mean_neg

  # Difference
  res$percent_of_control <- (means / mean_neg) * 100
  res$unit <- "%"

  res$note <- "Effect size ≠ model estimate"
  res
}
env_primary_assay_V1_2$add_fold_change_and_percentage <- add_fold_change_and_percentage

pa <- function(df, formula, name_column, neg_control_name, pos_control_name, pval_adj_method) {
  indep <- as.character(formula)[3]
  dep <- as.character(formula)[2]
  env_primary_assay_V1_2$check_mapping(df, indep, name_column)
  df <- env_primary_assay_V1_2$pos_norm(df, dep, indep, pos_control_name)
  res <- env_primary_assay_V1_2$pa_continous_internal(formula, df, name_column, neg_control_name, pval_adj_method)
  env_primary_assay_V1_2$add_fold_change_and_percentage(
    res, df, dep, indep, neg_control_name, name_column
  )
}
env_primary_assay_V1_2$pa <- pa

pa_binomial_internal <- function(formula, df, name_column, neg_control_name, pval_adj_method) {
  fit <- glm(formula, data = df, family = "binomial")
  emm <- emmeans::emmeans(fit, name_column)
  res <- emmeans::contrast(emm, method = "trt.vs.ctrl", ref = neg_control_name, adjust = pval_adj_method)
  res <- broom::tidy(res) |> as.data.frame()
  res <- res[, c(2, 4, 8)]
  names(res) <- c("name", "Standard Value", "adj. p value")
  pattern <- paste0("\\s*-\\s*", neg_control_name, "$")
  res$name <- gsub(pattern, "", res$name)
  res
}
env_primary_assay_V1_2$pa_binomial_internal <- pa_binomial_internal

pa_binomial <- function(df, formula, name_column, neg_control_name, pval_adj_method) {
  indep <- as.character(formula)[3]
  dep <- as.character(formula)[2]
  env_primary_assay_V1_2$check_mapping(df, indep, name_column)
  res <- env_primary_assay_V1_2$pa_binomial_internal(formula, df, name_column, neg_control_name, pval_adj_method)
  env_primary_assay_V1_2$add_fold_change_and_percentage(
    res, df, dep, indep, neg_control_name, name_column
  )
}
env_primary_assay_V1_2$pa_binomial <- pa_binomial

pa_count_internal <- function(formula, df, name_column, neg_control_name, pval_adj_method) {
  fit <- glm(formula, data = df, family = "poisson")
  emm <- emmeans::emmeans(fit, name_column)
  res <- emmeans::contrast(
    emm,
    method = "trt.vs.ctrl",
    ref = neg_control_name,
    adjust = pval_adj_method
  )
  res <- broom::tidy(res) |> as.data.frame()
  res <- res[, c(2, 4, 8)]
  names(res) <- c("name", "Standard Value", "adj. p value")
  pattern <- paste0("\\s*-\\s*", neg_control_name, "$")
  res$name <- gsub(pattern, "", res$name)
  res
}
env_primary_assay_V1_2$pa_count_internal <- pa_count_internal

pa_count <- function(df, formula, name_column, neg_control_name, pval_adj_method) {
  indep <- as.character(formula)[3]
  dep   <- as.character(formula)[2]
  res <- env_primary_assay_V1_2$pa_count_internal(
    formula, df, name_column, neg_control_name, pval_adj_method
  )
  env_primary_assay_V1_2$add_fold_change_and_percentage(
    res, df, dep, indep, neg_control_name, name_column
  )
}
env_primary_assay_V1_2$pa_count <- pa_count

df <- read.csv("./test_data/primary_data.csv")
res <- env_primary_assay_V1_2$pa(
  df, values ~ substances, "substances", "neg", "pos", "holm"
)
cat("Continous\n")
print(res)
cat("=====================\n")

df <- data.frame(
  substances = c(
    rep("S1", 5L),
    rep("S2", 5L),
    rep("S3", 5L),
    rep("neg", 5L),
    rep("pos", 5L)
  ),
  values = c(
    rbinom(5, 1, 0.95),
    rbinom(5, 1, 0.75),
    rbinom(5, 1, 0.45),
    rbinom(5, 1, 1),
    rbinom(5, 1, 0.0005)
  )
)
res <- env_primary_assay_V1_2$pa_binomial(
  df, values ~ substances, "substances", "neg", "holm"
)
cat("Binomial\n")
print(res)
cat("=====================\n")

set.seed(1234)
df <- data.frame(
  substances = c(
    rep("S1", 10),
    rep("S2", 10),
    rep("S3", 10),
    rep("neg", 10)
  ),
  counts = c(
    rpois(10, lambda = 120),  # strong effect
    rpois(10, lambda = 80),   # medium effect
    rpois(10, lambda = 40),   # weak effect
    rpois(10, lambda = 100)   # control
  )
)
res <- env_primary_assay_V1_2$pa_count(
  df, counts ~ substances, "substances", "neg", "holm"
)
cat("Count\n")
print(res)
cat("=====================\n")
# Missing multinomial primary assay
