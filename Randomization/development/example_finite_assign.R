design <- data.frame(
  treatment = rep(c("control", "one", "two"), each = 10L)
)

# Finite population with covariate and explicit ID
groups <- data.frame(
  weight = rnorm(30, mean = 300, sd = 25)
)

animal_ids <- paste0("Animal_", seq_len(nrow(groups)))

res <- Randomization::random_finite_assign(
  seed = 42,
  groups = groups,
  design = design,
  ids = animal_ids,
  loss_function = "Default"
)

head(res$assigned)
head(res$unit_id)
res$unit_id
res
design
