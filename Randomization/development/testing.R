tinytest::test_package("Randomization")
devtools::load_all(".")

library(ggplot2)

pl <- function(res) {
  res$random_group <- as.numeric(res$random_group)
  global_df <- data.frame(
    value = weights,
    source = "Global"
  )
  block_df <- data.frame(
    value = res$random_group,
    source = "Assigned",
    Treatment = res$Treatment,
    cellLines = res$cellLines
  )
  p1 <- ggplot() +
    geom_density(data = global_df,
      aes(x = value, linetype = source),
      linewidth = 1) +
    geom_density(data = block_df,
      aes(x = value, color = Treatment),
      alpha = 0.6) +
    labs(
      title = "Global vs Block-wise Weight Distributions",
      x = "Weight",
      y = "Density"
    ) +
    theme_minimal()
  p2 <- ggplot() +
    geom_density(data = global_df,
      aes(x = value, linetype = source),
      linewidth = 1) +
    geom_density(data = block_df,
      aes(x = value, color = cellLines),
      alpha = 0.6) +
    labs(
      title = "Global vs Block-wise Weight Distributions",
      x = "Weight",
      y = "Density"
    ) +
    theme_minimal()
  cowplot::plot_grid(p1, p2, ncol = 1L)
}
predictors <- list(
  cellLines = c("HeLa", "Hek"),
  Treatment = LETTERS[1:4]
)
primary_factor <- "Treatment"
set.seed(1234)
weights <- rnorm(550, 300, 25)
Randomization::randomization_one_way_anova(
  predictors = predictors, primary_factor = "Treatment",
  groups = weights, group_type = "finite",
  cohens_f = 0.25, sig_level = 0.05, desired_power = 0.9,
  randomization_method = "block_stratified",
  strata_cols = c("cellLines", "Treatment"),
  seed = 1234
) |> pl()
