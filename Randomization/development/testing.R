if (!grepl("Randomization", getwd())) {
  setwd("./Randomization")
}
ast2ast::make_package(
  files = file.path("inst", "tools", "global_match.R"),
  entry_points = c(
    "loss_m2cov_vs_all_perm", "loss_mahal_perm",
    "best_swap_m2cov", "best_swap_mahal"
  ),
  package_dir = "."
)
install.packages(".", repos = NULL, type = "source")
tinytest::test_package("Randomization")
