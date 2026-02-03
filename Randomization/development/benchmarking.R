n_blocks <- data.frame(
  treatment = rep(c("control", "one", "two"), each = 16)
)

set.seed(3344)
groups <- data.frame(
  weights = rnorm(48, 300, 25),
  blood   = rnorm(48, 100, 1)
)

Cpp <- function() {
  Randomization::random_finite_assign(
    seed = 1234,
    groups = groups,
    design = n_blocks,
    max_iter = 1000L
  )
}
Cpp() # for compilation

source("./Randomization/development/global_matching.R")
R <- function() {
  set.seed(1234)
  greedy_optimize(
    groups, as.factor(interaction(n_blocks)),
    seq.int(nrow(groups)), mode = "loss_mean_m2_cov_group_vs_all"
  )
}

b <- microbenchmark::microbenchmark(
  Cpp(),
  R(),
  times = 10L
)
b

p <- ggplot2::autoplot(b)
ggplot2::ggsave("./Randomization/development/benchmark.png", p)
