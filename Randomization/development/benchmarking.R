# Pure R vs ast2ast, SAME loss function on both sides.
#   ast2ast path : ast2ast::translate(loss_m2cov_vs_all_perm)
#   pure R path  : the identical function run by the R interpreter, with the type
#                  markers (argtypes/type/mat/vec/int) stubbed to no-ops
# Both go through R/global_match.R::greedy_optimize; only loss_ast differs, so the
# gap is attributable to ast2ast alone, not to two different implementations.
# The "pure R" time includes the DSL-style element loops (NA mask) -- same code,
# not idiomatic R. Run from Documents/OpenStats/.

suppressPackageStartupMessages({
  library(microbenchmark)
  library(ggplot2)
})

gm <- new.env()
sys.source("./Randomization/R/global_match.R", envir = gm)

# translate BEFORE stubbing: translation needs the pristine function
loss_ast <- ast2ast::translate(gm$loss_m2cov_vs_all_perm)

gm$argtypes <- function(...) invisible(NULL)
gm$type     <- function(x, ...) x
gm$mat      <- function(...) NULL
gm$vec      <- function(...) NULL
gm$int      <- function(...) NULL
loss_r <- gm$loss_m2cov_vs_all_perm

make_case <- function(n_per_group, n_cov = 2L, n_groups = 3L, seed = 3344) {
  n <- n_per_group * n_groups
  design <- data.frame(treatment = rep(paste0("g", seq_len(n_groups)), each = n_per_group))
  set.seed(seed)
  groups <- as.data.frame(matrix(rnorm(n * n_cov, 100, 10), n, n_cov))
  names(groups) <- paste0("x", seq_len(n_cov))
  list(design = design, groups = groups)
}

solve_with <- function(case, loss_fun, seed = 1234, max_iter = 50L) {
  blocks <- as.factor(interaction(case$design))
  set.seed(seed)
  perms <- sample.int(nrow(case$groups))
  gm$greedy_optimize(
    case$groups, blocks, perms,
    max_iter = max_iter, verbose = FALSE,
    loss_ast = loss_fun, loss_function = "Default"
  )
}

# per group; realistic = 16 (N = 48)
sizes <- c(8L, 16L, 24L)

invisible(solve_with(make_case(sizes[1L]), loss_ast))  # warm up first-call costs

results <- do.call(rbind, lapply(sizes, function(m) {
  case <- make_case(m)
  b <- microbenchmark(
    ast2ast = solve_with(case, loss_ast),
    R       = solve_with(case, loss_r),
    times   = 5L
  )
  data.frame(
    n_per_group = m,
    N           = m * 3L,
    impl        = as.character(b$expr),
    time_ms     = b$time / 1e6
  )
}))
results$impl[results$impl == "R"] <- "pure R"

agg <- aggregate(time_ms ~ N + impl, results, median)
m_r <- agg[agg$impl == "pure R", c("N", "time_ms")]
m_a <- agg[agg$impl == "ast2ast", c("N", "time_ms")]
tab <- merge(m_r, m_a, by = "N", suffixes = c("_R", "_ast2ast"))
tab$speedup <- tab$time_ms_R / tab$time_ms_ast2ast
print(tab, row.names = FALSE)

p <- ggplot(results, aes(N, time_ms, colour = impl)) +
  stat_summary(fun = median, geom = "line") +
  stat_summary(fun = median, geom = "point") +
  geom_vline(xintercept = 16L * 3L, linetype = "dashed", colour = "grey50") +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "N (total units)", y = "median time [ms]", colour = NULL,
       title = "random_finite_assign: same loss, pure R vs ast2ast",
       subtitle = "dashed line = realistic size (16 / group)") +
  theme_minimal()
p
# ggsave("./Randomization/development/benchmark.png", p, width = 7, height = 4, dpi = 150)
