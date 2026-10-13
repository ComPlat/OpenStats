# make_package migration check: random_finite_assign of the installed
# Randomization has to give bit-identical results to the translate() version
# of commit e6c8d1a. Run from the OpenStats repository root after installing.
old <- new.env()
source_code <- system2("git", c("show", "e6c8d1a:Randomization/R/global_match.R"), stdout = TRUE)
eval(parse(text = source_code), envir = old)

set.seed(1)
n <- 90L
groups <- data.frame(
  weight = rnorm(n, 300, 25),
  age = rnorm(n, 10, 2),
  score = c(rnorm(n - 5L, 50, 10), rep(NA, 5L))
)
design <- data.frame(
  treatment = rep(c("control", "one", "two"), each = 28L),
  cells = rep(c("HeLa", "Hek"), times = 42L)
)

compare <- function(...) {
  a <- old$random_finite_assign(...)
  b <- Randomization::random_finite_assign(...)
  stopifnot(identical(a$perm, b$perm), identical(a$loss, b$loss), identical(a$assigned, b$assigned))
}

# Mahalanobis without NA: stats::cov() gives NA, the old version stops with an error
groups_complete <- groups
groups_complete$score[is.na(groups_complete$score)] <- 50
for (seed in c(42, 1234, 3344)) {
  compare(seed, groups, design, loss_function = "Default")
  compare(seed, groups_complete, design, loss_function = "Mahalanobis")
}
compare(7, groups, design, w = c(2, 1, 0.5))
compare(7, groups, design, lambda_m2 = 0.5, lambda_cov = 2)
cat("random_finite_assign: make_package and translate() are bit-identical\n")

old_time <- system.time(old$random_finite_assign(42, groups, design))[["elapsed"]]
new_time <- system.time(Randomization::random_finite_assign(42, groups, design))[["elapsed"]]
cat(sprintf("Default loss: old %.2f s (incl. translate), new %.2f s\n", old_time, new_time))
