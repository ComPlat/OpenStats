# BO (LHS start, 4*d points, then
# bo_suggest) vs pure LHS, equal budget.
# Uses installed OpenDOE (internals via :::).
# Run from Documents/OpenStats/.
# Slow: each BO step fits a GP or a RF.

n_reps <- 10L

branin <- function(x1, x2) {
  a <- 5.1 / (4 * pi^2)
  b <- 5 / pi
  s <- 10 * (1 - 1 / (8 * pi))
  (x2 - a * x1^2 + b * x1 - 6)^2 +
    s * cos(x1) + 10
}

cat_offset <- c(A = 0, B = 5, C = 10)

problems <- list(
  sin_1d = list(
    predictors = list(x = c("0", "5")),
    types = c(x = "continuous"),
    f = function(df) -sin(df$x),
    f_opt = -1,
    noise_sd = 0.1,
    budget = 12L
  ),
  branin_2d = list(
    predictors = list(
      x1 = c("-5", "10"),
      x2 = c("0", "15")
    ),
    types = c(
      x1 = "continuous",
      x2 = "continuous"
    ),
    f = function(df) branin(df$x1, df$x2),
    f_opt = 0.397887,
    noise_sd = 1,
    budget = 20L
  ),
  branin_mixed = list(
    predictors = list(
      x1 = c("-5", "10"),
      x2 = c("0", "15"),
      cat = c("A", "B", "C")
    ),
    types = c(
      x1 = "continuous",
      x2 = "continuous",
      cat = "categorical"
    ),
    f = function(df) {
      branin(df$x1, df$x2) +
        unname(cat_offset[df$cat])
    },
    f_opt = 0.397887,
    noise_sd = 1,
    budget = 24L
  )
)

# Regret: noise free f at the best
# *observed* point, i.e. what the chemist
# would pick from the table
regret <- function(problem, df) {
  i <- which.min(df$y)
  best <- df[i, , drop = FALSE]
  problem$f(best) - problem$f_opt
}

run_lhs <- function(problem, seed, noise) {
  df <- OpenDOE:::lhs_design(
    problem$predictors, problem$types,
    problem$budget, seed
  )
  df$y <- problem$f(df) + noise
  df
}

run_bo <- function(problem, seed, noise,
                   surrogate = "rf") {
  d <- length(problem$predictors)
  df <- OpenDOE:::lhs_design(
    problem$predictors, problem$types,
    4L * d, seed
  )
  n <- nrow(df)
  df$y <- problem$f(df) + noise[seq_len(n)]
  while (nrow(df) < problem$budget) {
    df <- OpenDOE:::run_bo_suggest(
      df, problem$types, "y", "minimize",
      seed * 1000L + nrow(df), surrogate
    )
    i <- nrow(df)
    row <- df[i, , drop = FALSE]
    df$y[i] <- problem$f(row) + noise[i]
  }
  df
}

run_rep <- function(name, rep) {
  problem <- problems[[name]]
  # noise drawn up front: run_bo_suggest
  # reseeds the RNG on every call
  set.seed(rep)
  noise <- stats::rnorm(
    problem$budget, sd = problem$noise_sd
  )
  t0 <- Sys.time()
  runs <- list(
    LHS = run_lhs(problem, rep, noise),
    BO = run_bo(problem, rep, noise)
  )
  # GP variant only differs with
  # categorical predictors
  if (any(problem$types == "categorical")) {
    runs[["BO-GP"]] <- run_bo(
      problem, rep, noise, "gp_onehot"
    )
  }
  dt <- round(Sys.time() - t0, 1)
  message(name, " rep ", rep, " (", dt, ")")
  data.frame(
    problem = name, rep = rep,
    method = names(runs),
    regret = vapply(
      runs, function(df) regret(problem, df),
      numeric(1)
    )
  )
}

grid <- expand.grid(
  name = names(problems),
  rep = seq_len(n_reps),
  stringsAsFactors = FALSE
)
results <- do.call(rbind, Map(
  run_rep, grid$name, grid$rep
))

print(aggregate(
  regret ~ problem + method,
  results, median
))

par(mfrow = c(1, length(problems)))
for (name in names(problems)) {
  sub <- results[results$problem == name, ]
  boxplot(
    regret ~ method, sub, main = name,
    ylab = "regret (lower is better)"
  )
}
