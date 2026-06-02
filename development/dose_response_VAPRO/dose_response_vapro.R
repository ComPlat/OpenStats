simulate <- function(name, slope, true_ic50) {
  b  <- slope         # slope
  c  <- 0.05          # lower limit
  d  <- 1.1           # upper limit
  e  <- true_ic50     # IC50
  set.seed(42)
  conc_levels <- c(0.1, seq(2.5, 26, by = 2.5))
  conc <- rep(conc_levels, each = 5)
  # Generate response
  logistic_response <- function(conc, b, c, d, e) {
    c + (d - c) / (1 + (conc / e)^b)
  }
  abs <- logistic_response(conc, b, c, d, e) + rnorm(length(conc), sd = 0.05)
  data.frame(
    substance = rep(name, length(conc)), conc = conc, abs = abs, unit = "M"
  )
}

ll4 <- function(b, e, df, get_params = FALSE) {
  g <- 1 / (1 + exp(b * (log(df$conc) - log(e))))
  X <- cbind(1 - g, g)
  beta_hat <- qr.solve(X, df$abs)
  c_hat <- beta_hat[[1L]]
  d_hat <- beta_hat[[2L]]
  if (c_hat > d_hat) {
    tmp <- c_hat
    c_hat <- d_hat
    d_hat <- tmp
    b <- -b
    g <- 1 / (1 + exp(b * (log(df$conc) - log(e))))
  }
  abs_in_silico <- c_hat * (1 - g) + d_hat * g
  if (get_params) return(c(b = b, c = c_hat, d = d_hat, e = e))
  sum((df$abs - abs_in_silico)^2)
}

define_grid <- function(n, b_start, b_end, df, eps = 1e-6) {
  b <- seq(b_start, b_end, length.out = n)
  b <- b[abs(b) > eps]
  log_e <- seq(log(min(df$conc)) - 1, log(max(df$conc)) + 1, length.out = n)
  e <- exp(log_e)
  expand.grid(b, e)
}

eval_grid <- function(df, grid) {
  errors <- lapply(seq_len(nrow(grid)), function(r) {
    pair <- grid[r, ]
    res <- try(ll4(pair[[1L]], pair[[2L]], df), silent = TRUE)
    if (inherits(res, "try-error")) return(NA)
    res
  })
  errors_vec <- unlist(errors)
  valid_idx <- which(!is.na(errors_vec))
  best <- valid_idx[which.min(errors_vec[valid_idx])]
  best <- grid[best, ]
  params <- ll4(best[[1]], best[[2]], df, TRUE)
  list(params = params, errors_vec = errors_vec)
}

calc_uncertainty <- function(df, errors_vec, grid) {
  e_vals <- unique(grid$Var2)
  profile_e <- sapply(e_vals, function(e_val) {
    idx <- which(grid$Var2 == e_val)
    min(errors_vec[idx], na.rm = TRUE)
  })
  names(profile_e) <- e_vals
  rss_min <- min(profile_e)
  n_obs <- nrow(df)
  p <- 4L
  sigma2 <- rss_min / (n_obs - p)
  threshold <- rss_min + qchisq(0.95, df = 1) * sigma2
  valid_e <- as.numeric(names(profile_e))[profile_e <= threshold]
  CI_e <- range(valid_e)
  CI_e
}

calc_xy <- function(b, e, xy_percent) {
  p <- xy_percent / 100
  if (p <= 0 || p >= 1) {
    stop("xy_percent must be strictly between 0 and 100")
  }
  e * ((1 - p) / p)^(1 / b)
}

calc_uncertainty_xy <- function(df, grid, errors_vec, xy_percent) {
  p <- xy_percent / 100
  b_vals <- unique(grid$Var1)
  ic_vals <- unique(grid$Var2)
  
  profile_ic <- sapply(ic_vals, function(ic_val) {
    rss_vals <- sapply(b_vals, function(b_val) {
      e_val <- ic_val * (p / (1 - p))^(1 / b_val)
      if (!is.finite(e_val) || e_val <= 0) return(NA)
      ll4(b_val, e_val, df)
    })
    min(rss_vals, na.rm = TRUE)
  })
  names(profile_ic) <- ic_vals
  rss_min <- min(profile_ic, na.rm = TRUE)
  n_obs <- nrow(df)
  p_param <- 4L
  sigma2 <- rss_min / (n_obs - p_param)
  threshold <- rss_min + qchisq(0.95, df = 1) * sigma2
  valid_ic <- as.numeric(names(profile_ic))[profile_ic <= threshold]
  range(valid_ic)
}

df <- simulate("A", 7, 10)
n <- 300L
grid <- define_grid(n, -20, 20, df)
res <- eval_grid(df, grid)
params <- res$params
params
errors_vec <- res$errors_vec
calc_uncertainty(df, errors_vec, grid)

ll4_optim <- function(par, df) {
  b <- par[1]
  e <- par[2]
  if (!is.finite(e) || e <= 0 || abs(b) < 1e-6) {
    return(Inf)
  }
  ll4(b, e, df)
}
optim(c(5, 5), ll4_optim, df = df, method = "BFGS")

calc_xy(params[["b"]], params[["e"]], 10)
calc_xy(params[["b"]], params[["e"]], 50)
calc_xy(params[["b"]], params[["e"]], 90)
calc_uncertainty_xy(df, grid, errors_vec, 10)
calc_uncertainty_xy(df, grid, errors_vec, 50)
calc_uncertainty_xy(df, grid, errors_vec, 90)

library(drc)
fit_drc <- drm(abs ~ conc, data = df, fct = LL.4())
coef(fit_drc)
ED(fit_drc, c(10, 50, 90))
