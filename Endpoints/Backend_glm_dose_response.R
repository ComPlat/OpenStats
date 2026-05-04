errorClass <- R6::R6Class("errorClass", # not exported via env_lc_V1_2 as engine tests whether the result inherits from errorClass
  public = list(
    error_message = NULL,
    object = NULL,
    initialize = function(error_message = NULL) {
      self$error_message <- error_message
    },
    isNull = function() {
      if (is.null(self$error_message)) {
        return(TRUE)
      }
      return(FALSE)
    }
  )
)

env_lc_V1_2 <- new.env(parent = getNamespace("OpenStats"))
shapenumber <- function(num) {
  if (is.finite(num)) {
    res <- signif(num)
  } else {
    res <- NA
  }
  return(res)
}
env_lc_V1_2$shapenumber <- shapenumber

get_residuals <- function(model, type) {
  if (type == "continuous") {
    return(residuals(model))
  }
  resids <- residuals(model)# y - mu, raw
  mu <- fitted(model)
  eps<- .Machine$double.eps
  if (type %in% c("binomial", "quantal")) {
    n  <- weights(model)
    if (is.null(n)) n <- 1L
    mu <- pmin(pmax(mu, eps), 1 - eps)
    resids / sqrt(mu * (1 - mu) / n)
  } else if (type == "Poisson") {
    resids / sqrt(pmax(mu, eps))
  } else {
    stop("unsupported type: ", type)
  }
}
env_lc_V1_2$get_residuals <- get_residuals

# calculates the robust 68th percentile of the residuals
# adapted from Motulsky HJ, Brown RE, BMC Bioinformatics 2006, 7:123
robust_68_percentile <- function(residuals) {
  res <- abs(residuals)
  res_sorted <- sort(res)
  res_percentiles <- (seq_len(length(res_sorted)) / length(res_sorted))
  index <- min(which(res_percentiles > 0.6825))
  x <- c(res_percentiles[index - 1], res_percentiles[index])
  y <- c(res_sorted[index - 1], res_sorted[index])
  m <- lm(y ~ x)
  x <- 0.6825
  y <- predict(m, as.data.frame(x))
  return(y)
}
env_lc_V1_2$robust_68_percentile <- robust_68_percentile

# calculates the robust standard deviation of the residuals (RSDR)
# with correction for degrees of freedom
# adapted from Motulsky HJ, Brown RE, BMC Bioinformatics 2006, 7:123
# robust_standard_deviation_residuals = env_lc_V1_2$rsdr
rsdr <- function(residuals, number_of_coefficients_fitted) {
  resids <- as.numeric(residuals)
  resids <- na.omit(residuals)
  N <- length(resids)
  env_lc_V1_2$robust_68_percentile(residuals) *
    N / (N - number_of_coefficients_fitted)
}
env_lc_V1_2$rsdr <- rsdr

# false discovery rate (FDR) approach,
# returns a T/F vector for selection of valid data points
# adapted from Motulsky HJ, Brown RE, BMC Bioinformatics 2006, 7:123
false_discovery_rate <- function(model, type) {
  res <- get_residuals(model, type)
  N <- length(res)
  K <- length(coef(model))
  df <- data.frame(id = seq_along(res), res = res)
  df$res_abs <- abs(res)
  df <- df[order(df$res_abs), ]
  df$i <- seq_len(N)
  Q <- 0.01
  df$alpha <- Q * (N - (df$i - 1)) / N
  if (type == "continuous") {
    R <- env_lc_V1_2$rsdr(res, K)
    df$stat <- df$res_abs / R
    df$P <- 2 * pt(-abs(df$stat), df = N - K)
  } else {
    df$stat <- df$res
    df$P <- 2 * pnorm(-abs(df$stat))
  }
  df$include <- !(df$P < df$alpha & df$i / N >= 0.7)
  df <- df[order(df$id), ]
  df$include
}
env_lc_V1_2$false_discovery_rate <- false_discovery_rate

check_fit <- function(model, ic_percentage, min_conc, max_conc,
                      min_abs, max_abs, substance_name, unit, type) {
  if (model$fit$convergence != TRUE) {
    return(errorClass$new(paste(
      substance_name,
      "Model did not converge"
    )))
  }
  b <- coefficients(model)[1] # Hill coefficient
  c <- coefficients(model)[2] # asymptote 1
  d <- coefficients(model)[3] # asymptote 2
  e <- coefficients(model)[4] # IC50
  RSE <- summary(model)$rseMat[1] # residual standard error estimated
  Response_lowestdose_predicted <- predict(
    model, data.frame(conc = min_conc),
    se.fit = FALSE
  )[1]
  Response_highestdose_predicted <- predict(
    model, data.frame(conc = max_conc),
    se.fit = FALSE
  )[1]
  Response_difference <- abs(
    Response_lowestdose_predicted - Response_highestdose_predicted
  )
  HillCoefficient <- b
  ed_res <- drc::ED(
    model,
    respLev = ic_percentage,
    interval = "delta",
    level = 0.95,
    type = "relative",
    display = FALSE
  )
  IC_relative <- ed_res[1, 1]
  Problems <- ""
  if (Response_difference < 0.25) {
    Problems <- paste(Problems,
      "Response Difference lower than 25%", collapse = " , "
    )
  } else if (IC_relative > max_conc) {
    Problems <- paste(Problems,
      "IC larger than highest measured concentration", collapse = " , "
    )
  } else if (IC_relative < min_conc) {
    Problems <- paste(Problems,
      "IC lower than lowest measured concentration", collapse = " , "
    )
  }

  IC_relative_lower <- ed_res[1, 3]
  IC_relative_higher <- ed_res[1, 4]
  p_value <- drc::noEffect(model)[3]
  Response_lowestdose_predicted <- env_lc_V1_2$shapenumber(Response_lowestdose_predicted)
  Response_highestdose_predicted <- env_lc_V1_2$shapenumber(Response_highestdose_predicted)
  HillCoefficient <- env_lc_V1_2$shapenumber(HillCoefficient)
  IC_relative <- env_lc_V1_2$shapenumber(IC_relative)
  IC_relative_lower <- env_lc_V1_2$shapenumber(IC_relative_lower)
  IC_relative_higher <- env_lc_V1_2$shapenumber(IC_relative_higher)
  pIC <- env_lc_V1_2$shapenumber(-log10(IC_relative))
  p_value <- env_lc_V1_2$shapenumber(p_value)
  outvar <- data.frame(
    name = substance_name,
    Response_lowestdose_predicted = Response_lowestdose_predicted,
    Response_highestdose_predicted = Response_highestdose_predicted,
    HillCoefficient = HillCoefficient,
    asymptote_one = c, asymptote_two = d,
    IC_relative = IC_relative, IC_relative_lower = IC_relative_lower, IC_relative_higher = IC_relative_higher,
    unit = unit, pIC = pIC,
    RSE = RSE, p_value = p_value, Problems = Problems
  )
  names(outvar)[7:9] <- c(
    paste0("IC_", ic_percentage, "_relative"),
    paste0("IC_", ic_percentage, "_relative_lower"),
    paste0("IC_", ic_percentage, "_relative_higher")
  )

  return(outvar)
}
env_lc_V1_2$check_fit <- check_fit

ic_internal <- function(df, ic_percentage, abs, conc, unit, title, type) {
  model <- drc::drm(abs ~ conc,
    data = df, fct = drc::LL.4(),
    robust = "median", type = type
  )
  valid_points <- env_lc_V1_2$false_discovery_rate(model, type)
  model <- drc::drm(abs ~ conc,
    data = df,
    subset = valid_points,
    start = model$coefficients,
    fct = drc::LL.4(), robust = "mean",
  )
  env_lc_V1_2$check_fit(
    model, ic_percentage, min(df[, conc]),
    max(df[, conc]), min(df[, abs]), max(df[, abs]), title, unit, type
  )
}
env_lc_V1_2$ic_internal <- ic_internal

set.seed(1234)
df_binomial <- data.frame(
  conc = c(
    rep(0.5, 5L),
    rep(2.5, 5L),
    rep(5, 5L),
    rep(10, 5L),
    rep(15, 5L),
    rep(20, 5L),
    rep(25, 5L)
  ),
  abs = c(
    rbinom(5, 1, 0.85),
    rbinom(5, 1, 0.75),
    rbinom(5, 1, 0.65),
    rbinom(5, 1, 0.55),
    rbinom(5, 1, 0.35),
    rbinom(5, 1, 0.25),
    rbinom(5, 1, 0.15)
  ),
  unit = "Whatever"
)
env_lc_V1_2$ic_internal(df_binomial, 0.5, 2L, 1L, 3L, "Test binomial", "binomial")

# continuous: Gaussian-distributed response decreasing with dose
set.seed(1234)
df_continuous <- data.frame(
  conc = rep(c(0.5, 2.5, 5, 10, 15, 20, 25), each = 5),
  abs = c(
    rnorm(5, 0.85, 0.05),
    rnorm(5, 0.75, 0.05),
    rnorm(5, 0.65, 0.05),
    rnorm(5, 0.55, 0.05),
    rnorm(5, 0.35, 0.05),
    rnorm(5, 0.25, 0.05),
    rnorm(5, 0.15, 0.05)
  ),
  unit = "Whatever"
)
env_lc_V1_2$ic_internal(df_continuous, 0.5, 2L, 1L, 3L, "Test continuous", "continuous")

# Poisson: counts decreasing with dose
set.seed(1234)
df_poisson <- data.frame(
  conc = rep(c(0.5, 2.5, 5, 10, 15, 20, 25), each = 5),
  abs = c(
    rpois(5, 85),
    rpois(5, 75),
    rpois(5, 65),
    rpois(5, 55),
    rpois(5, 35),
    rpois(5, 25),
    rpois(5, 15)
  ),
  unit = "Whatever"
)
env_lc_V1_2$ic_internal(df_poisson, 0.5, 2L, 1L, 3L, "Test Poisson", "Poisson")

# quantal (0/1 outcomes per individual) is listed in drc::drm's formals but
# is not actually implemented — the dispatch in drm() has no branch for it,
# so estMethod is never set and the call fails with
#   "object 'estMethod' not found".
# Use type = "binomial" with Bernoulli (size = 1) data instead — see the
# rbinom(5, 1, p) example at the top of this block.
if (FALSE) {
  set.seed(1234)
  df_quantal <- data.frame(
    conc = rep(c(0.5, 2.5, 5, 10, 15, 20, 25), each = 5),
    abs = c(
      rbinom(5, 1, 0.85),
      rbinom(5, 1, 0.75),
      rbinom(5, 1, 0.65),
      rbinom(5, 1, 0.55),
      rbinom(5, 1, 0.35),
      rbinom(5, 1, 0.25),
      rbinom(5, 1, 0.15)
    ),
    unit = "Whatever"
  )
  env_lc_V1_2$ic_internal(df_quantal, 0.5, 2L, 1L, 3L, "Test quantal", "quantal")
}
