env_dose_response_activation_V1_2 <- new.env(parent = getNamespace("OpenStats"))

root_finding_activation <- function(df, abs, conc, fold_target, ref_conc = 0.1) {
  model <- drc::drm(
    formula = as.formula(paste(abs, "~", conc)),
    data = df,
    fct = drc::LL.4(),
    robust = "median"
  )
  coefs <- coef(model)
  b <- coefs[[1L]]
  c0 <- coefs[[2L]]
  d <- coefs[[3L]]
  e <- coefs[[4L]]

  f <- function(x) {
    c0 + (d - c0) / (1 + (x / e)^b)
  }
  ref_val <- f(ref_conc)

  max_fold <- max(f(min(df[[conc]])), f(max(df[[conc]]) * 100)) / ref_val
  if (fold_target > max_fold) {
    stop(sprintf(
      "Target fold activation %.3f is outside model range. Maximum possible is about %.3f.", fold_target, max_fold
    ))
  }

  loss <- function(x) {
    (f(x) / ref_val) - fold_target
  }
  uniroot(loss, c(1e-8, 1e6))$root
}
env_dose_response_activation_V1_2$root_finding_activation <- root_finding_activation

set.seed(1234)
df <- data.frame(
  conc = c(
    rep(0.1, 5),
    rep(0.5, 5),
    rep(1, 5),
    rep(2.5, 5),
    rep(5, 5),
    rep(10, 5),
    rep(15, 5),
    rep(20, 5),
    rep(25, 5),
    rep(30, 5)
  ),
  abs = c(
    rnorm(5, 0.1, 0.005),
    rnorm(5, 0.2, 0.005),
    rnorm(5, 0.3, 0.005),
    rnorm(5, 0.4, 0.005),
    rnorm(5, 0.5, 0.005),
    rnorm(5, 0.6, 0.005),
    rnorm(5, 0.7, 0.005),
    rnorm(5, 0.8, 0.005),
    rnorm(5, 0.9, 0.005),
    rnorm(5, 1, 0.005)
  )
)
env_dose_response_activation_V1_2$root_finding_activation(df, "abs", "conc", 7.8, 0.1)
