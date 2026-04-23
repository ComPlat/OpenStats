env_TGI_V1_2 <- new.env(parent = getNamespace("OpenStats"))

tgi_area <- function(time_treatment, treatment, time_control, control) {
  stopifnot(
    is.numeric(time_treatment),
    is.numeric(treatment),
    is.numeric(time_control),
    is.numeric(control),
    length(time_treatment) == length(treatment),
    length(time_control) == length(control),
    length(treatment) > 1L,
    length(control) > 1L
  )

  auc <- function(x, y) {
    sum(diff(x) * (head(y, -1L) + tail(y, -1L)) / 2)
  }

  auc_treatment <- auc(time_treatment, treatment)
  auc_control <- auc(time_control, control)

  1 - (auc_treatment / auc_control)
}
env_TGI_V1_2$tgi_area <- tgi_area

tgi_slope <- function(time_treatment, treatment, time_control, control) {
  stopifnot(
    is.numeric(time_treatment),
    is.numeric(treatment),
    is.numeric(time_control),
    is.numeric(control),
    length(time_treatment) == length(treatment),
    length(time_control) == length(control),
    length(treatment) > 1L,
    length(control) > 1L
  )

  slope_treatment <- coef(lm(treatment ~ time_treatment))[[2L]]
  slope_control <- coef(lm(control ~ time_control))[[2L]]

  1 - (slope_treatment / slope_control)
}
env_TGI_V1_2$tgi_slope <- tgi_slope
