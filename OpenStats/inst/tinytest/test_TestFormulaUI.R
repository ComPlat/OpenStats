sync_code <- function(session) {
  session$setInputs(`FO-editable_code` = session$userData$export_formula_rhs)
  session$flushReact()
}

coverage_test <- nzchar(Sys.getenv("R_COVR"))

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

app <- OpenStats:::app()
srv <- app$server

test_optim_linear_formula <- function(app, srv) {
  tol <- 1e-3
  set.seed(1234)
  x <- seq(1, 10, length.out = 20)
  df <- data.frame(x = x, y = x + rnorm(20))
  linear_expected <- lm(y ~ x, data = df)$coefficients
  linear <- NULL
  shiny::testServer(srv, {
    DataModelState$df <- df
    session$setInputs(`FO-model_type` = "Optimization Model")
    session$setInputs(`FO-PredefinedModels` = "linear")
    session$setInputs(`FO-linear_lhs_var` = "y")
    session$setInputs(`FO-linear_x` = "x")
    session$setInputs(`FO-linear_slope` = "slope_test")
    session$setInputs(`FO-linear_intercept` = "intercept_test")
    session$setInputs(`FO-LowerBoundary` = -10)
    session$setInputs(`FO-UpperBoundary` = 10)
    session$setInputs(`FO-Seed` = 42)
    session$setInputs(`FO-create_formula` = 1)
    linear <<- session$userData$export[[1]]@summary
  })
  expect_true(abs(linear$slope_test - linear_expected[[2]]) < tol)
  expect_true(abs(linear$intercept_test - linear_expected[[1]]) < tol)
}
test_optim_linear_formula(app, srv)

test_optim_log_linear_formula <- function(app, srv) {
  tol <- 1e-3
  set.seed(1234)
  x <- seq(1, 10, length.out = 20)
  df <- data.frame(x = x, y = x + rnorm(20))
  log_linear_expected <- lm(y ~ log(x), data = df)$coefficients
  log_linear <- NULL
  shiny::testServer(srv, {
    DataModelState$df <- df
    session$setInputs(`FO-model_type` = "Optimization Model")
    session$setInputs(`FO-PredefinedModels` = "log_linear")
    session$setInputs(`FO-log_linear_lhs_var` = "y")
    session$setInputs(`FO-log_linear_x` = "x")
    session$setInputs(`FO-log_linear_slope` = "slope_test")
    session$setInputs(`FO-log_linear_intercept` = "intercept_test")
    session$setInputs(`FO-LowerBoundary` = -10)
    session$setInputs(`FO-UpperBoundary` = 10)
    session$setInputs(`FO-Seed` = 42)
    session$setInputs(`FO-create_formula` = 1)
    log_linear <<- session$userData$export[[1]]@summary
  })
  expect_true(abs(log_linear$slope_test - log_linear_expected[[2]]) < tol)
  expect_true(abs(log_linear$intercept_test - log_linear_expected[[1]]) < tol)
}
test_optim_log_linear_formula(app, srv)

test_optim_mm_formula <- function(app, srv) {
  tol <- 1e-3
  set.seed(1234)
  vmax <- 3
  km <- 0.01
  x <- seq(1, 10, length.out = 20)
  df <- data.frame(x = x, y = (vmax * x) / (x + km))
  mm <- NULL
  shiny::testServer(srv, {
    DataModelState$df <- df
    session$setInputs(`FO-model_type` = "Optimization Model")
    session$setInputs(`FO-PredefinedModels` = "michaelis_menten")
    session$setInputs(`FO-mm_lhs_var` = "y")
    session$setInputs(`FO-mm_x` = "x")
    session$setInputs(`FO-mm_vmax` = "vmax_test")
    session$setInputs(`FO-mm_km` = "km_test")
    session$setInputs(`FO-LowerBoundary` = -10)
    session$setInputs(`FO-UpperBoundary` = 10)
    session$setInputs(`FO-Seed` = 42)
    session$setInputs(`FO-create_formula` = 1)
    mm <<- session$userData$export[[1]]@summary
  })
  expect_true(abs(mm$km_test- km) < tol)
  expect_true(abs(mm$vmax_test-vmax) < tol)
}
test_optim_mm_formula(app, srv)

test_optim_one_site_formula <- function(app, srv) {
  tol <- 1e-3
  set.seed(1234)
  bmax <- 3
  kd <- 0.01
  x <- seq(1, 10, length.out = 20)
  df <- data.frame(x = x, y = (bmax * x) / (x + kd))
  one <- NULL
  shiny::testServer(srv, {
    DataModelState$df <- df
    session$setInputs(`FO-model_type` = "Optimization Model")
    session$setInputs(`FO-PredefinedModels` = "one_site_binding")
    session$setInputs(`FO-binding_lhs_var` = "y")
    session$setInputs(`FO-binding_x` = "x")
    session$setInputs(`FO-binding_bmax` = "bmax_test")
    session$setInputs(`FO-binding_kd` = "kd_test")
    session$setInputs(`FO-LowerBoundary` = -10)
    session$setInputs(`FO-UpperBoundary` = 10)
    session$setInputs(`FO-Seed` = 42)
    session$setInputs(`FO-create_formula` = 1)
    one <<- session$userData$export[[1]]@summary
  })
  expect_true(abs(one$kd_test- kd) < tol)
  expect_true(abs(one$bmax-bmax) < tol)
}
test_optim_one_site_formula(app, srv)

test_optim_two_hot_binding_formula <- function(app, srv) {
  tol <- 1e-3
  set.seed(1234)
  Bmax <- 30
  kon  <- 1e6
  koff <- 1e-3
  conc <- c(40, 50)
  time <- seq(0, 120, length.out = 100)
  df <- expand.grid(conc = conc, time = time)
  df$y <- (df$conc * 1e-9 / (df$conc * 1e-9 + koff / kon)) * Bmax *
    (1 - exp(-(kon * df$conc * 1e-9 + koff) * df$time))
  err <- NULL
  y_hat <- NULL
  shiny::testServer(srv, {
    DataModelState$df <- df
    session$setInputs(`FO-model_type` = "Optimization Model")
    session$setInputs(`FO-PredefinedModels` = "two_hot_binding")
    session$setInputs(`FO-hotbind_lhs_var` = "y")
    session$setInputs(`FO-hotbind_conc` = "conc")
    session$setInputs(`FO-hotbind_koff` = "koff_test")
    session$setInputs(`FO-hotbind_kon` = "kon_test")
    session$setInputs(`FO-hotbind_bmax` = "bmax_test")
    session$setInputs(`FO-hotbind_time` = "time")
    session$setInputs(`FO-LowerBoundary`= 0)
    session$setInputs(`FO-UpperBoundary` = 100)
    session$setInputs(`FO-Seed` = 42)
    session$setInputs(`FO-create_formula` = 1)

    res <- session$userData$export[[1]]
    res_summary <- res@summary
    koff_hat <- res_summary$koff_test
    kon_hat  <- res_summary$kon_test
    bmax_hat <- res_summary$bmax_test
    y_hat <<- (df$conc*1e-9 / (df$conc*1e-9 + koff_hat / kon_hat)) * bmax_hat *
      (1 - exp(-(kon_hat * df$conc*1e-9 + koff_hat) * df$time))
    err <<- mean(abs(df$y - y_hat))
  })
  df$y_hat <- y_hat
}
# FIX: two hot binding does not work really well with optim.
# I have to add also the option to use nls.
# Furthermore, I could also add the possibility to use a PSO
# test_optim_two_hot_binding_formula(app, srv)

test_linear_formula <- function(app, srv) {
  options(OpenStats.background = FALSE)
  expected <- list(
    broom::tidy(lm(uptake ~ conc + Treatment, data = CO2)),
    broom::tidy(lm(uptake ~ conc * Treatment, data = CO2)),
    broom::tidy(lm(uptake ~ Plant - Treatment, data = CO2))
  )
  ex <- list()
  shiny::testServer(srv, {
    DataModelState$df <- CO2

    session$setInputs(`FO-model_type` = "Linear")
    session$setInputs(`FO-colnames-dropdown_` = "uptake");
    session$setInputs(`FO-colnames_conc_` = 1); sync_code(session)
    session$setInputs(`FO-add` = 1); sync_code(session)
    session$setInputs(`FO-colnames_Treatment_` = 1); sync_code(session)
    session$setInputs(`FO-create_formula` = 1)
    ex[[1]] <<- session$userData$export[[1]]@summary
    session$userData$export_formula_rhs<- ""; sync_code(session)

    session$setInputs(`FO-colnames-dropdown_` = "uptake");
    session$setInputs(`FO-colnames_conc_` = 1); sync_code(session)
    session$setInputs(`FO-mul` = 1); sync_code(session)
    session$setInputs(`FO-colnames_Treatment_` = 1); sync_code(session)
    session$setInputs(`FO-create_formula` = 1)
    ex[[2]] <<- session$userData$export[[2]]@summary
    session$userData$export_formula_rhs<- ""; sync_code(session)

    session$setInputs(`FO-colnames-dropdown_` = "uptake");
    session$setInputs(`FO-colnames_Plant_` = 1); sync_code(session)
    session$setInputs(`FO-minus` = 1); sync_code(session)
    session$setInputs(`FO-colnames_Treatment_` = 1); sync_code(session)
    session$setInputs(`FO-create_formula` = 1)
    ex[[3]] <<- session$userData$export[[3]]@summary
    session$userData$export_formula_rhs<- ""; sync_code(session)

  })
   expect_equal(ex, expected)
}
test_linear_formula(app, srv)

test_glm_formula <- function(app, srv) {
  options(OpenStats.background = FALSE)
  expected <- list(
    broom::tidy(glm(uptake ~ conc + Treatment, data = CO2, family = "Gamma")),
    broom::tidy(glm(uptake ~ conc + Treatment, data = CO2, family = stats::gaussian()))
  )
  ex <- list()
  shiny::testServer(srv, {
    DataModelState$df <- CO2

    session$setInputs(`FO-model_type` = "Generalised Linear Model")
    session$setInputs(`FO-Family` = "Gamma")
    session$setInputs(`FO-Link_function` = "inverse")
    session$setInputs(`FO-colnames-dropdown_` = "uptake");
    session$setInputs(`FO-colnames_conc_` = 1); sync_code(session)
    session$setInputs(`FO-add` = 1); sync_code(session)
    session$setInputs(`FO-colnames_Treatment_` = 1); sync_code(session)
    session$setInputs(`FO-create_formula` = 1)
    ex[[1]] <<- session$userData$export[[1]]@summary
    session$userData$export_formula_rhs<- ""; sync_code(session)

    session$setInputs(`FO-model_type` = "Generalised Linear Model")
    session$setInputs(`FO-Family` = stats::gaussian())
    session$setInputs(`FO-Link_function` = "identity")
    session$setInputs(`FO-colnames-dropdown_` = "uptake");
    session$setInputs(`FO-colnames_conc_` = 1); sync_code(session)
    session$setInputs(`FO-add` = 1); sync_code(session)
    session$setInputs(`FO-colnames_Treatment_` = 1); sync_code(session)
    session$setInputs(`FO-create_formula` = 1)
    ex[[2]] <<- session$userData$export[[2]]@summary
    session$userData$export_formula_rhs<- ""; sync_code(session)
  })
   expect_equal(ex, expected)
}
test_glm_formula(app, srv)
