if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)
options(OpenDOE.background = FALSE)

test_design_from_computed_sample_size <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)
  shiny::testServer(app$server, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B")
    session$setInputs(`PREDICTORS-add_predictor` = 1)

    session$setInputs(`SAMPLESIZE-primary_factor` = "treatment")
    session$setInputs(`SAMPLESIZE-cohens_d` = 0.8)
    session$setInputs(`SAMPLESIZE-sig_level_ttest` = 0.05)
    session$setInputs(`SAMPLESIZE-desired_power_ttest` = 0.8)
    session$setInputs(`SAMPLESIZE-calc_ttest` = 1)
    n_computed <- State$results[["1"]]@n

    session$setInputs(`DESIGN-n_source` = "1")
    session$setInputs(`DESIGN-build` = 1)

    checks[1] <<- identical(names(State$results), c("1", "2"))
    checks[2] <<- inherits(State$results[["2"]], "designResult")
    df <- State$results[["2"]]@df
    checks[3] <<- nrow(df) == n_computed * 2L # 2 predictor levels x n_computed
    checks[4] <<- "treatment" %in% names(df)
    checks[5] <<- State$history[["2"]]$type == "design"
    checks[6] <<- State$history[["2"]]$params$n_per_level == n_computed
  })
  expect_true(all(checks))
}
test_design_from_computed_sample_size()

test_design_from_free_custom_n <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    session$setInputs(`PREDICTORS-predictor_name` = "dose")
    session$setInputs(`PREDICTORS-predictor_levels` = "low,high")
    session$setInputs(`PREDICTORS-add_predictor` = 1)

    session$setInputs(`DESIGN-n_source` = "free")
    session$setInputs(`DESIGN-custom_n` = 5)
    session$setInputs(`DESIGN-build` = 1)

    checks[1] <<- identical(names(State$results), "1")
    df <- State$results[["1"]]@df
    checks[2] <<- nrow(df) == 10L # 2 predictor levels x 5
    checks[3] <<- State$history[["1"]]$params$n_per_level == 5L
  })
  expect_true(all(checks))
}
test_design_from_free_custom_n()

test_design_requires_predictors <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    session$setInputs(`DESIGN-n_source` = "free")
    session$setInputs(`DESIGN-custom_n` = 5)
    session$setInputs(`DESIGN-build` = 1)
    checks[1] <<- length(State$results) == 0L
  })
  expect_true(all(checks))
}
test_design_requires_predictors()

test_design_free_rejects_invalid_n <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B")
    session$setInputs(`PREDICTORS-add_predictor` = 1)

    session$setInputs(`DESIGN-n_source` = "free")
    session$setInputs(`DESIGN-custom_n` = 0)
    session$setInputs(`DESIGN-build` = 1)
    checks[1] <<- length(State$results) == 0L
  })
  expect_true(all(checks))
}
test_design_free_rejects_invalid_n()
