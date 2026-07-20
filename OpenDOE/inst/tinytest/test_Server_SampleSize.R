if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

options(OpenDOE.background = FALSE)

app <- OpenDOE:::app()
srv <- app$server

test_ttest_flow <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B")
    session$setInputs(`PREDICTORS-add_predictor` = 1)
    session$setInputs(`PREDICTORS-add_to_results` = 1)

    session$setInputs(`SAMPLESIZE-primary_factor` = "treatment")
    session$setInputs(`SAMPLESIZE-cohens_d` = 0.8)
    session$setInputs(`SAMPLESIZE-sig_level_ttest` = 0.05)
    session$setInputs(`SAMPLESIZE-desired_power_ttest` = 0.8)
    session$setInputs(`SAMPLESIZE-calc_ttest` = 1)

    checks[1] <<- identical(names(State$results), c("1", "2"))
    checks[2] <<- inherits(State$results[["1"]], "predictorTable")
    checks[3] <<- inherits(State$results[["2"]], "sampleSizeResult")
    checks[4] <<- State$results[["2"]]@n == State$n_per_level
    checks[5] <<- attr(State$results[["2"]], "label") == "2 Power analysis: t-test"
    checks[6] <<- identical(names(State$history), c("1", "2"))
    checks[7] <<- State$history[["2"]]$type == "ttest"
    checks[8] <<- State$history[["2"]]$params$cohens_d == 0.8
  })
  expect_true(all(checks))
}
test_ttest_flow(srv)

test_anova_flow <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B,C")
    session$setInputs(`PREDICTORS-add_predictor` = 1)

    session$setInputs(`SAMPLESIZE-primary_factor` = "treatment")
    session$setInputs(`SAMPLESIZE-cohens_f` = 0.25)
    session$setInputs(`SAMPLESIZE-sig_level_anova` = 0.05)
    session$setInputs(`SAMPLESIZE-desired_power_anova` = 0.8)
    session$setInputs(`SAMPLESIZE-calc_anova` = 1)

    checks[1] <<- identical(names(State$results), "1")
    checks[2] <<- inherits(State$results[["1"]], "sampleSizeResult")
    checks[3] <<- State$history[["1"]]$type == "anova"
  })
  expect_true(all(checks))
}
test_anova_flow(srv)

test_mc_flow <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B,C")
    session$setInputs(`PREDICTORS-add_predictor` = 1)

    session$setInputs(`SAMPLESIZE-n_groups` = 3)
    session$setInputs(`SAMPLESIZE-group_name_1` = "A", `SAMPLESIZE-group_mean_1` = 0, `SAMPLESIZE-group_sd_1` = 1)
    session$setInputs(`SAMPLESIZE-group_name_2` = "B", `SAMPLESIZE-group_mean_2` = 1, `SAMPLESIZE-group_sd_2` = 1)
    session$setInputs(`SAMPLESIZE-group_name_3` = "C", `SAMPLESIZE-group_mean_3` = 2, `SAMPLESIZE-group_sd_3` = 1)
    session$setInputs(`SAMPLESIZE-mcc` = "holm", `SAMPLESIZE-family` = "any")
    session$setInputs(`SAMPLESIZE-alpha` = 0.05, `SAMPLESIZE-power_target_mc` = 0.8)
    session$setInputs(`SAMPLESIZE-nsim` = 200, `SAMPLESIZE-n_min` = 2, `SAMPLESIZE-n_max` = 50)
    session$setInputs(`SAMPLESIZE-seed` = 42)
    session$setInputs(`SAMPLESIZE-calc_mc` = 1)

    checks[1] <<- identical(names(State$results), "1")
    checks[2] <<- inherits(State$results[["1"]], "sampleSizeResult")
    checks[3] <<- State$history[["1"]]$type == "mc"
    checks[4] <<- !isTRUE(State$mc_running)
  })
  expect_true(all(checks))
}
test_mc_flow(srv)
