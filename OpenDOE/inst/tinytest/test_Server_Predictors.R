if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

app <- OpenDOE:::app()
srv <- app$server

test_add_update_remove_predictor <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B,A")
    session$setInputs(`PREDICTORS-add_predictor` = 1)
    checks[1] <<- length(State$predictors) == 0L # duplicate level rejected

    session$setInputs(`PREDICTORS-predictor_levels` = "A,B,C")
    session$setInputs(`PREDICTORS-add_predictor` = 2)
    checks[2] <<- identical(State$predictors$treatment, c("A", "B", "C"))
    checks[3] <<- State$predictor_types[["treatment"]] == "categorical"

    session$setInputs(`PREDICTORS-update_predictor_select` = "treatment")
    session$setInputs(`PREDICTORS-update_predictor_levels` = "1,2,3")
    session$setInputs(`PREDICTORS-update_predictor` = 1)
    checks[4] <<- identical(State$predictors$treatment, c("1", "2", "3"))
    checks[5] <<- State$predictor_types[["treatment"]] == "discrete"

    session$setInputs(`PREDICTORS-remove_predictor_select` = "treatment")
    session$setInputs(`PREDICTORS-remove_predictor` = 1)
    checks[6] <<- length(State$predictors) == 0L
  })
  expect_true(all(checks))
}
test_add_update_remove_predictor(srv)

test_continuous_grid_predictor <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-predictor_name` = "temp")
    session$setInputs(`PREDICTORS-predictor_type` = "continuous_grid")
    session$setInputs(`PREDICTORS-predictor_levels` = "20,60")
    session$setInputs(`PREDICTORS-add_predictor` = 1)
    checks[1] <<- length(State$predictors) == 0L # step missing rejected

    session$setInputs(`PREDICTORS-predictor_levels` = "60,20,10")
    session$setInputs(`PREDICTORS-add_predictor` = 2)
    checks[2] <<- length(State$predictors) == 0L # start >= end rejected

    session$setInputs(`PREDICTORS-predictor_levels` = "20,60,15")
    session$setInputs(`PREDICTORS-add_predictor` = 3)
    checks[3] <<- length(State$predictors) == 0L # end not on grid rejected

    session$setInputs(`PREDICTORS-predictor_levels` = "20,60,10")
    session$setInputs(`PREDICTORS-add_predictor` = 4)
    checks[4] <<- identical(State$predictors$temp, c("20", "30", "40", "50", "60")) &&
      State$predictor_types[["temp"]] == "continuous_grid"

    session$setInputs(`PREDICTORS-predictor_name` = "solvent")
    session$setInputs(`PREDICTORS-predictor_type` = "discrete")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B")
    session$setInputs(`PREDICTORS-add_predictor` = 5)
    checks[5] <<- !("solvent" %in% names(State$predictors)) # non-numeric discrete rejected

    session$setInputs(`PREDICTORS-remove_predictor_select` = "temp")
    session$setInputs(`PREDICTORS-remove_predictor` = 1)
    checks[6] <<- length(State$predictor_types) == 0L
  })
  expect_true(all(checks))
}
test_continuous_grid_predictor(srv)

test_continuous_predictor <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-predictor_name` = "temp")
    session$setInputs(`PREDICTORS-predictor_type` = "continuous")
    session$setInputs(`PREDICTORS-predictor_levels` = "20,40,60")
    session$setInputs(`PREDICTORS-add_predictor` = 1)
    checks[1] <<- length(State$predictors) == 0L # three values rejected

    session$setInputs(`PREDICTORS-predictor_levels` = "60,20")
    session$setInputs(`PREDICTORS-add_predictor` = 2)
    checks[2] <<- length(State$predictors) == 0L # min >= max rejected

    session$setInputs(`PREDICTORS-predictor_levels` = "20,60")
    session$setInputs(`PREDICTORS-add_predictor` = 3)
    checks[3] <<- identical(State$predictors$temp, c("20", "60"))
    checks[4] <<- State$predictor_types[["temp"]] == "continuous"
    checks[5] <<- OpenDOE:::step_unlocked(State, "Design of experiment") # LHS handles continuous
    checks[6] <<- !OpenDOE:::step_unlocked(State, "Sample size")
  })
  expect_true(all(checks))
}
test_continuous_predictor(srv)
