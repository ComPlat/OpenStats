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
    checks[3] <<- State$predictor_types[["treatment"]] == "character"

    session$setInputs(`PREDICTORS-update_predictor_select` = "treatment")
    session$setInputs(`PREDICTORS-update_predictor_levels` = "1,2,3")
    session$setInputs(`PREDICTORS-update_predictor` = 1)
    checks[4] <<- identical(State$predictors$treatment, c("1", "2", "3"))
    checks[5] <<- State$predictor_types[["treatment"]] == "numeric"

    session$setInputs(`PREDICTORS-remove_predictor_select` = "treatment")
    session$setInputs(`PREDICTORS-remove_predictor` = 1)
    checks[6] <<- length(State$predictors) == 0L
  })
  expect_true(all(checks))
}
test_add_update_remove_predictor(srv)

test_add_predictor_table_to_results <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-add_to_results` = 1)
    checks[1] <<- length(State$results) == 0L

    session$setInputs(`PREDICTORS-predictor_name` = "dose")
    session$setInputs(`PREDICTORS-predictor_levels` = "low,high")
    session$setInputs(`PREDICTORS-add_predictor` = 1)

    session$setInputs(`PREDICTORS-add_to_results` = 2)
    checks[2] <<- identical(names(State$results), "1")
    checks[3] <<- inherits(State$results[["1"]], "predictorTable")
    checks[4] <<- attr(State$results[["1"]], "label") == "1 Predictor table"
    checks[5] <<- identical(names(State$history), "1")
    checks[6] <<- State$history[["1"]]$type == "predictor_table"
    checks[7] <<- identical(State$history[["1"]]$params$predictors, State$predictors)
  })
  expect_true(all(checks))
}
test_add_predictor_table_to_results(srv)
