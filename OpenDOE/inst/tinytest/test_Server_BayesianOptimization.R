if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
for (p in c("mlr3mbo", "mlr3learners", "DiceKriging", "rgenoud", "ranger")) {
  if (!requireNamespace(p, quietly = TRUE)) exit_file(paste("needs", p))
}
library(tinytest)

upload_csv <- function(session, df) {
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(df, tmp, row.names = FALSE)
  session$setInputs(`DATA-upload` = data.frame(
    name = "data.csv", size = file.info(tmp)$size,
    type = "text/csv", datapath = tmp
  ))
  unlink(tmp)
}

test_predictors_from_data <- function() {
  data <- data.frame(temp = c(20, 35, 50), solvent = c("B", "A", "B"), ph = c(7, 5, 7))
  conv <- OpenDOE:::predictors_from_data(
    data, c(temp = "continuous", solvent = "categorical", ph = "discrete")
  )
  expect_equal(conv$predictors$temp, c("20", "50"))
  expect_equal(conv$predictors$solvent, c("A", "B"))
  expect_equal(conv$predictors$ph, c("5", "7"))
  expect_error(OpenDOE:::predictors_from_data(data, c(solvent = "continuous")), "numeric")
  expect_error(OpenDOE:::predictors_from_data(data.frame(x = c(1, 1)), c(x = "continuous")), "distinct")
  expect_error(OpenDOE:::predictors_from_data(data, c(missing = "continuous")), "Missing columns")
}
test_predictors_from_data()

test_bo_sync_flow <- function() {
  old <- getOption("OpenDOE.background")
  options(OpenDOE.background = FALSE)
  on.exit(options(OpenDOE.background = old))

  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    measured <- data.frame(temp = c(20, 35, 50, 60), yield = c(0.2, 0.6, 0.7, 0.4), note = "lab")
    upload_csv(session, measured)
    checks[1] <<- identical(names(State$results), "1")

    session$setInputs(`BO-df_source` = "1")
    session$setInputs(`BO-predictor_cols` = "temp")
    session$setInputs(`BO-objective` = "note")
    session$setInputs(`BO-direction` = "maximize")
    session$setInputs(`BO-seed` = 42)
    session$setInputs(`BO-suggest` = 1)
    checks[2] <<- length(State$results) == 1L # non-numeric objective rejected

    session$setInputs(`BO-objective` = "yield")
    session$setInputs(`BO-suggest` = 2)
    checks[3] <<- identical(names(State$results), c("1", "2"))
    checks[4] <<- inherits(State$results[["2"]], "boSuggestionResult")
    df <- State$results[["2"]]@df
    checks[5] <<- nrow(df) == 5L
    checks[6] <<- is.na(df$yield[5L]) && is.na(df$note[5L])
    checks[7] <<- df$temp[5L] >= 20 && df$temp[5L] <= 60 && df$temp[5L] == signif(df$temp[5L], 3)
    checks[8] <<- !isTRUE(State$bo_running)
    checks[9] <<- State$history[["2"]]$type == "bo_suggest"
    checks[10] <<- State$history[["2"]]$params$predictor_types[["temp"]] == "continuous"

    session$setInputs(`BO-n_points` = 3)
    session$setInputs(`BO-suggest` = 3)
    df3 <- State$results[["3"]]@df
    checks[11] <<- nrow(df3) == 7L && all(is.na(df3$yield[5:7]))
    checks[12] <<- State$history[["3"]]$params$n_points == 3L
  })
  expect_true(all(checks))
}
test_bo_sync_flow()

test_bo_objective_as_predictor_rejected <- function() {
  old <- getOption("OpenDOE.background")
  options(OpenDOE.background = FALSE)
  on.exit(options(OpenDOE.background = old))

  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    upload_csv(session, data.frame(temp = c(20, 35, 50), yield = c(0.2, 0.6, 0.7)))
    session$setInputs(`BO-df_source` = "1")
    session$setInputs(`BO-predictor_cols` = c("temp", "yield"))
    session$setInputs(`BO-objective` = "yield")
    session$setInputs(`BO-direction` = "maximize")
    session$setInputs(`BO-seed` = 42)
    session$setInputs(`BO-suggest` = 1)
    checks[1] <<- length(State$results) == 1L
  })
  expect_true(all(checks))
}
test_bo_objective_as_predictor_rejected()

test_bo_input_checks <- function() {
  old <- getOption("OpenDOE.background")
  options(OpenDOE.background = FALSE)
  on.exit(options(OpenDOE.background = old))

  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    upload_csv(session, data.frame(conc = c(1, 100)))
    session$setInputs(`BO-df_source` = "1")
    session$setInputs(`BO-predictor_cols` = "conc")
    session$setInputs(`BO-objective` = "conc")
    session$setInputs(`BO-direction` = "maximize")
    session$setInputs(`BO-seed` = 42)
    session$setInputs(`BO-suggest` = 1)
    checks[1] <<- length(State$results) == 1L # single column rejected

    upload_csv(session, data.frame(conc = c(1, 100), y = c(0.1, 3.4)))
    session$setInputs(`BO-df_source` = "2")
    session$setInputs(`BO-objective` = "y")
    session$setInputs(`BO-suggest` = 2)
    checks[2] <<- length(State$results) == 2L # two measured rows rejected
  })
  expect_true(all(checks))
}
test_bo_input_checks()
