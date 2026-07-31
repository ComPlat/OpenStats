if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
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

build_design_inputs <- function(session) {
  session$setInputs(`PREDICTORS-predictor_name` = "treatment")
  session$setInputs(`PREDICTORS-predictor_levels` = "A,B")
  session$setInputs(`PREDICTORS-add_predictor` = 1)
  session$setInputs(`DESIGN-n_source` = "free")
  session$setInputs(`DESIGN-custom_n` = 5)
  session$setInputs(`DESIGN-build` = 1)
}

test_finite_assign_sync_flow <- function() {
  old <- getOption("OpenDOE.background")
  options(OpenDOE.background = FALSE)
  on.exit(options(OpenDOE.background = old))

  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    build_design_inputs(session)
    checks[1] <<- identical(names(State$results), "1")

    upload_csv(session, data.frame(animal_id = 1:10, weight = c(20, 21, 22, 23, 24, 25, 26, 27, 28, 29)))
    checks[2] <<- identical(names(State$results), c("1", "2"))
    checks[3] <<- inherits(State$results[["2"]], "importedData")

    session$setInputs(`FINITEASSIGN-groups_source` = "2")
    session$setInputs(`FINITEASSIGN-id_col` = "animal_id")
    session$setInputs(`FINITEASSIGN-design_source` = "1")
    session$setInputs(`FINITEASSIGN-loss_function` = "Default")
    session$setInputs(`FINITEASSIGN-max_iter` = 5)
    session$setInputs(`FINITEASSIGN-seed` = 42)
    session$setInputs(`FINITEASSIGN-assign` = 1)

    checks[4] <<- identical(names(State$results), c("1", "2", "3"))
    checks[5] <<- inherits(State$results[["3"]], "finiteAssignmentResult")
    df <- State$results[["3"]]@df
    checks[6] <<- nrow(df) == 10L
    checks[7] <<- "treatment" %in% names(df)
    checks[8] <<- is.numeric(State$results[["3"]]@loss)
    checks[9] <<- !isTRUE(State$finite_assign_running)
    checks[10] <<- State$history[["3"]]$type == "random_finite_assign"
    checks[11] <<- State$history[["3"]]$params$design_id == "1"
    checks[12] <<- State$history[["3"]]$params$groups_id == "2"
    checks[13] <<- State$history[["3"]]$params$id_col == "animal_id"
    checks[14] <<- "unit_id" %in% names(df)
    checks[15] <<- !("unit_index" %in% names(df))
  })
  expect_true(all(checks))
}
test_finite_assign_sync_flow()

test_finite_assign_design_source_excludes_uploaded_csv <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    # Uploading a CSV must not offer it as a design source -- design_source
    # is deliberately restricted to designResult/assignmentResult (no
    # "upload a CSV as the design" escape hatch).
    upload_csv(session, data.frame(treatment = rep(c("A", "B"), each = 5)))
    checks[1] <<- identical(names(State$results), "1")
    checks[2] <<- length(OpenDOE:::df_result_choices(State, classes = "designResult")) == 0L

    build_design_inputs(session)
    checks[3] <<- identical(names(State$results), c("1", "2"))
    choices <- OpenDOE:::df_result_choices(State, classes = "designResult")
    checks[4] <<- identical(unname(choices), "2")
  })
  expect_true(all(checks))
}
test_finite_assign_design_source_excludes_uploaded_csv()

test_finite_assign_accepts_random_assignment_result_as_design <- function() {
  old <- getOption("OpenDOE.background")
  options(OpenDOE.background = FALSE)
  on.exit(options(OpenDOE.background = old))

  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    build_design_inputs(session)
    checks[1] <<- identical(names(State$results), "1")

    # A Random-assignment result (design rows + one extra assigned column,
    # here "day") is still design-shaped -- must be selectable as Finite
    # assignment's design source too, not just the raw designResult.
    session$setInputs(`RANDOMASSIGN-df_source` = "1")
    session$setInputs(`RANDOMASSIGN-groups` = "day1,day2")
    session$setInputs(`RANDOMASSIGN-ratios` = "1,1")
    session$setInputs(`RANDOMASSIGN-col` = "day")
    session$setInputs(`RANDOMASSIGN-seed` = 42)
    session$setInputs(`RANDOMASSIGN-assign` = 1)
    checks[2] <<- identical(names(State$results), c("1", "2"))
    checks[3] <<- inherits(State$results[["2"]], "assignmentResult")

    choices <- OpenDOE:::df_result_choices(State, classes = c("designResult", "assignmentResult"))
    checks[4] <<- setequal(unname(choices), c("1", "2"))

    upload_csv(session, data.frame(animal_id = 1:10, weight = c(20, 21, 22, 23, 24, 25, 26, 27, 28, 29)))
    checks[5] <<- identical(names(State$results), c("1", "2", "3"))

    session$setInputs(`FINITEASSIGN-groups_source` = "3")
    session$setInputs(`FINITEASSIGN-id_col` = "animal_id")
    session$setInputs(`FINITEASSIGN-design_source` = "2")
    session$setInputs(`FINITEASSIGN-loss_function` = "Default")
    session$setInputs(`FINITEASSIGN-max_iter` = 5)
    session$setInputs(`FINITEASSIGN-seed` = 42)
    session$setInputs(`FINITEASSIGN-assign` = 1)

    checks[6] <<- identical(names(State$results), c("1", "2", "3", "4"))
    checks[7] <<- inherits(State$results[["4"]], "finiteAssignmentResult")
    df <- State$results[["4"]]@df
    checks[8] <<- nrow(df) == 10L
    checks[9] <<- "day" %in% names(df)
    checks[10] <<- "unit_id" %in% names(df)
  })
  expect_true(all(checks))
}
test_finite_assign_accepts_random_assignment_result_as_design()

test_finite_assign_rejects_non_numeric_columns <- function() {
  old <- getOption("OpenDOE.background")
  options(OpenDOE.background = FALSE)
  on.exit(options(OpenDOE.background = old))

  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    build_design_inputs(session)
    # "id" here is the (legitimate) ID column, excluded from the numeric
    # check; "name" is a genuinely non-numeric covariate column, which must
    # still be rejected.
    upload_csv(session, data.frame(id = 1:5, weight = 20:24, name = letters[1:5]))
    session$setInputs(`FINITEASSIGN-groups_source` = "2")
    session$setInputs(`FINITEASSIGN-id_col` = "id")
    session$setInputs(`FINITEASSIGN-design_source` = "1")
    session$setInputs(`FINITEASSIGN-assign` = 1)
    checks[1] <<- identical(names(State$results), c("1", "2"))
  })
  expect_true(all(checks))
}
test_finite_assign_rejects_non_numeric_columns()

test_finite_assign_requires_id_col <- function() {
  old <- getOption("OpenDOE.background")
  options(OpenDOE.background = FALSE)
  on.exit(options(OpenDOE.background = old))

  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    build_design_inputs(session)
    upload_csv(session, data.frame(id = 1:10, weight = c(20, 21, 22, 23, 24, 25, 26, 27, 28, 29)))
    session$setInputs(`FINITEASSIGN-groups_source` = "2")
    # deliberately not setting id_col
    session$setInputs(`FINITEASSIGN-design_source` = "1")
    session$setInputs(`FINITEASSIGN-assign` = 1)
    checks[1] <<- identical(names(State$results), c("1", "2"))
  })
  expect_true(all(checks))
}
test_finite_assign_requires_id_col()

test_finite_assign_requires_groups_selection <- function() {
  old <- getOption("OpenDOE.background")
  options(OpenDOE.background = FALSE)
  on.exit(options(OpenDOE.background = old))

  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    build_design_inputs(session)
    session$setInputs(`FINITEASSIGN-design_source` = "1")
    session$setInputs(`FINITEASSIGN-assign` = 1)
    checks[1] <<- identical(names(State$results), "1")
  })
  expect_true(all(checks))
}
test_finite_assign_requires_groups_selection()

test_finite_assign_rejects_too_few_covariate_rows <- function() {
  old <- getOption("OpenDOE.background")
  options(OpenDOE.background = FALSE)
  on.exit(options(OpenDOE.background = old))

  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    build_design_inputs(session)
    upload_csv(session, data.frame(id = 1:3, weight = c(20, 21, 22)))
    session$setInputs(`FINITEASSIGN-groups_source` = "2")
    session$setInputs(`FINITEASSIGN-id_col` = "id")
    session$setInputs(`FINITEASSIGN-design_source` = "1")
    session$setInputs(`FINITEASSIGN-assign` = 1)
    checks[1] <<- identical(names(State$results), c("1", "2"))
  })
  expect_true(all(checks))
}
test_finite_assign_rejects_too_few_covariate_rows()

test_finite_assign_cancel_button <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    build_design_inputs(session)
    upload_csv(session, data.frame(id = 1:10, weight = c(20, 21, 22, 23, 24, 25, 26, 27, 28, 29)))
    session$setInputs(`FINITEASSIGN-groups_source` = "2")
    session$setInputs(`FINITEASSIGN-id_col` = "id")
    session$setInputs(`FINITEASSIGN-design_source` = "1")
    session$setInputs(`FINITEASSIGN-max_iter` = 5)
    session$setInputs(`FINITEASSIGN-seed` = 42)
    session$setInputs(`FINITEASSIGN-assign` = 1)

    checks[1] <<- isTRUE(State$finite_assign_running)
    checks[2] <<- State$bgp$running

    session$setInputs(`FINITEASSIGN-cancel` = 1)

    checks[3] <<- !isTRUE(State$finite_assign_running)
    checks[4] <<- !State$bgp$running
    checks[5] <<- identical(names(State$results), c("1", "2")) # no finite-assign result was added
  })
  expect_true(all(checks))
}
test_finite_assign_cancel_button()
