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

test_simple_assign <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    upload_csv(session, data.frame(id = 1:10, age = 20:29))
    checks[1] <<- identical(names(State$results), "1")

    session$setInputs(`RANDOMASSIGN-df_source` = "1")
    session$setInputs(`RANDOMASSIGN-groups` = "A,B")
    session$setInputs(`RANDOMASSIGN-ratios` = "1,1")
    session$setInputs(`RANDOMASSIGN-col` = "arm")
    session$setInputs(`RANDOMASSIGN-seed` = 42)
    session$setInputs(`RANDOMASSIGN-assign` = 1)

    checks[2] <<- identical(names(State$results), c("1", "2"))
    checks[3] <<- inherits(State$results[["2"]], "assignmentResult")
    assigned <- State$results[["2"]]@df
    checks[4] <<- nrow(assigned) == 10L
    checks[5] <<- "arm" %in% names(assigned)
    checks[6] <<- setequal(unique(assigned$arm), c("A", "B"))
    checks[7] <<- State$history[["2"]]$type == "random_assign"
    checks[8] <<- State$history[["2"]]$params$col == "arm"
    checks[9] <<- State$history[["2"]]$params$df_source == "1"
  })
  expect_true(all(checks))
}
test_simple_assign()

test_assign_rejects_mismatched_groups_and_ratios <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    upload_csv(session, data.frame(id = 1:5))
    session$setInputs(`RANDOMASSIGN-df_source` = "1")
    session$setInputs(`RANDOMASSIGN-groups` = "A,B,C")
    session$setInputs(`RANDOMASSIGN-ratios` = "1,1")
    session$setInputs(`RANDOMASSIGN-col` = "arm")
    session$setInputs(`RANDOMASSIGN-seed` = 42)
    session$setInputs(`RANDOMASSIGN-assign` = 1)

    checks[1] <<- identical(names(State$results), "1")
  })
  expect_true(all(checks))
}
test_assign_rejects_mismatched_groups_and_ratios()

test_assign_rejects_existing_column_name <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    upload_csv(session, data.frame(id = 1:5, arm = "x"))
    session$setInputs(`RANDOMASSIGN-df_source` = "1")
    session$setInputs(`RANDOMASSIGN-groups` = "A,B")
    session$setInputs(`RANDOMASSIGN-ratios` = "1,1")
    session$setInputs(`RANDOMASSIGN-col` = "arm")
    session$setInputs(`RANDOMASSIGN-seed` = 42)
    session$setInputs(`RANDOMASSIGN-assign` = 1)

    checks[1] <<- identical(names(State$results), "1")
  })
  expect_true(all(checks))
}
test_assign_rejects_existing_column_name()

test_block_assign <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    upload_csv(session, data.frame(id = 1:20, sex = rep(c("f", "m"), each = 10)))
    session$setInputs(`RANDOMASSIGN-df_source` = "1")
    session$setInputs(`RANDOMASSIGN-randomization_method` = "block")
    session$setInputs(`RANDOMASSIGN-groups` = "A,B")
    session$setInputs(`RANDOMASSIGN-ratios` = "1,1")
    session$setInputs(`RANDOMASSIGN-col` = "arm")
    session$setInputs(`RANDOMASSIGN-block_col` = "sex")
    session$setInputs(`RANDOMASSIGN-seed` = 42)
    session$setInputs(`RANDOMASSIGN-assign` = 1)

    checks[1] <<- identical(names(State$results), c("1", "2"))
    assigned <- State$results[["2"]]@df
    checks[2] <<- nrow(assigned) == 20L
    checks[3] <<- all(table(assigned$sex, assigned$arm) == 5L)
    checks[4] <<- State$history[["2"]]$params$randomization_method == "block"
    checks[5] <<- State$history[["2"]]$params$block_col == "sex"
  })
  expect_true(all(checks))
}
test_block_assign()

test_block_assign_rejects_high_cardinality_block_col <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    upload_csv(session, data.frame(
      id = 1:10,
      weight = c(20.1, 20.3, 20.5, 20.7, 20.9, 21.1, 21.3, 21.5, 21.7, 21.9)
    ))
    session$setInputs(`RANDOMASSIGN-df_source` = "1")
    session$setInputs(`RANDOMASSIGN-randomization_method` = "block")
    session$setInputs(`RANDOMASSIGN-groups` = "A,B")
    session$setInputs(`RANDOMASSIGN-ratios` = "1,1")
    session$setInputs(`RANDOMASSIGN-col` = "arm")
    session$setInputs(`RANDOMASSIGN-block_col` = "weight")
    session$setInputs(`RANDOMASSIGN-seed` = 42)
    session$setInputs(`RANDOMASSIGN-assign` = 1)

    checks[1] <<- identical(names(State$results), "1")
  })
  expect_true(all(checks))
}
test_block_assign_rejects_high_cardinality_block_col()

test_block_assign_requires_block_col <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    upload_csv(session, data.frame(id = 1:10, sex = rep(c("f", "m"), 5)))
    session$setInputs(`RANDOMASSIGN-df_source` = "1")
    session$setInputs(`RANDOMASSIGN-randomization_method` = "block")
    session$setInputs(`RANDOMASSIGN-groups` = "A,B")
    session$setInputs(`RANDOMASSIGN-ratios` = "1,1")
    session$setInputs(`RANDOMASSIGN-col` = "arm")
    session$setInputs(`RANDOMASSIGN-seed` = 42)
    session$setInputs(`RANDOMASSIGN-assign` = 1)

    checks[1] <<- identical(names(State$results), "1")
  })
  expect_true(all(checks))
}
test_block_assign_requires_block_col()

test_block_stratified_assign <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    upload_csv(session, data.frame(
      id = 1:20,
      sex = rep(c("f", "m"), each = 10),
      site = rep(c("A", "B"), times = 10)
    ))
    session$setInputs(`RANDOMASSIGN-df_source` = "1")
    session$setInputs(`RANDOMASSIGN-randomization_method` = "block_stratified")
    session$setInputs(`RANDOMASSIGN-groups` = "A,B")
    session$setInputs(`RANDOMASSIGN-ratios` = "1,1")
    session$setInputs(`RANDOMASSIGN-col` = "arm")
    session$setInputs(`RANDOMASSIGN-strata_cols` = c("sex", "site"))
    session$setInputs(`RANDOMASSIGN-seed` = 42)
    session$setInputs(`RANDOMASSIGN-assign` = 1)

    checks[1] <<- identical(names(State$results), c("1", "2"))
    assigned <- State$results[["2"]]@df
    checks[2] <<- nrow(assigned) == 20L
    checks[3] <<- !("STRATA_INTERACTION" %in% names(assigned))
    checks[4] <<- State$history[["2"]]$params$randomization_method == "block_stratified"
    checks[5] <<- identical(State$history[["2"]]$params$strata_cols, c("sex", "site"))
  })
  expect_true(all(checks))
}
test_block_stratified_assign()

test_block_stratified_assign_requires_strata_cols <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    upload_csv(session, data.frame(id = 1:10, sex = rep(c("f", "m"), 5)))
    session$setInputs(`RANDOMASSIGN-df_source` = "1")
    session$setInputs(`RANDOMASSIGN-randomization_method` = "block_stratified")
    session$setInputs(`RANDOMASSIGN-groups` = "A,B")
    session$setInputs(`RANDOMASSIGN-ratios` = "1,1")
    session$setInputs(`RANDOMASSIGN-col` = "arm")
    # no strata_cols selected
    session$setInputs(`RANDOMASSIGN-seed` = 42)
    session$setInputs(`RANDOMASSIGN-assign` = 1)

    checks[1] <<- identical(names(State$results), "1")
  })
  expect_true(all(checks))
}
test_block_stratified_assign_requires_strata_cols()
