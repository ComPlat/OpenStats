if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)
options(OpenDOE.background = FALSE)

# Random assignment is now only ever fed the design matrix generated in the
# "Design of experiment" tab (no upload here anymore -- see project
# discussion), so every test builds a design first instead of uploading a
# CSV. `predictors` is a named list of level vectors, added one at a time
# via the Predictors tab; `n` is the design's n_per_level.
build_design <- function(session, predictors, n) {
  for (i in seq_along(predictors)) {
    session$setInputs(`PREDICTORS-predictor_name` = names(predictors)[[i]])
    session$setInputs(`PREDICTORS-predictor_levels` = paste(predictors[[i]], collapse = ","))
    session$setInputs(`PREDICTORS-add_predictor` = i)
  }
  session$setInputs(`DESIGN-n_source` = "free")
  session$setInputs(`DESIGN-custom_n` = n)
  session$setInputs(`DESIGN-build` = 1)
}

test_simple_assign <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    build_design(session, list(batch = c("x", "y")), n = 5)
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
    build_design(session, list(g = c("only")), n = 5)
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
    build_design(session, list(arm = c("x")), n = 5)
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
    build_design(session, list(sex = c("f", "m")), n = 10)
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
    # n_per_level = 1 with 10 distinct levels -> every design row has its own
    # unique `unit` value (no replication), so blocking on `unit` puts one
    # row per block (< the 2 groups needed) -- the same "too many unique
    # values" case the upload version of this test exercised on a
    # continuous covariate. (The design used to also carry a generated `id`
    # column that worked just as well for this -- removed now, see project
    # discussion: it confused non-statistician users.)
    build_design(session, list(unit = as.character(1:10)), n = 1)
    session$setInputs(`RANDOMASSIGN-df_source` = "1")
    session$setInputs(`RANDOMASSIGN-randomization_method` = "block")
    session$setInputs(`RANDOMASSIGN-groups` = "A,B")
    session$setInputs(`RANDOMASSIGN-ratios` = "1,1")
    session$setInputs(`RANDOMASSIGN-col` = "arm")
    session$setInputs(`RANDOMASSIGN-block_col` = "unit")
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
    build_design(session, list(sex = c("f", "m")), n = 5)
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
    build_design(session, list(sex = c("f", "m"), site = c("A", "B")), n = 5)
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
    build_design(session, list(sex = c("f", "m")), n = 5)
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

test_random_assign_only_offers_design_results <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    # An imported CSV must not show up as a Random assignment data source
    # anymore -- only a designResult should.
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp))
    utils::write.csv(data.frame(id = 1:5), tmp, row.names = FALSE)
    session$setInputs(`DATA-upload` = data.frame(
      name = "data.csv", size = file.info(tmp)$size, type = "text/csv", datapath = tmp
    ))
    checks[1] <<- identical(names(State$results), "1")
    checks[2] <<- length(OpenDOE:::df_result_choices(State, classes = "designResult")) == 0L

    build_design(session, list(batch = c("x", "y")), n = 5)
    checks[3] <<- identical(names(State$results), c("1", "2"))
    choices <- OpenDOE:::df_result_choices(State, classes = "designResult")
    checks[4] <<- identical(unname(choices), "2")
  })
  expect_true(all(checks))
}
test_random_assign_only_offers_design_results()
