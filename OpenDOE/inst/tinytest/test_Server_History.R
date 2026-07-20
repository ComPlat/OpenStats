library(tinytest)

test_serialize_params_preserves_names_as_json_object <- function() {
  params <- list(means = c(A = 0, B = 1, C = 2), cohens_d = 0.8)
  j <- jsonlite::toJSON(OpenDOE:::serialize_params("mc", params), auto_unbox = TRUE)
  parsed <- jsonlite::fromJSON(j, simplifyVector = FALSE)
  expect_equal(names(parsed$means), c("A", "B", "C"))
  expect_equal(unlist(parsed$means), c(A = 0, B = 1, C = 2))
}
test_serialize_params_preserves_names_as_json_object()

test_serialize_params_deparses_formula_and_call_for_mc_anova <- function() {
  params <- list(
    formula = stats::as.formula("values ~ treatment + dose"),
    interactions = list(list(mask = quote(treatment == "A" & dose == "low"), value = 1.5))
  )
  safe <- OpenDOE:::serialize_params("mc_anova", params)
  expect_true(is.character(safe$formula))
  expect_equal(safe$formula, "values ~ treatment + dose")
  expect_true(is.character(safe$interactions[[1]]$mask))
  j <- jsonlite::toJSON(safe, auto_unbox = TRUE)
  expect_true(is.character(j))
}
test_serialize_params_deparses_formula_and_call_for_mc_anova()

test_create_excel_file <- function() {
  if (!requireNamespace("openxlsx", quietly = TRUE)) exit_file("needs openxlsx")
  r1 <- methods::new("predictorTable", df = data.frame(
    `treatment (character)` = c("A", "B"), check.names = FALSE
  ))
  attr(r1, "label") <- "1 Predictor table"
  r2 <- methods::new("sampleSizeResult", n = 26)
  attr(r2, "label") <- "2 Power analysis: t-test"
  l <- list(r1, r2, "some history json text")
  names(l) <- c(attr(r1, "label"), attr(r2, "label"), "History (JSON)")
  fn <- OpenDOE:::create_excel_file(l)
  on.exit(unlink(fn))
  expect_true(file.exists(fn))
  raw <- openxlsx::read.xlsx(fn, sheet = "Results", colNames = FALSE)
  flat <- as.character(unlist(raw))
  expect_true("1 Predictor table" %in% flat)
  expect_true("2 Power analysis: t-test" %in% flat)
  expect_true("History (JSON)" %in% flat)
  expect_true("some history json text" %in% flat)
  expect_true("A" %in% flat && "B" %in% flat)
  expect_true("26" %in% flat)
}
test_create_excel_file()

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
options(OpenDOE.background = FALSE)

test_save_history_content_builds_valid_excel <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)
  fn <- tempfile(fileext = ".xlsx")
  on.exit(unlink(fn))

  shiny::testServer(app$server, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B")
    session$setInputs(`PREDICTORS-add_predictor` = 1)
    session$setInputs(`PREDICTORS-add_to_results` = 1)

    session$setInputs(`SAMPLESIZE-primary_factor` = "treatment")
    session$setInputs(`SAMPLESIZE-cohens_d` = 0.8)
    session$setInputs(`SAMPLESIZE-sig_level_ttest` = 0.05)
    session$setInputs(`SAMPLESIZE-desired_power_ttest` = 0.8)
    session$setInputs(`SAMPLESIZE-calc_ttest` = 1)

    l <- State$results
    names(l) <- vapply(State$results, function(v) attr(v, "label"), character(1))
    history_json <- jsonlite::toJSON(
      OpenDOE:::history_entries_list(State), pretty = TRUE, auto_unbox = TRUE, null = "null"
    )
    l <- c(l, stats::setNames(list(as.character(history_json)), "History (JSON)"))
    generated <- OpenDOE:::create_excel_file(l)
    file.copy(generated, fn, overwrite = TRUE)
    unlink(generated)
  })

  raw <- openxlsx::read.xlsx(fn, sheet = "Results", colNames = FALSE)
  flat <- as.character(unlist(raw))
  checks[1] <- "1 Predictor table" %in% flat
  checks[2] <- "2 Power analysis: t-test" %in% flat
  checks[3] <- "History (JSON)" %in% flat
  checks[4] <- any(grepl('"ttest"', flat, fixed = TRUE)) && any(grepl('"type"', flat, fixed = TRUE))
  expect_true(all(checks))
}
test_save_history_content_builds_valid_excel()

test_history_tab_lists_entries_without_recomputing <- function() {
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

    checks[1] <<- identical(names(State$history), "1")
    checks[2] <<- State$history[["1"]]$type == "ttest"
    checks[3] <<- State$history[["1"]]$params$cohens_d == 0.8
    checks[4] <<- identical(names(State$results), "1")
  })
  expect_true(all(checks))
}
test_history_tab_lists_entries_without_recomputing()

drain_replay_queue <- function(State) {
  while (length(State$replay_queue) > 0L) {
    queue <- State$replay_queue
    entry <- queue[[1]]
    State$replay_queue <- queue[-1]
    OpenDOE:::replay_history_entry(State, entry)
  }
}

test_replay_json_reproduces_result_without_mutating_original <- function() {
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

    history_json <- jsonlite::toJSON(
      OpenDOE:::history_entries_list(State), auto_unbox = TRUE, null = "null"
    )
    session$setInputs(`HISTORY-history_json` = as.character(history_json))
    session$setInputs(`HISTORY-replay_json` = 1)
    drain_replay_queue(State)

    checks[1] <<- identical(names(State$results), c("1", "2"))
    checks[2] <<- State$results[["1"]]@n == State$results[["2"]]@n
    checks[3] <<- grepl("replay", attr(State$results[["2"]], "label"), fixed = TRUE)
    checks[4] <<- attr(State$results[["1"]], "label") == "1 Power analysis: t-test"
  })
  expect_true(all(checks))
}
test_replay_json_reproduces_result_without_mutating_original()

test_replay_json_recomputes_across_operation_types <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B")
    session$setInputs(`PREDICTORS-add_predictor` = 1)
    session$setInputs(`DESIGN-n_source` = "free")
    session$setInputs(`DESIGN-custom_n` = 5)
    session$setInputs(`DESIGN-build` = 1)
    checks[1] <<- identical(names(State$results), "1")

    history_json <- jsonlite::toJSON(
      OpenDOE:::history_entries_list(State), auto_unbox = TRUE, null = "null"
    )
    session$setInputs(`HISTORY-history_json` = as.character(history_json))
    session$setInputs(`HISTORY-replay_json` = 1)
    drain_replay_queue(State)

    checks[2] <<- identical(names(State$results), c("1", "2"))
    checks[3] <<- inherits(State$results[["2"]], "designResult")
    checks[4] <<- nrow(State$results[["2"]]@df) == nrow(State$results[["1"]]@df)
    checks[5] <<- grepl("replay", attr(State$results[["2"]], "label"), fixed = TRUE)
  })
  expect_true(all(checks))
}
test_replay_json_recomputes_across_operation_types()

test_replay_rejects_unsupported_entry_types <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  utils::write.csv(data.frame(id = 1:5), tmp, row.names = FALSE)

  shiny::testServer(app$server, {
    session$setInputs(`DATA-upload` = data.frame(
      name = "data.csv", size = file.info(tmp)$size, type = "text/csv", datapath = tmp
    ))
    entry <- State$history[["1"]]
    checks[1] <<- entry$type == "import_csv"
    OpenDOE:::replay_history_entry(State, entry)
    checks[2] <<- identical(names(State$results), "1")
  })
  expect_true(all(checks))
}
test_replay_rejects_unsupported_entry_types()

test_replay_json_sets_running_flag_when_queuing <- function() {
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

    history_json <- jsonlite::toJSON(
      OpenDOE:::history_entries_list(State), auto_unbox = TRUE, null = "null"
    )
    session$setInputs(`HISTORY-history_json` = as.character(history_json))
    session$setInputs(`HISTORY-replay_json` = 1)

    checks[1] <<- isTRUE(State$history_replay_running)
    checks[2] <<- length(State$replay_queue) == 1L
  })
  expect_true(all(checks))
}
test_replay_json_sets_running_flag_when_queuing()

test_cancel_replay_clears_queue_and_stops_running_job <- function() {
  old <- getOption("OpenDOE.background")
  options(OpenDOE.background = TRUE)
  on.exit(options(OpenDOE.background = old))

  app <- OpenDOE:::app()
  checks <- logical(0)

  shiny::testServer(app$server, {
    State$bgp$start(
      fun = function() { Sys.sleep(30); "should never finish" },
      args = list(),
      on_success = function(res) NULL
    )
    State$replay_queue <- list(list(
      type = "ttest", label = "queued behind the running job",
      params = list(
        predictors = list(treatment = c("A", "B")), primary_factor = "treatment",
        cohens_d = 0.8, sig_level = 0.05, desired_power = 0.8
      )
    ))
    State$history_replay_running <- TRUE
    checks[1] <<- State$bgp$running

    session$setInputs(`HISTORY-cancel_replay` = 1)

    checks[2] <<- length(State$replay_queue) == 0L
    checks[3] <<- !isTRUE(State$history_replay_running)
    checks[4] <<- !State$bgp$running
    checks[5] <<- length(State$results) == 0L
  })
  expect_true(all(checks))
}
test_cancel_replay_clears_queue_and_stops_running_job()
