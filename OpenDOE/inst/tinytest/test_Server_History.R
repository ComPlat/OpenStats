library(tinytest)

# serialize_params() is what feeds the History (JSON) block of the Excel
# export -- it must produce valid JSON without losing names on plain named
# vectors, and without choking on formula/call objects (mc_anova only).
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

  # must be representable as JSON at all (formula/call objects would error here)
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
  expect_true("A" %in% flat && "B" %in% flat) # predictorTable content made it in
  expect_true("26" %in% flat) # sampleSizeResult content made it in
}
test_create_excel_file()

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
options(OpenDOE.background = FALSE)

# The "Save" download handler bundles results (chronological, titled by
# their label) and a trailing History JSON block into one Excel sheet --
# exercise the exact list build it performs, end to end into a real file.
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

    # Exactly what output$save_history's content() does.
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

# The History tab lists past actions (type/params/time) for inspection --
# it does not recompute anything.
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
    checks[4] <<- identical(names(State$results), "1") # no extra result from just having history
  })
  expect_true(all(checks))
}
test_history_tab_lists_entries_without_recomputing()
