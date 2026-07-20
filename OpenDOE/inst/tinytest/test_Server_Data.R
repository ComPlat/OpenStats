if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

test_upload_creates_imported_data_result <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  utils::write.csv(data.frame(id = 1:5, weight = c(20, 21, 22, 23, 24)), tmp, row.names = FALSE)

  shiny::testServer(app$server, {
    session$setInputs(`DATA-upload` = data.frame(
      name = "subjects.csv", size = file.info(tmp)$size,
      type = "text/csv", datapath = tmp
    ))

    checks[1] <<- identical(names(State$results), "1")
    checks[2] <<- inherits(State$results[["1"]], "importedData")
    checks[3] <<- identical(dim(State$results[["1"]]@df), c(5L, 2L))
    checks[4] <<- attr(State$results[["1"]], "label") == "1 subjects.csv"
    checks[5] <<- State$history[["1"]]$type == "import_csv"
  })
  expect_true(all(checks))
}
test_upload_creates_imported_data_result()

test_upload_rejects_empty_file <- function() {
  app <- OpenDOE:::app()
  checks <- logical(0)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  utils::write.csv(data.frame(id = integer(0)), tmp, row.names = FALSE)

  shiny::testServer(app$server, {
    session$setInputs(`DATA-upload` = data.frame(
      name = "empty.csv", size = file.info(tmp)$size,
      type = "text/csv", datapath = tmp
    ))
    checks[1] <<- length(State$results) == 0L
  })
  expect_true(all(checks))
}
test_upload_rejects_empty_file()
