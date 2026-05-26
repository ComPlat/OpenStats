coverage_test <- nzchar(Sys.getenv("R_COVR"))
run_test <- function(f) {
  if (coverage_test) f(app, srv, FALSE) else f(app, srv, TRUE)
}

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

app <- OpenStats:::app()
srv <- app$server

test_import <- function(app, srv) {
  tmp <- tempfile(fileext = ".csv")
  write.csv(CO2, tmp, row.names = FALSE)
  ex <- NULL
  shiny::testServer(srv, {
    session$setInputs(conditionedPanels = "Data")
    session$setInputs(
      file = list(
        name = "CO2.csv",
        size = file.size(tmp),
        type = "text/csv",
        datapath = tmp
      )
    )
    ex <<- session$userData$export[[1]]
    attr(ex, "rendered") <<- NULL
  })
  unlink(tmp)
  expect_equal(dim(CO2), dim(ex))
  expect_equal(CO2$conc, ex$conc)
  expect_equal(CO2$uptake, ex$uptake)
  expect_true(is.factor(ex$Plant))
  expect_true(is.factor(ex$Treatment))
  expect_true(is.factor(ex$Type))
}
test_import(app, srv)
