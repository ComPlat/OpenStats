coverage_test <- nzchar(Sys.getenv("R_COVR"))
run_test <- function(f) {
  if (coverage_test) f(app, srv, FALSE) else f(app, srv, TRUE)
}

sync_code <- function(session) {
  session$setInputs(`OP-editable_code` = session$userData$export_code_string)
  session$flushReact()
}

coverage_test <- nzchar(Sys.getenv("R_COVR"))

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

app <- OpenStats:::app()
srv <- app$server

test_ttest <- function(app, srv, in_background) {
  options(OpenStats.background = in_background)

  CO2$Treatment <- as.factor(CO2$Treatment)
  CO2$Type <- as.factor(CO2$Type)
  CO2$conc <- as.factor(CO2$conc)
  expected <- broom::tidy(t.test(uptake ~ Treatment,
    data = CO2, conf.level = 0.95,
    alternative = "two.sided", var.equal = TRUE
  ))

  ib <- getOption("OpenStats.background", TRUE)
  got <- NULL
  shiny::testServer(srv, {
    DataModelState$df      <- CO2
    DataModelState$formula <- new("LinearFormula", formula = uptake ~ Treatment)
    session$flushReact()

    counter <- ResultsState$counter

    session$setInputs(`TESTS-confLevel` = 0.95)
    session$setInputs(`TESTS-altHyp` = "two.sided")
    session$setInputs(`TESTS-varEq` = "eq")
    session$setInputs(`TESTS-tTest` = 1)

    t0 <- Sys.time()
    l0 <- length(ResultsState$all_data)
    repeat {
      ResultsState$bgp$tick(ResultsState, DataModelState, DataWranglingState)
      session$flushReact()

      if (nzchar(ResultsState$bgp$running_status) &&
        grepl("Error|Canceled", ResultsState$bgp$running_status, ignore.case = TRUE)) {
        stop(paste("bgp:", ResultsState$bgp$running_status))
      }

      ex <- session$userData$export
      if (!is.null(ex) && (counter < ResultsState$counter)) break

      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 20)
      stop(paste("timeout; have keys:", paste(names(ex %||% list()), collapse = ",")))

      Sys.sleep(0.05)
    }

    got <<- session$userData$export[[ResultsState$counter]]
    attr(got, "rendered") <<- NULL
  })

  print(tinytest::expect_equal(expected, got))
}
run_test(test_ttest)
