if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

app <- OpenStats:::app()
srv <- app$server

test_correlation <- function(app, srv) {
  expected_pear <- suppressWarnings(cor.test(CO2$uptake, CO2$conc, method = "pearson"))
  expected_pear <- broom::tidy(expected_pear)
  expected_spear <- suppressWarnings(cor.test(CO2$uptake, CO2$conc, method = "spearman"))
  expected_spear <- broom::tidy(expected_spear)
  expected_kendall <- suppressWarnings(cor.test(CO2$uptake, CO2$conc, method = "kendall"))
  expected_kendall <- broom::tidy(expected_kendall)
  ex_pear <- NULL
  ex_spear <- NULL
  ex_kendall <- NULL

  shiny::testServer(srv, {
    DataModelState$df      <- CO2
    DataModelState$formula <- new("LinearFormula", formula = uptake ~ conc)

    session$flushReact()
    session$setInputs(`TESTS-conflevel` = 0.95)
    session$setInputs(`TESTS-alt` = "two.sided")
    session$setInputs(`TESTS-pear` = 1)
    ex_pear <<- session$userData$export[[ResultsState$counter]]
    attr(ex_pear, "rendered") <<- NULL

    session$setInputs(`TESTS-spear` = 1)
    ex_spear <<- session$userData$export[[ResultsState$counter]]
    attr(ex_spear, "rendered") <<- NULL

    session$setInputs(`TESTS-kendall` = 1)
    ex_kendall <<- session$userData$export[[ResultsState$counter]]
    attr(ex_kendall, "rendered") <<- NULL
  })
  expect_equal(ex_pear, expected_pear)
  expect_equal(ex_spear, expected_spear)
  expect_equal(ex_kendall, expected_kendall)
}
test_correlation(app, srv)
