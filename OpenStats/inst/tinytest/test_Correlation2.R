if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

app <- OpenStats:::app()
srv <- app$server

test_correlation <- function(app, srv) {
  expected_pear <- cor.test(CO2$uptake, CO2$conc, method = "pearson")
  expected_pear <- broom::tidy(expected_pear)
  expected_spear <- cor.test(CO2$uptake, CO2$conc, method = "spearman")
  expected_spear <- broom::tidy(expected_spear)
  expected_kendall <- cor.test(CO2$uptake, CO2$conc, method = "kendall")
  expected_kendall <- broom::tidy(expected_kendall)
  ex_pear <- NULL
  ex_spear <- NULL
  ex_kendall <- NULL
  
  shiny::testServer(srv, {
    DataModelState$df      <- CO2
    DataModelState$formula <- new("LinearFormula", formula = uptake ~ conc)

    session$flushReact()
    session$setInputs(`CORR-conflevel` = 0.95)
    session$setInputs(`CORR-alt` = "two.sided")
    session$setInputs(`CORR-pear` = 1)
    session$setInputs(`CORR-spear` = 1)
    session$setInputs(`CORR-kendall` = 1)
    ex_pear <<- session$userData$export[[1]]
    attr(ex_pear, "rendered") <<- NULL
    ex_spear <<- session$userData$export[[2]]
    attr(ex_spear, "rendered") <<- NULL
    ex_kendall <<- session$userData$export[[3]]
    attr(ex_kendall, "rendered") <<- NULL
  })
  tinytest::expect_equal(ex_pear, expected_pear)
  tinytest::expect_equal(ex_spear, expected_spear)
  tinytest::expect_equal(ex_kendall, expected_kendall)
}
test_correlation(app, srv)
