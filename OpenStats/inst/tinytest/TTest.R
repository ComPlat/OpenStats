library(shinytest2)
library(tinytest)
wait <- function(app) {
  try(app$wait_for_idle(), silent = TRUE)
}
app <- OpenStats:::app()
app <- shiny::shinyApp(app$ui, app$server)
app <- AppDriver$new(app)
wait(app)
app$upload_file(
  file = system.file("/test_data/CO2.csv", package = "OpenStats")
)
wait(app)

app$set_inputs(conditionedPanels = "Tests")
wait(app)
app$click("open_formula_editor")
wait(app)
app$set_inputs(`FO-colnames-dropdown_` = "uptake")
wait(app)
app$click("FO-colnames_Treatment_")
wait(app)
app$click("FO-create_formula")
wait(app)
app$run_js("$('.modal-footer button:contains(\"Close\")').click();")
wait(app)

app$click("TESTS-tTest")
Sys.sleep(10)
wait(app)
res <- app$get_values()$export
res <- res[["FO-result_list"]]
wait(app)
CO2$Treatment <- as.character(CO2$Treatment)
expected <- broom::tidy(
  t.test(
    uptake ~ Treatment,
    data = CO2,
    var.equal = TRUE, conf.level = 0.95,
    alternative = "two.sided"
  )
)
expect_equal(res[[3]], expected)

# Update output value
app$set_inputs(`TESTS-altHyp` = "less")
wait(app)
app$click("TESTS-tTest")
Sys.sleep(10)
wait(app)
res <- app$get_values()$export
res <- res[["FO-result_list"]]
expected <- broom::tidy(
  t.test(
    uptake ~ Treatment,
    data = CO2,
    var.equal = TRUE, conf.level = 0.95,
    alternative = "less"
  )
)
expect_equal(res[[4]], expected)

# Update output value
app$set_inputs(`TESTS-altHyp` = "greater")
wait(app)
app$click("TESTS-tTest")
Sys.sleep(10)
wait(app)
res <- app$get_values()$export
res <- res[["FO-result_list"]]
expected <- broom::tidy(
  t.test(
    uptake ~ Treatment,
    data = CO2,
    var.equal = TRUE, conf.level = 0.95,
    alternative = "greater"
  )
)
expect_equal(res[[5]], expected)

# Update output value
app$set_inputs(`TESTS-varEq` = "noeq")
wait(app)
app$click("TESTS-tTest")
Sys.sleep(10)
wait(app)
res <- app$get_values()$export
res <- res[["FO-result_list"]]
expected <- broom::tidy(
  t.test(
    uptake ~ Treatment,
    data = CO2,
    var.equal = FALSE, conf.level = 0.95,
    alternative = "greater"
  )
)
expect_equal(res[[6]], expected)

app$stop()
