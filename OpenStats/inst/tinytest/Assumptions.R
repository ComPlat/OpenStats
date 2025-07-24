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
app$set_inputs(conditionedPanels = "Assumption")
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
app$click("ASS-shapiro")
wait(app)
res <- app$get_values()$export
wait(app)
expected <- rbind(
  broom::tidy(shapiro.test(CO2[CO2$Treatment == "nonchilled", "uptake"])),
  broom::tidy(shapiro.test(CO2[CO2$Treatment == "chilled", "uptake"]))
)
expected$variable <- c("nonchilled", "chilled")
expected$`Normal distributed` <- expected$p.value > 0.05
expect_equal(res[["ASS-result_list"]][[3]], expected)

# Update output value
app$click("ASS-shapiroResiduals")
wait(app)
res <- app$get_values()$export
wait(app)
fit <- lm(uptake ~ Treatment, data = CO2)
r <- resid(fit)
expected <- broom::tidy(shapiro.test(r))
expected$`Residuals normal distributed` <- expected$p.value > 0.05
expect_equal(res[["ASS-result_list"]][[4]], expected)

# Update output value
app$click("ASS-levene")
wait(app)
res <- app$get_values()$export
wait(app)
expected <- broom::tidy(car::leveneTest(uptake ~ Treatment,
  data = CO2, center = "mean"
))
expected$`Variance homogenity` <- expected$p.value > 0.05
expect_equal(res[["FO-result_list"]][[5]], expected)

# Update output value
app$click("ASS-DiagnosticPlot")
Sys.sleep(20)
wait(app)
res <- app$get_values()$export
wait(app)
expect_equal(inherits(res[["FO-result_list"]][[6]], "plot"), TRUE)

wait(app)
app$stop()
