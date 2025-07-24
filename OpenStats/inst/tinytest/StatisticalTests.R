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

# Define formula
app$click("open_formula_editor")
wait(app)
app$set_inputs(`FO-colnames-dropdown_` = "uptake")
wait(app)
app$set_inputs(`FO-editable_code` = "conc * Treatment + Type")
wait(app)
app$click("FO-create_formula")
wait(app)
app$run_js("$('.modal-footer button:contains(\"Close\")').click();")
wait(app)

# ANOVA
app$set_inputs(`TESTS-TestsConditionedPanels` = "More than two groups")
wait(app)
app$click("TESTS-aovTest")
Sys.sleep(10)
wait(app)
res <- app$get_values()$export
res <- res[["result_list"]]
wait(app)
CO2$Treatment <- as.character(CO2$Treatment)
CO2$Type <- as.character(CO2$Type)
expected <- broom::tidy(aov(uptake ~ conc * Treatment + Type, data = CO2))
expected <- cbind(expected, row.names(expected))
names(expected)[ncol(expected)] <- paste0("conc * Treatment + Type", collapse = ".")
expect_equal(res[[3]], expected)

# Kruskal-Wallis
app$click("open_formula_editor")
wait(app)
app$set_inputs(`FO-colnames-dropdown_` = "uptake")
wait(app)
app$set_inputs(`FO-editable_code` = "conc")
wait(app)
app$click("FO-create_formula")
wait(app)
app$run_js("$('.modal-footer button:contains(\"Close\")').click();")
wait(app)

app$set_inputs(`TESTS-TestsConditionedPanels` = "More than two groups")
app$click("TESTS-kruskalTest")
Sys.sleep(10)
wait(app)
res <- app$get_values()$export
res <- res[["result_list"]]
wait(app)
CO2$Treatment <- as.character(CO2$Treatment)
expected <- broom::tidy(kruskal.test(uptake ~ conc, data = CO2))
expected <- cbind(expected, row.names(expected))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
expect_equal(res[[5]], expected)

# PostHoc tests
# TukeyHSD
app$set_inputs(`TESTS-TestsConditionedPanels` = "Posthoc tests")
wait(app)
app$set_inputs(`TESTS-PostHocTests` = "HSD")
wait(app)
app$click("TESTS-PostHocTest")
Sys.sleep(10)
wait(app)
res <- app$get_values()$export
res <- res[["result_list"]]
fit <- aov(uptake ~ conc, data = CO2)
fit <- agricolae::HSD.test(fit,
  trt = "conc",
  alpha = 0.05, group = TRUE, unbalanced = TRUE
)$groups
expected <- cbind(fit, row.names(fit))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
expect_equal(res[[6]], expected)

app$set_inputs(`TESTS-PostHocTests` = "HSD")
wait(app)
app$set_inputs(`TESTS-design` = "ub")
wait(app)
app$click("TESTS-PostHocTest")
Sys.sleep(10)
wait(app)
res <- app$get_values()$export
res <- res[["result_list"]]
wait(app)
fit <- aov(uptake ~ conc, data = CO2)
fit <- agricolae::HSD.test(fit,
  trt = "conc",
  alpha = 0.05, group = TRUE, unbalanced = FALSE
)$groups
expected <- cbind(fit, row.names(fit))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
expect_equal(res[[7]], expected)

# Kruskal-Wallis test
app$set_inputs(`TESTS-PostHocTests` = "kruskalTest")
wait(app)
app$set_inputs(`TESTS-padj` = "BH")
wait(app)
app$click("TESTS-PostHocTest")
Sys.sleep(10)
wait(app)
res <- app$get_values()$export
res <- res[["result_list"]]
wait(app)
df <- CO2
dep <- "uptake"
fit <- with(df, agricolae::kruskal(df[, dep], df[, "conc"]),
  alpha = 0.05, p.adj = "BH", group = TRUE
)$groups
expected <- cbind(fit, row.names(fit))
names(expected) <- c("uptake", "groups", "conc")
expect_equal(res[[8]], expected)

# LSD
app$set_inputs(`TESTS-PostHocTests` = "LSD")
wait(app)
app$set_inputs(`TESTS-padj` = "BY")
wait(app)
app$click("TESTS-PostHocTest")
Sys.sleep(10)
wait(app)
res <- app$get_values()$export
res <- res[["result_list"]]
wait(app)
df <- CO2
dep <- "uptake"
fit <- aov(uptake ~ conc, data = CO2)
fit <- agricolae::LSD.test(
  fit,
  trt = "conc",
  alpha = 0.05, p.adj = "BY", group = TRUE
)$groups
expected <- cbind(fit, row.names(fit))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
expect_equal(res[[9]], expected)

# scheffe
# aov_res <- aov(formula, data = df)
# fit <- agricolae::scheffe.test(aov_res, trt = indep, alpha = input$pval, group = TRUE)$groups
app$set_inputs(`TESTS-PostHocTests` = "scheffe")
wait(app)
app$click("TESTS-PostHocTest")
Sys.sleep(10)
wait(app)
res <- app$get_values()$export
res <- res[["result_list"]]
wait(app)
fit <- aov(uptake ~ conc, data = CO2)
fit <- agricolae::scheffe.test(
  fit,
  trt = "conc",
  alpha = 0.05, group = TRUE
)$groups
expected <- cbind(fit, row.names(fit))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
expect_equal(res[[10]], expected)

# REGW
app$set_inputs(`TESTS-PostHocTests` = "REGW")
wait(app)
app$click("TESTS-PostHocTest")
Sys.sleep(10)
wait(app)
res <- app$get_values()$export
res <- res[["result_list"]]
wait(app)
fit <- aov(uptake ~ conc, data = CO2)
fit <- agricolae::REGW.test(
  fit,
  trt = "conc",
  alpha = 0.05, group = TRUE
)$groups
expected <- cbind(fit, row.names(fit))
names(expected)[ncol(expected)] <- paste0("conc", collapse = ".")
expect_equal(res[[11]], expected)

app$stop()
