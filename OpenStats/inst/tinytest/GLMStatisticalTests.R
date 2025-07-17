wait <- function(app) {
  try(app$wait_for_idle(), silent = TRUE)
}
library(shinytest2)
library(tinytest)
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
app$set_inputs(`FO-model_type` = "Generalised Linear Model")
wait(app)
app$set_inputs(`FO-colnames-dropdown_` = "uptake")
wait(app)
app$set_inputs(`FO-Family` = "Gamma")
wait(app)
app$set_inputs(`FO-Link_function` = "inverse")
wait(app)
app$set_inputs(`FO-editable_code` = "conc * Treatment + Type")
wait(app)
app$click("FO-create_formula")
wait(app)
app$run_js("$('.modal-footer button:contains(\"Close\")').click();")
wait(app)

# ANOVA
app$click("TESTS-aovTest")
wait(app)
res <- app$get_values()$export
wait(app)
CO2$Treatment <- as.character(CO2$Treatment)
CO2$Type <- as.character(CO2$Type)
family <- "Gamma"
link_fct <- "inverse"
family <- str2lang(paste0("stats::", family, "(\"", link_fct, "\")"))
model <- glm(uptake ~ conc * Treatment + Type, data = CO2, family = eval(family))
expected <- broom::tidy(anova(model, test = "Chisq"))
tinytest::expect_equal(res$`FO-result_list`[[3]], expected)

# Posthoc tests
app$set_inputs("TESTS-TestsConditionedPanels" = "Posthoc tests")
wait(app)
run_posthoc_glm <- function(method) {
  family <- "Gamma"
  link_fct <- "inverse"
  family <- str2lang(paste0("stats::", family, "(\"", link_fct, "\")"))
  formula <- uptake ~ conc * Treatment + Type
  f_split <- OpenStats:::split_formula(formula)
  rhs_vars <- OpenStats:::vars_rhs(f_split$right_site)
  df_temp <- OpenStats:::num_to_factor(CO2, rhs_vars)
  if (any(apply(CO2, 2, is.numeric))) {
    warning(paste0("Found numeric predictors and converted them to factors"))
  }
  model <- glm(formula, data = df_temp, family = eval(family))
  emm <- emmeans::emmeans(model, rhs_vars)
  fit <- pairs(emm, adjust = method)
  as.data.frame(fit)
}
choices <- c(
  "tukey",
  "sidak",
  "bonferroni",
  "scheffe",
  "none",
  "fdr",
  "holm",
  "hochberg",
  "hommel"
)
# Tukey
app$set_inputs(`TESTS-PostHocEmmeans` = choices[1])
wait(app)
app$click("TESTS-PostHocEmmeansTest")
Sys.sleep(10)
res <- app$get_values()$export
expected <- run_posthoc_glm(choices[1])
tinytest::expect_equal(res$result_list[[4]], expected)
# Sidak
app$set_inputs(`TESTS-PostHocEmmeans` = choices[2])
wait(app)
app$click("TESTS-PostHocEmmeansTest")
Sys.sleep(10)
res <- app$get_values()$export
expected <- run_posthoc_glm(choices[2])
tinytest::expect_equal(res$result_list[[5]], expected)
# Hommel
app$set_inputs(`TESTS-PostHocEmmeans` = choices[9])
wait(app)
app$click("TESTS-PostHocEmmeansTest")
Sys.sleep(10)
res <- app$get_values()$export
expected <- run_posthoc_glm(choices[9])
tinytest::expect_equal(res$result_list[[6]], expected)

app$stop()
