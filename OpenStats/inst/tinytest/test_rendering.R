if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

app <- OpenStats:::app()
srv <- app$server

# TODO: finish
test_rendering_formula_editor <- function(app, srv) {
  checks <- c()
  shiny::testServer(srv, {
    outputOptions(output, "buttons", suspendWhenHidden = FALSE)

    DataModelState$df <- CO2
    session$setInputs(`FO-model_type` = "Linear")
    session$flushReact()

    ui_obj <- output[["FO-buttons"]]
    html   <- htmltools::renderTags(ui_obj)$html

    checks <<- c(checks, grepl('id="FO-add"', html))
    checks <<- c(checks, grepl('id="FO-minus"', html))
    checks <<- c(checks, grepl('id="FO-mul"', html))
    checks <<- c(checks, grepl('id="FO-colon"', html))

    ui_obj <- output[["FO-colnames_list"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('id="FO-colnames_Plant_"', html))
    checks <<- c(checks, grepl('id="FO-colnames_Type_"', html))
    checks <<- c(checks, grepl('id="FO-colnames_Treatment_"', html))
    checks <<- c(checks, grepl('id="FO-colnames_conc_"', html))
    checks <<- c(checks, grepl('id="FO-colnames_uptake_"', html))

    ui_obj <- output[["FO-colnames_dropdown"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('Type', html))
    checks <<- c(checks, grepl('Treatment', html))
    checks <<- c(checks, grepl('conc', html))
    checks <<- c(checks, grepl('uptake', html))

    session$setInputs(`FO-model_type` = "Optimization Model")
    session$flushReact()
    ui_obj <- output[["FO-optim_predefined_equations"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('linear', html))
    checks <<- c(checks, grepl('log_linear', html))
    checks <<- c(checks, grepl('michaelis_menten', html))
    checks <<- c(checks, grepl('one_site_binding', html))
    checks <<- c(checks, grepl('two_hot_binding', html))
    checks <<- c(checks, grepl('free', html))

    ui_obj <- try(output[["FO-rhs"]], silent = TRUE)
    checks <<- c(checks, inherits(ui_obj, "try-error")) # As rhs text field is not existing for this case
    session$setInputs(`FO-model_type` = "LinearFormula")
    session$flushReact()
    ui_obj <- output[["FO-optim_predefined_equations"]]
    checks <<- c(checks, class(ui_obj) == "NULL")

    session$setInputs(`FO-model_type` = "Optimization Model")
    session$setInputs(`FO-PredefinedModels` = "linear")
    session$flushReact()
    ui_obj <- output[["FO-predefined_modelsUI"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('linear_lhs_var', html))
    checks <<- c(checks, grepl('linear_slope', html))
    checks <<- c(checks, grepl('linear_x', html))
    checks <<- c(checks, grepl('linear_intercept', html))

    session$setInputs(`FO-model_type` = "Optimization Model")
    session$setInputs(`FO-PredefinedModels` = "log_linear")
    ui_obj <- output[["FO-predefined_modelsUI"]]
    session$flushReact()
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('log_linear_lhs_var', html))
    checks <<- c(checks, grepl('log_linear_slope', html))
    checks <<- c(checks, grepl('log_linear_x', html))
    checks <<- c(checks, grepl('log_linear_intercept', html))

    session$setInputs(`FO-model_type` = "Optimization Model")
    session$setInputs(`FO-PredefinedModels` = "michaelis_menten")
    ui_obj <- output[["FO-predefined_modelsUI"]]
    session$flushReact()
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('mm_lhs_var', html))
    checks <<- c(checks, grepl('mm_vmax', html))
    checks <<- c(checks, grepl('mm_x', html))
    checks <<- c(checks, grepl('mm_km', html))

    session$setInputs(`FO-model_type` = "Optimization Model")
    session$setInputs(`FO-PredefinedModels` = "one_site_binding")
    ui_obj <- output[["FO-predefined_modelsUI"]]
    session$flushReact()
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('binding_lhs_var', html))
    checks <<- c(checks, grepl('binding_bmax', html))
    checks <<- c(checks, grepl('binding_x', html))
    checks <<- c(checks, grepl('binding_kd', html))

    session$setInputs(`FO-model_type` = "Optimization Model")
    session$setInputs(`FO-PredefinedModels` = "two_hot_binding")
    ui_obj <- output[["FO-predefined_modelsUI"]]
    session$flushReact()
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('hotbind_lhs', html))
    checks <<- c(checks, grepl('hotbind_conc', html))
    checks <<- c(checks, grepl('hotbind_koff', html))
    checks <<- c(checks, grepl('hotbind_kon', html))
    checks <<- c(checks, grepl('hotbind_bmax', html))
    checks <<- c(checks, grepl('hotbind_time', html))
    
    session$setInputs(`FO-model_type` = "Generalised Linear Model")
    ui_obj <- output[["FO-glm_family_dropdown"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('gaussian', html))
    checks <<- c(checks, grepl('inverse.gaussian', html))
    checks <<- c(checks, grepl('Gamma', html))
    checks <<- c(checks, grepl('poisson', html))
    checks <<- c(checks, grepl('quasi', html))
    checks <<- c(checks, grepl('quasibinomial', html))
    checks <<- c(checks, grepl('quasipoisson', html))

    session$setInputs(`FO-model_type` = "Generalised Linear Model")
    session$setInputs(`FO-Family` = "gaussian")
    ui_obj <- output[["FO-glm_link_fct_dropdown"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('identity', html))
    checks <<- c(checks, grepl('log', html))
    checks <<- c(checks, grepl('inverse', html))

    session$setInputs(`FO-model_type` = "Generalised Linear Model")
    session$setInputs(`FO-Family` = "Gamma")
    ui_obj <- output[["FO-glm_link_fct_dropdown"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('identity', html))
    checks <<- c(checks, grepl('log', html))
    checks <<- c(checks, grepl('inverse', html))

    session$setInputs(`FO-model_type` = "Generalised Linear Model")
    session$setInputs(`FO-Family` = "inverse.gaussian")
    ui_obj <- output[["FO-glm_link_fct_dropdown"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('identity', html))
    checks <<- c(checks, grepl('log', html))
    checks <<- c(checks, grepl('inverse', html))
    checks <<- c(checks, grepl('1/mu', html))

    session$setInputs(`FO-model_type` = "Generalised Linear Model")
    session$setInputs(`FO-Family` = "binomial")
    ui_obj <- output[["FO-glm_link_fct_dropdown"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('logit', html))
    checks <<- c(checks, grepl('probit', html))
    checks <<- c(checks, grepl('cauchit', html))

    session$setInputs(`FO-model_type` = "Generalised Linear Model")
    session$setInputs(`FO-Family` = "poisson")
    ui_obj <- output[["FO-glm_link_fct_dropdown"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('identity', html))
    checks <<- c(checks, grepl('log', html))
    checks <<- c(checks, grepl('sqrt', html))


    session$setInputs(`FO-model_type` = "Generalised Linear Model")
    session$setInputs(`FO-Family` = "quasi")
    ui_obj <- output[["FO-glm_link_fct_dropdown"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('identity', html))
    checks <<- c(checks, grepl('inverse', html))
    checks <<- c(checks, grepl('log', html))
    checks <<- c(checks, grepl('cloglog', html))
    checks <<- c(checks, grepl('logit', html))
    checks <<- c(checks, grepl('probit', html))
    checks <<- c(checks, grepl('1/mu', html))
    checks <<- c(checks, grepl('sqrt', html))

    session$setInputs(`FO-model_type` = "Optimization Model")
    session$setInputs(`FO-PredefinedModels` = "linear")
    session$flushReact()
    ui_obj <- output[["FO-optim_boundaries"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('LowerBoundary', html))
    checks <<- c(checks, grepl('UpperBoundary', html))
    checks <<- c(checks, grepl('Seed', html))
  })
   expect_true(all(checks))
}
test_rendering_formula_editor(app, srv)

test_rendering_statistical_tests <- function(app, srv) {
  checks <- c()
  shiny::testServer(srv, {
    DataModelState$df <- CO2
    DataModelState$formula <- new("LinearFormula", formula = uptake ~ conc)
    session$setInputs(active_tab = "Tests")
    session$flushReact()

    ui_obj <- output[["TESTS-tabs"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('Two groups', html))
    checks <<- c(checks, grepl('More than two groups', html))
    checks <<- c(checks, grepl('Posthoc tests', html))

    # T Test
    session$setInputs(`TESTS-TestsConditionedPanels`= "Two groups")
    session$flushReact()
    ui_obj <- output[["TESTS-SidebarTestsUI"]]
    html <- htmltools::renderTags(ui_obj)$html
    html <- strsplit(html, "<div")[[1]]
    checks <<- c(checks, grepl('confLevel', html[[3]]))
    checks <<- c(checks, grepl('data-from=\"0.95\"', html[[3]]))
    checks <<- c(checks, grepl('data-min=\"0\"', html[[3]]))
    checks <<- c(checks, grepl('data-max=\"1\"', html[[3]]))
    # alternative hypthosesis
    checks <<- c(checks, grepl('altHyp', html[[5]]))
    checks <<- c(checks, grepl('two.sided', html[[5]]))
    checks <<- c(checks, grepl('less', html[[5]]))
    checks <<- c(checks, grepl('greater', html[[5]]))
    # variances equal
    checks <<- c(checks, grepl('varEq', html[[7]]))
    checks <<- c(checks, grepl('eq', html[[7]]))
    checks <<- c(checks, grepl('noeq', html[[7]]))
    checks <<- c(checks, grepl('TESTS-tTest', html[[7]]))

    # More than two groups
    session$setInputs(`TESTS-TestsConditionedPanels`= "More than two groups")
    ui_obj <- output[["TESTS-SidebarTestsUI"]]
    html <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('aovTest', html))
    checks <<- c(checks, grepl('kruskalTest', html))

    # Posthocs
    session$setInputs(`TESTS-TestsConditionedPanels` = "Posthoc tests")
    ui_obj <- output[["TESTS-SidebarTestsUI"]]
    html <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('TESTS-PostHocTests', html))
    checks <<- c(checks, grepl('kruskalTest', html))
    checks <<- c(checks, grepl('LSD', html))
    checks <<- c(checks, grepl('scheffe', html))
    checks <<- c(checks, grepl('REGW', html))
    checks <<- c(checks, grepl('TESTS-PostHocTest', html))
    checks <<- c(checks, grepl('TESTS-design', html))

    session$flushReact()
    DataModelState$formula <- new("GeneralisedLinearFormula", formula = uptake ~ conc, family = "Gamma", link_fct = "inverse")
    session$setInputs(`TESTS-TestsConditionedPanels` = "Posthoc tests")
    ui_obj <- output[["TESTS-SidebarTestsUI"]]
    html <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('TESTS-PostHocEmmeans', html))
    checks <<- c(checks, grepl('sidak', html))
    checks <<- c(checks, grepl('bonferroni', html))
    checks <<- c(checks, grepl('scheffe', html))
    checks <<- c(checks, grepl('fdr', html))
    checks <<- c(checks, grepl('holm', html))
    checks <<- c(checks, grepl('hochberg', html))
    checks <<- c(checks, grepl('hommel', html))

  })
  expect_true(all(checks))
}
test_rendering_statistical_tests(app, srv)

test_rendering_visualization <- function(app, srv) {
  checks <- c()
  shiny::testServer(srv, {
    DataModelState$df <- CO2
    session$setInputs(active_tab = "Visualisation")
    session$flushReact()

    ui_obj <- output[["VIS-CreateModelBoxUI"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <- html == "" # as no model is present

    DataModelState$formula <- new("LinearFormula", formula = uptake ~ conc)
    session$setInputs(active_tab = "Visualisation")
    ui_obj <- output[["VIS-CreateModelBoxUI"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('Plot model', html)) # PLot model
    checks <<- c(checks, grepl('CreateModelBox', html)) # PLot model

    ui_obj <- output[["VIS-CreateModelScatterUI"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('CreateModelScatter', html))

    ui_obj <- output[["VIS-CreateModelLineUI"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('CreateModelLine', html))

    session$setInputs(`VIS-xVar` = "conc")
    ui_obj <- output[["VIS-XRangeUI"]]
    html <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('VIS-XRange', html))
    checks <<- c(checks, grepl('data-to="1250"', html))
    checks <<- c(checks, grepl('data-from="47.5"', html))

    session$setInputs(`VIS-yVar` = "uptake")
    ui_obj <- output[["VIS-YRangeUI"]]
    html <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('VIS-YRange', html))
    checks <<- c(checks, grepl('data-to="47.775"', html))
    checks <<- c(checks, grepl('data-from="7.315"', html))

    ui_obj <- output[["VIS-yVarUI"]]
    html <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('Select the value of the Y variable', html))
    checks <<- c(checks, grepl('Type', html))
    checks <<- c(checks, grepl('Treatment', html))
    checks <<- c(checks, grepl('conc', html))
    checks <<- c(checks, grepl('uptake', html))
    ui_obj <- output[["VIS-xVarUI"]]
    html <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('Select the value of the X variable', html))
    checks <<- c(checks, grepl('Type', html))
    checks <<- c(checks, grepl('Treatment', html))
    checks <<- c(checks, grepl('conc', html))
    checks <<- c(checks, grepl('uptake', html))

    ui_obj <- output[["VIS-fillUI"]]
    html <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('VIS-fill', html))
    ui_obj <- output[["VIS-colUI"]]
    html <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('VIS-col', html))
    ui_obj <- output[["VIS-facetByUI"]]
    html <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('VIS-facetBy', html))
    ui_obj <- output[["VIS-facetScalesUI"]]
    html <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('fixed', html))
    checks <<- c(checks, grepl('free', html))

  })
  expect_true(all(checks))
}
test_rendering_visualization(app, srv)

test_rendering_split_by_group <- function(app, srv) {
  checks <- c()
  shiny::testServer(srv, {
    outputOptions(output, "buttons", suspendWhenHidden = FALSE)

    DataModelState$df <- CO2
    DataModelState$formula <- new("LinearFormula", formula = uptake ~ conc)
    session$flushReact()

    ui_obj <- output[["SG-colnames_dropdown"]]
    html   <- htmltools::renderTags(ui_obj)$html
    checks <<- c(checks, grepl('Select the column by name which you want to split by', html))
    checks <<- c(checks, grepl('Type', html))
    checks <<- c(checks, grepl('Treatment', html))
    checks <<- c(checks, grepl('conc', html))
    checks <<- c(checks, grepl('uptake', html))

    # NOTE: chossing the respective colname "does not work" thus I cannot test the selected groups

  })
   expect_true(all(checks))
}
test_rendering_split_by_group(app, srv)
