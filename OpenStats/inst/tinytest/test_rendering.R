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
