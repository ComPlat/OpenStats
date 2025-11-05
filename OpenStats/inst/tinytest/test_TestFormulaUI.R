sync_code <- function(session) {
  session$setInputs(`FO-editable_code` = session$userData$export_formula_rhs)
  session$flushReact()
}

coverage_test <- nzchar(Sys.getenv("R_COVR"))

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

app <- OpenStats:::app()
srv <- app$server

test_linear_formula <- function(app, srv) {
  options(OpenStats.background = FALSE)
  expected <- list(
    broom::tidy(lm(uptake ~ conc + Treatment, data = CO2)),
    broom::tidy(lm(uptake ~ conc * Treatment, data = CO2)),
    broom::tidy(lm(uptake ~ Plant - Treatment, data = CO2))
  )
  ex <- list()
  shiny::testServer(srv, {
    DataModelState$df <- CO2

    session$setInputs(`FO-model_type` = "Linear")
    session$setInputs(`FO-colnames-dropdown_` = "uptake");
    session$setInputs(`FO-colnames_conc_` = 1); sync_code(session)
    session$setInputs(`FO-add` = 1); sync_code(session)
    session$setInputs(`FO-colnames_Treatment_` = 1); sync_code(session)
    session$setInputs(`FO-create_formula` = 1)
    ex[[1]] <<- session$userData$export[[1]]@summary
    session$userData$export_formula_rhs<- ""; sync_code(session)

    session$setInputs(`FO-colnames-dropdown_` = "uptake");
    session$setInputs(`FO-colnames_conc_` = 1); sync_code(session)
    session$setInputs(`FO-mul` = 1); sync_code(session)
    session$setInputs(`FO-colnames_Treatment_` = 1); sync_code(session)
    session$setInputs(`FO-create_formula` = 1)
    ex[[2]] <<- session$userData$export[[2]]@summary
    session$userData$export_formula_rhs<- ""; sync_code(session)

    session$setInputs(`FO-colnames-dropdown_` = "uptake");
    session$setInputs(`FO-colnames_Plant_` = 1); sync_code(session)
    session$setInputs(`FO-minus` = 1); sync_code(session)
    session$setInputs(`FO-colnames_Treatment_` = 1); sync_code(session)
    session$setInputs(`FO-create_formula` = 1)
    ex[[3]] <<- session$userData$export[[3]]@summary
    session$userData$export_formula_rhs<- ""; sync_code(session)

  })
   expect_equal(ex, expected)
}
test_linear_formula(app, srv)

test_glm_formula <- function(app, srv) {
  options(OpenStats.background = FALSE)
  expected <- list(
    broom::tidy(glm(uptake ~ conc + Treatment, data = CO2, family = "Gamma")),
    broom::tidy(glm(uptake ~ conc + Treatment, data = CO2, family = stats::gaussian()))
  )
  ex <- list()
  shiny::testServer(srv, {
    DataModelState$df <- CO2

    session$setInputs(`FO-model_type` = "Generalised Linear Model")
    session$setInputs(`FO-Family` = "Gamma")
    session$setInputs(`FO-Link_function` = "inverse")
    session$setInputs(`FO-colnames-dropdown_` = "uptake");
    session$setInputs(`FO-colnames_conc_` = 1); sync_code(session)
    session$setInputs(`FO-add` = 1); sync_code(session)
    session$setInputs(`FO-colnames_Treatment_` = 1); sync_code(session)
    session$setInputs(`FO-create_formula` = 1)
    ex[[1]] <<- session$userData$export[[1]]@summary
    session$userData$export_formula_rhs<- ""; sync_code(session)

    session$setInputs(`FO-model_type` = "Generalised Linear Model")
    session$setInputs(`FO-Family` = stats::gaussian())
    session$setInputs(`FO-Link_function` = "identity")
    session$setInputs(`FO-colnames-dropdown_` = "uptake");
    session$setInputs(`FO-colnames_conc_` = 1); sync_code(session)
    session$setInputs(`FO-add` = 1); sync_code(session)
    session$setInputs(`FO-colnames_Treatment_` = 1); sync_code(session)
    session$setInputs(`FO-create_formula` = 1)
    ex[[2]] <<- session$userData$export[[2]]@summary
    session$userData$export_formula_rhs<- ""; sync_code(session)
  })
   expect_equal(ex, expected)
}
test_glm_formula(app, srv)
