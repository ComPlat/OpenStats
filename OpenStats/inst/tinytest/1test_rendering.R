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
  tinytest::expect_true(all(checks))
}
