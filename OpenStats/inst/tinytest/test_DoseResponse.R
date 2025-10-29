if (!identical(Sys.getenv("NOT_CRAN"), "true")) exit_file("Skip on CRAN")
if (!identical(Sys.getenv("RUN_UI_TESTS"), "true")) exit_file("UI tests disabled")

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
  file = system.file("/test_data/DoseResponse.csv", package = "OpenStats")
)
wait(app)
app$set_inputs(conditionedPanels = "Dose Response analysis")
wait(app)

# Define formula
app$click("open_formula_editor")
wait(app)
app$set_inputs(`FO-colnames-dropdown_` = "abs")
wait(app)
app$set_inputs(`FO-editable_code` = "conc")
wait(app)
app$click("FO-create_formula")
wait(app)
app$run_js("$('.modal-footer button:contains(\"Close\")').click();")
wait(app)

app$set_inputs(`DOSERESPONSE-substanceNames` = "names")
wait(app)
app$click("DOSERESPONSE-ic50")
wait(app)
Sys.sleep(20)

res <- app$get_values()$export
res <- res[["FO-result_list"]]
res_df <- res[[length(res)]]@df

data <- read.csv(system.file("/test_data/DoseResponse.csv", package = "OpenStats"))
expected <- OpenStats:::ic50(
  data, "abs", "conc",
  "names",
  FALSE, FALSE
)
dfs <- lapply(expected, function(x) {
  if (is.list(x)) {
    return(x[[1]])
  }
})
expected <- do.call(rbind, dfs)
equal <- Map(function(a, b) {
  a <- a[!is.na(a)]
  b <- b[!is.na(b)]
  all(a == b)
}, res_df, expected) |> unlist() |> all()
expect_true(equal)


app$stop()
