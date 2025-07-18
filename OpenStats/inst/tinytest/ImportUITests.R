library(shinytest2)
library(tinytest)
wait <- function(app) {
  try(app$wait_for_idle(), silent = TRUE)
}

upload_file <- function() {
  app <- OpenStats:::app()
  app <- shiny::shinyApp(app$ui, app$server)
  app <- AppDriver$new(app)
  wait(app)
  app$upload_file(
    file = system.file("/test_data/MultiTable4.csv", package = "OpenStats")
  )
  wait(app)
  app
}
app <- upload_file()

# required in order to get the exported results list
do_something <- function(app) {
  app$click("open_formula_editor")
  wait(app)
  app$set_inputs(`FO-colnames-dropdown_` = "a")
  wait(app)
  app$click("FO-colnames_b_")
  wait(app)
  app$click("FO-create_formula")
  wait(app)
  app$run_js("$('.modal-footer button:contains(\"Close\")').click();")
  wait(app)
  results <- app$get_values()$export
  names <- app$get_values()$input |> names()
  names <- names[grepl("FO-colnames_", names)]
  list(results = results, names = names)
}
res <- do_something(app)
expect_equal(length(res$results$`FO-result_list`), 4)
expected_names <- c("FO-colnames_a_", "FO-colnames_b_", "FO-colnames_c_")
expect_equal(expected_names, res$names)

switch_table <- function(app) {
  app$set_inputs(`tables-dropdown` = "df1")
  wait(app)
  app$click("open_formula_editor")
  wait(app)
  app$run_js("$('.modal-footer button:contains(\"Close\")').click();")
  wait(app)
  names <- app$get_values()$input |> names()
  names <- names[grepl("FO-colnames_", names)]
  names
}
names <- switch_table(app)
expected_names <- c("FO-colnames_a_", "FO-colnames_b_", "FO-colnames_c_", "FO-colnames_d_", "FO-colnames_e_", "FO-colnames_f_")
expect_equal(names, expected_names)
wait(app)

app$stop()
