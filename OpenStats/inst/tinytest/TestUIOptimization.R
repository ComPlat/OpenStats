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
    file = system.file("/test_data/calibration.csv", package = "OpenStats")
  )
  wait(app)
  app
}

create_formula <- function(app, model, lhs = "response", custom_formula = NULL) {
  app$click("open_formula_editor")
  wait(app)
  app$set_inputs(`FO-model_type` = "Optimization Model")
  wait(app)
  app$set_inputs(`FO-PredefinedModels` = model)
  wait(app)
  if (model == "free") {
    app$set_inputs(`FO-PredefinedModels` = "free")
    wait(app)
    app$set_inputs(`FO-editable_code` = custom_formula)
    wait(app)
    app$set_inputs(`FO-colnames-dropdown_` = lhs)
    wait(app)
  } else if (model == "linear") {
    app$set_inputs(`FO-PredefinedModels` = model)
    wait(app)
    app$set_inputs(`FO-linear_lhs_var` = lhs)
    wait(app)
  } else if (model == "log_linear") {
    app$set_inputs(`FO-PredefinedModels` = model)
    wait(app)
    app$set_inputs(`FO-log_linear_lhs_var` = lhs)
    wait(app)
  } else if (model == "michaelis_menten") {
    app$set_inputs(`FO-PredefinedModels` = model)
    wait(app)
    app$set_inputs(`FO-mm_lhs_var` = lhs)
    wait(app)
  } else if (model == "one_site_binding") {
    app$set_inputs(`FO-PredefinedModels` = model)
    wait(app)
    app$set_inputs(`FO-binding_lhs_var` = lhs)
    wait(app)
  } else if (model == "two_hot_binding") {
    app$set_inputs(`FO-PredefinedModels` = model)
    wait(app)
    app$set_inputs(`FO-hotbind_lhs_var` = lhs)
    wait(app)
  }
  app$click("FO-create_formula")
  wait(app)
  app$run_js("$('.modal-footer button:contains(\"Close\")').click();")
  wait(app)
}

test_output <- function(app) {
  res <- app$get_values()$export$`FO-result_list`
  checks <- list(expect_true(is.data.frame(res[[1]])))
  checks[[2]] <- expect_inherits(res[[2]], "summaryModel")
  expect_true(all(unlist(checks)))
}

models <- c(
  "linear",
  "log_linear",
  "michaelis_menten",
  "one_site_binding",
  "two_hot_binding",
  "free"
)

for (model in models) {
  print(model)
  app <- upload_file()
  if (model == "free") {
    custom_formula <- "a + b * conc"
    create_formula(app, model, lhs = "response", custom_formula = custom_formula)
  } else {
    create_formula(app, model, lhs = "response")
  }
  res <- test_output(app)
  print(res)
  app$stop()
}
