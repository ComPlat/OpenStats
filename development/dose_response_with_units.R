files <- list.files("./OpenStats/R", full.names = TRUE)
trash <- lapply(files, source)

ResultsState <- OpenStats:::backend_result_state_V1_2$new(list())
ResultsState$bgp$in_backend <- TRUE
DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(CO2)
backend_method_state <- R6::R6Class(
  "backend_method_state",
  list(
    method = "Default",
    storage_class = new("MethodDoseResponse", id = "", request_id = ""),
    initialize = function() {}
  )
)
MethodState <- backend_method_state$new()
path <- "./development/dose_response_input.json"
import_dose_response_json(path, DataModelState, ResultsState, MethodState)

library(ggplot2)
res <- ic50(DataModelState$df, 10, "values", "conc", "name", "unit", FALSE, FALSE)
df <- lapply(res, function(obj) {
  obj[[1L]]
})
df <- Reduce(rbind, df)
df
