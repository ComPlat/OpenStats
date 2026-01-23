files <- list.files("./OpenStats/R", full.names = TRUE)
trash <- lapply(files, source)
file <- "./development/ChemotionToOpenStats - example.json"

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
OpenStats:::env_import_export_dose_response_V1_2$import_dose_response_json(file, DataModelState, ResultsState, MethodState)
DataModelState$df
MethodState$storage_class
ResultsState$all_data
.traceback()
