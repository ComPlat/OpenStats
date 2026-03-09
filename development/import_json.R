files <- list.files("./OpenStats/R", full.names = TRUE)
trash <- lapply(files, source)
file <- "./development/dose_response_input.json"

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

options(OpenStats.background = TRUE)
ib <- getOption("OpenStats.background", TRUE)
df <- DataModelState$df
DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(df)
formula <- values ~ conc
formula <- new("LinearFormula", formula = formula)
ResultsState <- OpenStats:::backend_result_state_V1_2$new(list(df))
ResultsState$bgp$in_backend <- TRUE
dr <- OpenStats:::dose_response_V1_2$new(
  df = df,
  is_xlog = FALSE,
  is_ylog = FALSE,
  substance_names = "name",
  formula = formula,
  com = OpenStats:::backend_communicator_V1_2
)
new_name <- "Mock1"
dr$eval(ResultsState, new_name)
if(ib) OpenStats:::backend_get_result_V1_2(ResultsState)
new_name <- "Mock2"
dr$eval(ResultsState, new_name)
if(ib) OpenStats:::backend_get_result_V1_2(ResultsState)
res <- ResultsState$all_data[[length(ResultsState$all_data)]]

MethodState <- new.env(parent = emptyenv())
MethodState$storage_class <- new("MethodDoseResponse", id = "1", request_id = "2")

dose_response_to_json <- function(MethodState, all_data) {
  Output <- list()

  for (k in seq_along(all_data)) {
    elem <- all_data[[k]]

    if (!inherits(elem, "doseResponse")) next

    inp <- split(elem@input_df, elem@input_df$name)
    res <- split(elem@df, elem@df$name)
    u <- union(names(inp), names(res))

    items <- vector("list", length(u))
    for (j in seq_along(u)) {
      nm <- u[[j]]
      items[[j]] <- list(
        id = as.character(j),
        input = inp[[nm]],
        result = res[[nm]]
      )
    }

    Output[[length(Output) + 1L]] <- list(
      id = as.character(length(Output)),
      items = items
    )
  }

  out <- list(
    id = as.character(MethodState$storage_class@id),
    request_id = as.character(MethodState$storage_class@request_id),
    Output = Output
  )

  jsonlite::toJSON(out, pretty = TRUE, auto_unbox = TRUE, null = "null")
}

res <- dose_response_to_json(MethodState, ResultsState$all_data)
writeLines(res, con = "./development/output.json")
