env_import_export_dose_response_V1_2 <- new.env(parent = getNamespace("OpenStats"))

extract_sample_name <- function(x) {
  # The short_label represents the sample name.
  # If the short_label is CU1-2-1, then the mother sample is CU1-2.
  # The format follows {name_abbr}-{number}.
  # ==> verything until the second -.
  sub("-$", "", gsub("^(([^-]*-){2}).*$", "\\1", x))
}
env_import_export_dose_response_V1_2$extract_sample_name <- extract_sample_name

read_well <- function(well) {
  nrow <- length(well$readouts)
  res <- data.frame(
    conc = rep(NA, nrow),
    name = rep(NA, nrow),
    values = rep(NA, nrow)
  )
  res$values <- sapply(well$readouts, function(read) {
    read$value |> as.numeric()
  })
  res$conc <- well$sample$conc
  res$name <- well$sample$short_label |> env_import_export_dose_response_V1_2$extract_sample_name()
  res$readout_id <- seq_len(nrow)
  res$well_id <- well$id
  res$sample_id <- well$sample$id
  res <- res[!is.na(res$values), ]
  return(res)
}
env_import_export_dose_response_V1_2$read_well <- read_well

read_plate <- function(data) {
  stopifnot("Found an empty wellplate cannot import the data" = length(data$wells) >= 1L)
  wells <- lapply(data$wells, env_import_export_dose_response_V1_2$read_well)
  all_wells <- Reduce(rbind, wells) |> as.data.frame()
  all_wells$wellplate_id <- data$id
  all_wells
}
env_import_export_dose_response_V1_2$read_plate <- read_plate

import_dose_response_json <- function(path) {
  data <- jsonlite::read_json(path)

  stopifnot("id is NULL cannot import the data" = !is.null(data$id))
  stopifnot("request_id is NULL cannot import the data" = !is.null(data$request_id))
  stopifnot("wellplates is NULL cannot import the data" = !is.null(data$wellplates))
  n <- length(data$wellplates)
  stopifnot("The number of wellplates seems to be 0" = n >= 1L)

  df <- Map(env_import_export_dose_response_V1_2$read_plate, data$wellplates)
  df <- Reduce(rbind, df) |> as.data.frame()
  row.names(df) <- NULL
  splitted_df <- split(df, df$readout_id)
  splitted_df <- setNames(splitted_df, paste0("Readout_", seq_len(length(splitted_df))))
  list(
    id = data$id, request_id = data$request_id,
    data = splitted_df
  )
}
env_import_export_dose_response_V1_2$import_dose_response_json <- import_dose_response_json

dose_response_to_json <- function(id, request_id, data_sets, results, path) {
  stopifnot("Lengths do not match between data_sets and results" = length(data_sets) == length(results))
  input_result_pairs <- Map(function(inp, res) {
    list(input = inp, result = res)
  }, data_sets, results)
  output <- list(id = id, request_id = request_id, Output = input_result_pairs)
  output_json <- jsonlite::toJSON(output, pretty = TRUE)
  writeLines(output_json, con = path)
}
env_import_export_dose_response_V1_2$dose_response_to_json <- dose_response_to_json 

path <- "./development/json_from_eln.json"
imported <- env_import_export_dose_response_V1_2$import_dose_response_json(path)
imported

df <- read.csv("./test_data/DoseResponse.csv")
df <- df[, c(3L, 2L, 1L)]
names(df) <- c("conc", "name", "values")
df$readout_id <- 1
df$well_id <- sample(c(1, 2, 3), nrow(df), replace = TRUE)
df$sample_id <- sample(c(1, 2, 3), nrow(df), replace = TRUE)
df$wellplate_id <- sample(c(1, 2, 3), nrow(df), replace = TRUE)
head(df)
res <- OpenStats:::ic50(df, "values", "conc", "name", islog_x = TRUE, FALSE)
res_df <- lapply(res, function(x) {
  if (inherits(x, "errorClass")) {
    return(NULL)
  }
  return(x[[1]])
})
res_df <- res_df[!is.null(res_df)]
res_df <- res_df[!sapply(res_df, is.null)]
res_df <- Reduce(rbind, res_df)
env_import_export_dose_response_V1_2$dose_response_to_json(
  imported$id, imported$request_id, list(df), list(res_df),
  "./development/result2.json")
