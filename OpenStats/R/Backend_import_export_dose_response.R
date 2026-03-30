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
  if (nrow == 0) {
    res <- data.frame(
      conc = NA, name = NA, values = NA,
      readout_id = NA, well_id = NA, sample_id = NA
    )
    res <- res[!is.na(res$values), ]
    return(res)
  }
  res <- data.frame(
    conc = rep(NA, nrow),
    name = rep(NA, nrow),
    values = rep(NA, nrow),
    unit = rep(NA, nrow)
  )
  res$values <- sapply(well$readouts, function(read) {
    read$value |> as.numeric()
  })
  res$unit <- sapply(well$readouts, function(read) {
    read$unit |> as.character()
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

import_dose_response_json <- function(path, DataModelState, ResultsState, MethodState) {
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
  MethodState$storage_class@id <- data$id
  MethodState$storage_class@request_id <- data$request_id
  DataModelState$df <- splitted_df[[1L]]
  if (length(splitted_df) >= 1) {
    lapply(splitted_df, function(t) {
      name <- paste0("df", ResultsState$counter)
      ResultsState$all_data[[name]] <- t
      ResultsState$counter <- ResultsState$counter + 1
    })
  }
  if (nrow(DataModelState$df) == 0) {
    stop("The uploaded file is empty. Please upload a file with data.")
  }
  max_cols <- 1000
  max_rows <- 1e6
  if (nrow(DataModelState$df) > max_rows || ncol(DataModelState$df) > max_cols) {
    stop(sprintf(
      "Data exceeds the limit of %d rows or %d columns. Please upload a smaller dataset.",
      max_rows, max_cols
    ))
  }
  tapply(DataModelState$df, DataModelState$df$name, function(block) {
    units <- unique(block$unit)
    if (length(units) != 1L) {
      stop(sprintf("Found for sample %s multiple units: %s",
        unique(block$name), paste(units, collapse = ", ")
      ))
    }
  })
  invisible(NULL)
}
env_import_export_dose_response_V1_2$import_dose_response_json <- import_dose_response_json

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

  output_json <- jsonlite::toJSON(out, pretty = TRUE, auto_unbox = TRUE, null = "null")
  path <- tempfile(fileext = ".json")
  writeLines(output_json, con = path)
  return(path)
}
env_import_export_dose_response_V1_2$dose_response_to_json <- dose_response_to_json
