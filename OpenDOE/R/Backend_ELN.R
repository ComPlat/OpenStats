# Query parameters set by the ELN when it opens OpenDOE (?url=...&method=...&mode=...):
# method = "DesignOfExperiments": url points to a reaction's DoE JSON
#   (doe_definition, scheme samples, properties, variations).
# mode = "initial":   first batch, no measurements yet -> predictors from doe_definition, LHS
# mode = "follow_up": variations with recorded results exist -> next batch via BO
ELN_DOE_METHOD <- "DesignOfExperiments"
ELN_DOE_MODES <- c("initial", "follow_up")
ELN_NONE_LEVEL <- "none"

eln_label_field <- function(sample, key) {
  value <- sample[[key]]
  if (is.null(value) || !nzchar(value)) return(NA_character_)
  as.character(value)
}

# The ELN gives every reactant/solvent the short label "reactant"/"solvent",
# so short labels are only used when they are unique within the factor.
eln_level_labels <- function(samples) {
  ids <- vapply(samples, function(s) as.character(s$id), character(1))
  short <- vapply(samples, eln_label_field, character(1), key = "shortLabel")
  name <- vapply(samples, eln_label_field, character(1), key = "name")
  name[is.na(name)] <- paste("sample", ids[is.na(name)])

  labels <- short
  ambiguous <- is.na(short) | short %in% short[duplicated(short)]
  labels[ambiguous] <- name[ambiguous]
  clash <- labels %in% labels[duplicated(labels)]
  labels[clash] <- paste0(labels[clash], " (", ids[clash], ")")
  labels
}

# Converts the ELN DoE JSON (parsed with simplifyVector = FALSE) into OpenDOE predictors.
# level_ids keeps the sample id behind every material_selection level for the way back to the ELN.
eln_doe_predictors <- function(doe) {
  definition <- doe$doe_definition
  if (is.null(definition) || length(definition$factors) == 0L) stop("The reaction has no design of experiments defined")
  if (!identical(as.integer(definition$version), 1L)) stop("Unsupported doe_definition version: ", definition$version)

  samples_by_id <- list()
  for (s in doe$samples) samples_by_id[[as.character(s$id)]] <- s

  predictors <- list()
  predictor_types <- character(0)
  level_ids <- list()

  for (f in definition$factors) {
    name <- f$name
    if (is.null(name) || !nzchar(name)) stop("Factor without name")
    if (name %in% names(predictors)) stop("Duplicated factor name: ", name)
    kind <- f$target$kind

    if (identical(kind, "material_selection")) {
      ids <- vapply(f$target$sample_ids, as.character, character(1))
      unknown <- setdiff(ids, names(samples_by_id))
      if (length(unknown) > 0L) stop("Factor ", name, ": samples not in the reaction: ", paste(unknown, collapse = ", "))
      levels <- unname(eln_level_labels(samples_by_id[ids]))
      level_id <- stats::setNames(as.integer(ids), levels)
      if (isTRUE(f$target$allow_none)) {
        levels <- c(levels, ELN_NONE_LEVEL)
        level_id <- c(level_id, stats::setNames(NA_integer_, ELN_NONE_LEVEL))
      }
      if (any_duplicates(levels)) stop("Factor ", name, ": samples share the same label")
      type <- "categorical"
      level_ids[[name]] <- level_id
    } else if (kind %in% c("material_amount", "property")) {
      levels <- as.character(c(f$min, f$max))
      type <- "continuous"
    } else {
      stop("Factor ", name, ": unknown kind ", kind)
    }

    msg <- validate_predictor(levels, type)
    if (!is.null(msg)) stop("Factor ", name, ": ", msg)
    predictors[[name]] <- levels
    predictor_types[[name]] <- type
  }

  batch_size <- definition$initial_batch_size
  if (is.null(batch_size) || batch_size < 1) batch_size <- 4L * length(predictors)

  list(
    predictors = predictors,
    predictor_types = predictor_types,
    eln = list(
      id = doe$id, request_id = doe$request_id, mode = doe$mode,
      batch_size = as.integer(batch_size), factors = definition$factors, level_ids = level_ids
    )
  )
}

import_eln_doe <- function(State, doe) {
  conv <- eln_doe_predictors(doe)
  State$predictors <- conv$predictors
  State$predictor_types <- conv$predictor_types
  State$eln <- conv$eln
  invisible(conv)
}

# Rows for the ELN: factor name -> sample id (material_selection, "none" -> null) or number.
eln_doe_rows <- function(df, eln) {
  factor_names <- vapply(eln$factors, function(f) f$name, character(1))
  missing_cols <- setdiff(factor_names, names(df))
  if (length(missing_cols) > 0L) stop("Missing factor columns: ", paste(missing_cols, collapse = ", "))

  columns <- lapply(eln$factors, function(f) {
    values <- df[[f$name]]
    if (identical(f$target$kind, "material_selection")) {
      level_id <- eln$level_ids[[f$name]]
      unknown <- setdiff(unique(as.character(values)), names(level_id))
      if (length(unknown) > 0L) stop("Factor ", f$name, ": unknown levels ", paste(unknown, collapse = ", "))
      return(unname(level_id[as.character(values)]))
    }
    values <- suppressWarnings(as.numeric(values))
    if (anyNA(values)) stop("Factor ", f$name, ": values must be numeric")
    values
  })
  names(columns) <- factor_names

  lapply(seq_len(nrow(df)), function(i) {
    row <- lapply(columns, function(col) if (is.na(col[[i]])) NULL else col[[i]])
    names(row) <- factor_names
    row
  })
}

write_eln_doe_batch <- function(df, eln, path) {
  batch <- list(id = eln$id, request_id = eln$request_id, mode = eln$mode, rows = eln_doe_rows(df, eln))
  writeLines(jsonlite::toJSON(batch, auto_unbox = TRUE, null = "null", digits = NA), path)
  invisible(path)
}

send_eln_doe_batch <- function(session, df, eln) {
  if (!requireNamespace("COMELN", quietly = TRUE)) stop("Sending to the ELN requires the 'COMELN' package")
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  write_eln_doe_batch(df, eln, path)
  uploader <- getExportedValue("COMELN", "upload")
  uploader(session, path, "opendoe_batch.json")
}

fetch_eln_doe <- function(session) {
  if (!requireNamespace("COMELN", quietly = TRUE)) {
    stop("Importing from the ELN requires the 'COMELN' package: remotes::install_github(\"ComPlat/OpenStats\", subdir = \"comeln\")")
  }
  downloader <- getExportedValue("COMELN", "download")
  file <- downloader(session, tempdir())
  on.exit(unlink(file))
  jsonlite::fromJSON(file, simplifyVector = FALSE)
}
