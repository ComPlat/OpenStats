# Query parameters set by the ELN when it opens OpenDOE (?url=...&method=...&mode=...):
# method = "DesignOfExperiments": url points to a reaction's DoE JSON
#   (doe_definition, scheme samples, properties, variations).
# mode = "initial":   first batch, no measurements yet -> predictors from doe_definition, LHS
# mode = "follow_up": variations with recorded results exist -> next batch via BO
ELN_DOE_METHOD <- "DesignOfExperiments"
ELN_DOE_MODES <- c("initial", "follow_up")
ELN_NONE_LEVEL <- "none"

# The ELN stores variations in fixed standard units (degC, Second(s), g, mol, l);
# other units only exist in its display.
ELN_PROPERTY_UNITS <- c(temperature = "°C", duration = "Hour(s)")
ELN_DURATION_SECONDS <- c("Second(s)" = 1, "Minute(s)" = 60, "Hour(s)" = 3600, "Day(s)" = 86400, "Week(s)" = 604800)
ELN_MATERIAL_STANDARD_UNITS <- c(mass = "g", amount = "mol", volume = "l")
ELN_MATERIAL_UNIT_FACTORS <- c(
  g = 1, mg = 1e-3, "μg" = 1e-6,
  mol = 1, mmol = 1e-3,
  l = 1, ml = 1e-3, "μl" = 1e-6
)
ELN_MATERIAL_GROUPS <- c("startingMaterials", "reactants", "solvents", "products")
# entry in the variations table -> column label; the last three only exist for gas products
ELN_RESPONSE_ENTRIES <- c(
  yield = "yield", mass = "mass", amount = "amount", volume = "volume",
  turnoverNumber = "TON", turnoverFrequency = "TOF", concentration = "concentration"
)
ELN_PRESENT_LEVELS <- c("yes", "no")

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

# Same naming as doePresentPredictorName in the ELN modal.
eln_present_predictor_name <- function(factor) paste(factor$name, "present")

eln_allows_absent <- function(factor) {
  identical(factor$target$kind, "material_amount") && isTRUE(factor$target$allow_absent)
}

# Names of all predictors a factor turns into (material_amount that may be absent gives two).
eln_factor_predictor_names <- function(factor) {
  if (eln_allows_absent(factor)) return(c(eln_present_predictor_name(factor), factor$name))
  factor$name
}

eln_range_levels <- function(range) as.character(c(range$min, range$max))

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
  add_predictor <- function(name, levels, type) {
    if (name %in% names(predictors)) stop("Duplicated factor name: ", name)
    msg <- validate_predictor(levels, type)
    if (!is.null(msg)) stop("Factor ", name, ": ", msg)
    predictors[[name]] <<- levels
    predictor_types[[name]] <<- type
  }

  for (f in definition$factors) {
    name <- f$name
    if (is.null(name) || !nzchar(name)) stop("Factor without name")
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
      add_predictor(name, levels, "categorical")
      level_ids[[name]] <- level_id
    } else if (kind %in% c("material_amount", "property")) {
      if (eln_allows_absent(f)) add_predictor(eln_present_predictor_name(f), ELN_PRESENT_LEVELS, "categorical")
      add_predictor(name, eln_range_levels(f), "continuous")
    } else {
      stop("Factor ", name, ": unknown kind ", kind)
    }
  }

  batch_size <- definition$initial_batch_size
  if (is.null(batch_size) || batch_size < 1) batch_size <- 4L * length(predictors)
  replicates <- definition$replicates
  if (is.null(replicates) || replicates < 1) replicates <- 1L

  list(
    predictors = predictors,
    predictor_types = predictor_types,
    eln = list(
      id = doe$id, request_id = doe$request_id, mode = doe$mode,
      batch_size = as.integer(batch_size), replicates = as.integer(replicates),
      factors = definition$factors, level_ids = level_ids
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

eln_convert_property <- function(value, from, to) {
  if (is.na(value) || identical(from, to)) return(value)
  if (identical(from, "Second(s)") && to %in% names(ELN_DURATION_SECONDS)) {
    return(value / ELN_DURATION_SECONDS[[to]])
  }
  stop("Unexpected unit from the ELN: ", from, " (expected ", to, " or Second(s))")
}

eln_entry_value <- function(material, entry) {
  value <- material[[entry]]$value
  if (is.null(value)) NA_real_ else as.numeric(value)
}

# Material entry in the unit of the doe_definition (equivalent has no unit).
eln_material_entry <- function(material, entry, unit) {
  value <- eln_entry_value(material, entry)
  if (is.na(value) || entry == "equivalent" || is.null(unit) || !nzchar(unit)) return(value)
  stored <- material[[entry]]$unit
  if (!is.null(stored) && !identical(stored, ELN_MATERIAL_STANDARD_UNITS[[entry]])) {
    stop("Unexpected unit from the ELN: ", stored, " (expected ", ELN_MATERIAL_STANDARD_UNITS[[entry]], ")")
  }
  if (!(unit %in% names(ELN_MATERIAL_UNIT_FACTORS))) stop("Unknown unit: ", unit)
  value / ELN_MATERIAL_UNIT_FACTORS[[unit]]
}

eln_variation_material <- function(row, sample_id) {
  for (group in ELN_MATERIAL_GROUPS) {
    material <- row[[group]][[as.character(sample_id)]]
    if (!is.null(material)) return(material)
  }
  NULL
}

# Amount, mass or volume, whatever the material type carries (solvents only have a volume).
eln_material_quantity <- function(material) {
  if (is.null(material)) return(NA_real_)
  for (entry in c("amount", "mass", "volume")) {
    value <- eln_entry_value(material, entry)
    if (!is.na(value)) return(value)
  }
  NA_real_
}

eln_variation_level <- function(row, factor, level_id) {
  ids <- unname(level_id[!is.na(level_id)])
  active <- vapply(ids, function(id) {
    quantity <- eln_material_quantity(eln_variation_material(row, id))
    !is.na(quantity) && quantity > 0
  }, logical(1))
  if (sum(active) == 1L) return(names(level_id)[match(ids[active], level_id)])
  if (sum(active) == 0L && isTRUE(factor$target$allow_none)) return(ELN_NONE_LEVEL)
  NA_character_
}

# Predictor values of one factor in one variation row (named list, see eln_factor_predictor_names).
eln_variation_factor_values <- function(row, factor, level_ids) {
  target <- factor$target
  value <- switch(target$kind,
    material_selection = eln_variation_level(row, factor, level_ids[[factor$name]]),
    material_amount = {
      material <- eln_variation_material(row, target$sample_id)
      entry <- if (is.null(target$entry)) "equivalent" else target$entry
      amount <- eln_material_entry(material, entry, target$unit)
      if (!eln_allows_absent(factor)) {
        amount
      } else {
        quantity <- eln_material_quantity(material)
        present <- !is.na(quantity) && quantity > 0
        # an absent sample has no amount; the GP still needs a number, the lower bound is the closest meaning
        list(ELN_PRESENT_LEVELS[[if (present) 1L else 2L]], if (present) amount else as.numeric(factor$min))
      }
    },
    property = {
      prop <- row$properties[[target$field]]
      to <- if (is.null(target$unit)) ELN_PROPERTY_UNITS[[target$field]] else target$unit
      if (is.null(prop)) NA_real_ else eln_convert_property(eln_entry_value(row$properties, target$field), prop$unit, to)
    },
    stop("Unknown factor kind: ", target$kind)
  )
  values <- if (is.list(value)) value else list(value)
  names(values) <- eln_factor_predictor_names(factor)
  values
}

eln_product_labels <- function(variations) {
  products <- list()
  for (row in variations) {
    for (id in names(row$products)) {
      if (!is.null(products[[id]])) next
      aux <- row$products[[id]]$aux
      label <- eln_label_field(aux, "shortLabel")
      if (is.na(label)) label <- eln_label_field(aux, "name")
      if (is.na(label)) label <- paste("product", id)
      products[[id]] <- label
    }
  }
  unlist(products)
}

# ELN variations -> one row per experiment: predictor columns (levels/values as in the predictors)
# plus "<product> <entry>" response columns (columns without any value are dropped).
# Rows whose material_selection is ambiguous are dropped and reported.
eln_doe_variations_df <- function(variations, eln) {
  predictor_names <- unlist(lapply(eln$factors, eln_factor_predictor_names))
  products <- eln_product_labels(variations)
  product_ids <- if (is.null(products)) character(0) else names(products)
  response_ids <- expand.grid(entry = names(ELN_RESPONSE_ENTRIES), id = product_ids, stringsAsFactors = FALSE)
  response_names <- make.unique(paste(products[response_ids$id], ELN_RESPONSE_ENTRIES[response_ids$entry]))

  rows <- lapply(variations, function(row) {
    values <- unlist(lapply(eln$factors, eln_variation_factor_values, row = row, level_ids = eln$level_ids), recursive = FALSE)
    responses <- lapply(seq_len(nrow(response_ids)), function(i) {
      product <- row$products[[response_ids$id[[i]]]]
      if (is.null(product)) NA_real_ else eln_entry_value(product, response_ids$entry[[i]])
    })
    names(responses) <- response_names
    c(values, responses)
  })

  selection_names <- vapply(
    Filter(function(f) f$target$kind == "material_selection", eln$factors), function(f) f$name, character(1)
  )
  invalid <- vapply(rows, function(r) any(is.na(unlist(r[selection_names]))), logical(1))
  invalid_ids <- vapply(variations[invalid], function(v) as.character(v$id), character(1))

  columns <- c(predictor_names, response_names)
  df <- as.data.frame(
    stats::setNames(lapply(columns, function(col) {
      unlist(lapply(rows[!invalid], function(r) r[[col]]))
    }), columns),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (nrow(df) == 0L) {
    df <- as.data.frame(stats::setNames(rep(list(logical(0)), length(columns)), columns), check.names = FALSE)
  }
  keep <- c(predictor_names, response_names[vapply(response_names, function(col) !all(is.na(df[[col]])), logical(1))])
  list(df = df[keep], invalid_ids = invalid_ids)
}

# Rows for the ELN: factor name -> sample id (material_selection, "none" -> null) or number
# (material_amount that is absent -> null).
eln_doe_rows <- function(df, eln) {
  predictor_names <- unlist(lapply(eln$factors, eln_factor_predictor_names))
  missing_cols <- setdiff(predictor_names, names(df))
  if (length(missing_cols) > 0L) stop("Missing factor columns: ", paste(missing_cols, collapse = ", "))

  numeric_column <- function(name) {
    values <- suppressWarnings(as.numeric(df[[name]]))
    if (anyNA(values)) stop("Factor ", name, ": values must be numeric")
    values
  }

  columns <- lapply(eln$factors, function(f) {
    if (!identical(f$target$kind, "material_selection")) {
      values <- numeric_column(f$name)
      if (eln_allows_absent(f)) {
        present <- as.character(df[[eln_present_predictor_name(f)]])
        unknown <- setdiff(unique(present), ELN_PRESENT_LEVELS)
        if (length(unknown) > 0L) stop("Factor ", f$name, ": unknown levels ", paste(unknown, collapse = ", "))
        values[present == "no"] <- NA_real_
      }
      return(values)
    }
    values <- as.character(df[[f$name]])
    level_id <- eln$level_ids[[f$name]]
    unknown <- setdiff(unique(values), names(level_id))
    if (length(unknown) > 0L) stop("Factor ", f$name, ": unknown levels ", paste(unknown, collapse = ", "))
    unname(level_id[values])
  })
  factor_names <- vapply(eln$factors, function(f) f$name, character(1))

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
