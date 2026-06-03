env_import_export_variations_V1_2 <- new.env(parent = getNamespace("OpenStats"))

read_variations <- function(path, DataModelState, ResultsState) {
  data <- jsonlite::read_json(path)

  resolve_column_order <- function(column_order) {
    lapply(column_order, \(c) {
      strsplit(c, "\\.")[[1L]]
    })
  }

  sanitize <- function(obj) {
    if (is.null(obj) || length(obj) == 0L || is.na(obj)) return(NA_character_)
    obj <- as.character(obj)
    obj <- gsub("[\u00B5\u00B5]", "u", obj)
    obj <- gsub("-", "", obj)
    obj <- gsub("[^A-Za-z0-9]+", "_", obj)
    obj <- gsub("^_+|_+$", "", obj)
    if (!nzchar(obj)) "obj" else obj
  }

  cast_unit <- function(unit) {
    if (is.null(unit) || length(unit) == 0L || is.na(unit) || !nzchar(unit)) {
      return(NA_character_)
    }
    units <- list(
      "g" = "g", "mg" = "mg", "\u00B5g" = "ug",
      "mol" = "mol", "mmol" = "mmol",
      "l" = "l", "ml" = "ml", "\u00B5l" = "ul",
      "Second(s)" = "s", "Minute(s)" = "min", "Hour(s)" = "h",
      "Day(s)" = "d", "Week(s)" = "wk",
      "K" = "K", "\u00B0C" = "degC", "\u00B0F" = "degF",
      "ppm" = "ppm", "%" = "pct",
      "turnoverNumber" = "turnover_num",
      "turnoverFrequency" = "turnover_freq"
    )
    if (unit %in% names(units)) unname(units[[unit]]) else sanitize(unit)                                                                                                                                                                       
  }

  read_meta_data <- function(entry, elements) {
    # analyses can alos be a metadata. This cannot be handled here
    entry <- entry[[elements[1]]]
    if (elements[2] == "group") {
      data.frame(
        group = entry$group$group,
        subgroup = entry$group$subgroup
      )
    } else if (elements[2] == "notes") {
      data.frame(
        notes = entry$notes
      )
    }
  }

  read_properties <- function(entry, elements) {
    entry <- entry[[elements[1]]]
    entry <- entry[[elements[2]]]
    val <- if (is.null(entry$value)) NA else entry$value
    unit <- if (is.null(entry$unit)) NA else entry$unit
    res <- data.frame(value = val)
    names(res)[1L] <- paste0(elements[2], "_", cast_unit(unit))
    res
  }

  read_sample <- function(entry, elements) {
    if (length(entry) == 0L) return(NULL)
    entry <- entry[[elements[1]]]
    o <- entry
    entry <- entry[[elements[2]]]
    name <- entry$aux$name |> sanitize()
    name <- paste0(name, "_", sanitize(entry$aux$shortLabel))
    entry <- entry[[elements[3]]]
    val <- if (is.null(entry$value)) NA else entry$value
    unit <- NA
    unit <- if (!is.null(entry$unit)) {
      unit <- entry$unit
    } else if (elements[3] %in% c("turnoverNumber", "turnoverFrequency", "equivalent")) {
      unit <- elements[3] |> cast_unit()
    }
    res <- data.frame(value = val)
    names(res) <- paste0(name, "_", cast_unit(unit))
    res
  }

  names_trees <- resolve_column_order(data$columnOrder)

  read_single_variation <- function(variation) {
    res <- lapply(seq_len(length(names_trees)), \(i) {
      tree <- names_trees[[i]]
      if(tree[1] %in% c("reactants", "products", "solvents", "startingMaterials")) {
        read_sample(variation, tree)
      } else if (tree[1] == "metadata") {
        read_meta_data(variation, tree)
      } else if (tree[1] == "properties") {
        read_properties(variation, tree)
      } else {
        NULL
      }
    })
    res <- Filter(Negate(is.null), res)
    do.call(cbind, res)
  }

  res <- lapply(data$variations, read_single_variation)
  res <- Reduce(rbind, res)

  if (!is.data.frame(res) || nrow(res) == 0) {
    stop("The uploaded file is empty. Please upload a file with data.")
  }
  max_cols <- 1000
  max_rows <- 1e6
  if (nrow(res) > max_rows || ncol(res) > max_cols) {
    stop(sprintf(
      "Data exceeds the limit of %d rows or %d columns. Please upload a smaller dataset.",
      max_rows, max_cols
    ))
  }
  DataModelState$df <- res
  name <- paste0("df", ResultsState$counter)
  ResultsState$all_data[[name]] <- DataModelState$df
  ResultsState$counter <- ResultsState$counter + 1
}

env_import_export_variations_V1_2$read_variations <- read_variations
