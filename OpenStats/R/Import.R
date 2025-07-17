# TODO: update docu. Important: one empty row/col between each table
# TODO: allow to define seperator --> reload data and delete file after session end.
# and warn that user shouldn't use comma separated numbers
# Upload data into R
# TODO: check that import works also for server version

identify_seperator <- function(path) {
  line <- readLines(path, n = 1)
  if(grepl(";", line)) return(";")
  if(grepl("\t", line)) return("\t")
  if(grepl(",", line)) return(",")
  stop("Could not identify the separator. Please upload a file with a known separator.")
}

trim_outer_quotes <- function(v) {
  sapply(v, function(x) {
    if (x == "") return("")
    sub('^"(.*)"$', '\\1', x)
  })
}
cast_types_cols <- function(df, excel = FALSE) {
  f <- function(x) {
    options(warn = -1)
    x <- as.numeric(x)
    options(warn = 0)
    x <- x[!is.na(x)]
    length(x) > 0
  }
  check <- apply(df, 2, f)
  conv <- function(a, b) {
    if (a) {
      return(as.numeric(b))
    }
    if (!excel) b <- trim_outer_quotes(b)
    return(as.factor(b))
  }
  df <- Map(conv, check, df)
  data.frame(df)
}

is_separator <- function(c) {
  all(is.na(c))
}

scan_rows_or_cols <- function(df, rows = TRUE) {
  dim_fct <- nrow
  if (!rows) dim_fct <- ncol

  getter <- function(df, idx) {
    if (rows) {
      df[idx, ]
    } else {
      df[, idx]
    }
  }
  find_start <- function(df, offset) {
    indices <- offset:dim_fct(df)
    for (i in seq_along(indices)) {
      if (!is_separator(getter(df, indices[i]))) return(indices[i])
    }
    stop("Did not found any start col")
  }
  scan <- function(df, offset) {
    start <- find_start(df, offset)
    end <- start
    indices <- start:dim_fct(df)
    for (i in seq_along(indices)) {
      if (!is_separator(getter(df, indices[i]))) {
        end <- end + 1
      } else {
        break
      }
    }
    list(start = start, end = end - 1)
  }
  find_indices <- function(df) {
    offset <- 1
    res <- list()
    while(offset <= dim_fct(df)) {
      entry <- try(scan(df, offset), silent = TRUE)
      if (inherits(entry, "try-error")) {
        return(res)
      }
      res[[length(res) + 1]] <- entry
      offset <- res[[length(res)]]$end + 1
    }
    res
  }
  find_indices(df)
}

find_sub_tables <- function(rows, cols, df) {
  if (length(cols) > 1) return(TRUE)
  if (length(rows) > 1) return(TRUE)
  dims <- c(rows[[1]]$end, cols[[1]]$end)
  !all(dims == dim(df))
}

extract_tables <- function(env_tables, df, excel = FALSE) {
  cols <- scan_rows_or_cols(df, FALSE)
  tables <- list()
  for (cs in seq_along(cols)) {
    temp <- df[, cols[[cs]]$start:cols[[cs]]$end]
    rows <- scan_rows_or_cols(temp, TRUE)
    tables <- c(tables, lapply(rows, function(rs) {
      temp[rs$start:rs$end, ]
    }))
  }
  if (!find_sub_tables(rows, cols, df)) {
    env_tables$tables <- c(env_tables$tables, tables)
    return()
  }
  tables <- lapply(tables, \(x) {
    extract_tables(env_tables, x, excel)
  })
}

convert_to_dfs <- function(tables, excel = FALSE) {
  tables <- lapply(tables, function(x) {
    if (nrow(x) > 2) {
      temp <- x[2:nrow(x), ]
      if (excel) {
        names(temp) <- sapply(x[1, ], make.names)
      } else {
        names(temp) <- sapply(trim_outer_quotes(x[1, ]), make.names)
      }
      x <- temp
    }
    x
  })
  lapply(tables, function(x) {
    cast_types_cols(x, excel)
  })
}

read_data_excel <- function(path) {
  sheets <- readxl::excel_sheets(path)
  res <- list()
  for (s in sheets) {
    tables <- suppressMessages(
      suppressWarnings(
        readxl::read_excel(path, sheet = s, col_names = FALSE)
      )
    )
    tables <- as.data.frame(tables)
    env_tables <- new.env(parent = emptyenv())
    env_tables$tables <- list()
    extract_tables(env_tables, tables, TRUE)
    tables <- convert_to_dfs(env_tables$tables, TRUE)
    res <- c(res, tables)
  }
  res
}

read_raw <- function(path) {
  sep <- identify_seperator(path)
  raw_content <- readLines(path)
  raw_content <- lapply(raw_content, function(r){
    r <- strsplit(r, split = sep, fixed = TRUE)[[1]]
    r[r == ""] <- NA
    r
  })
  max_cols <- max(lengths(raw_content))
  raw_content <- lapply(raw_content, function(r) {
    length(r) <- max_cols  # pads with NA if too short
    r
  })
  raw_df <- do.call(rbind, raw_content)
  as.data.frame(raw_df, stringsAsFactors = FALSE)
}

read_data_csv <- function(path) {
  tables <- read_raw(path)
  env_tables <- new.env(parent = emptyenv())
  env_tables$tables <- list()
  extract_tables(env_tables, tables, FALSE)
  convert_to_dfs(env_tables$tables, FALSE)
}

readData <- function(path, DataModelState, ResultsState) {
  stopifnot(is.character(path))
  if (!file.exists(path)) stop("File does not exists")
  max_file_size <- 50 * 1024^2 # 50 MB in bytes
  file_size <- file.info(path)$size
  if (is.na(file_size) || file_size > max_file_size) {
    stop("File size exceeds the 50 MB limit. Please upload a smaller file.")
  }
  tables <- try(read_data_excel(path), silent = TRUE)
  if (class(tables) == "try-error") {
    tables <- try(read_data_csv(path))
    if (class(tables) == "try-error") {
      stop(tables)
    }
  }

  DataModelState$df <- tables[[1]]
  if (length(tables) >= 1) {
    lapply(tables, function(t) {
      name <- paste0("df", ResultsState$counter)
      ResultsState$all_data[[name]] <- t
      ResultsState$counter <- ResultsState$counter + 1
    })
  }

  # Check data frame dimensions
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
}
