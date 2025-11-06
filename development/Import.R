env_import_V1_2$trim_outer_quotes <- function(v) {
  sapply(v, function(x) {
    if (x == "") return("")
    sub('^"(.*)"$', '\\1', x)
  })
}
env_import_V1_2$cast_types_cols <- function(df, excel = FALSE) {
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
    if (!excel) b <- env_import_V1_2$trim_outer_quotes(b)
    return(as.factor(b))
  }
  df <- Map(conv, check, df)
  data.frame(df)
}

env_import_V1_2$is_separator <- function(c) {
  all(is.na(c))
}

env_import_V1_2$scan_rows_or_cols <- function(df, rows = TRUE) {
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
      if (!env_import_V1_2$is_separator(getter(df, indices[i]))) return(indices[i])
    }
    stop("Did not found any start col")
  }
  scan <- function(df, offset) {
    start <- find_start(df, offset)
    end <- start
    indices <- start:dim_fct(df)
    for (i in seq_along(indices)) {
      if (!env_import_V1_2$is_separator(getter(df, indices[i]))) {
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

env_import_V1_2$find_sub_tables <- function(rows, cols, df) {
  if (length(cols) > 1) return(TRUE)
  if (length(rows) > 1) return(TRUE)
  dims <- c(rows[[1]]$end, cols[[1]]$end)
  !all(dims == dim(df))
}

env_import_V1_2$extract_tables <- function(env_tables, df, excel = FALSE) {
  cols <- env_import_V1_2$scan_rows_or_cols(df, FALSE)
  tables <- list()
  for (cs in seq_along(cols)) {
    temp <- df[, cols[[cs]]$start:cols[[cs]]$end]
    rows <- env_import_V1_2$scan_rows_or_cols(temp, TRUE)
    tables <- c(tables, lapply(rows, function(rs) {
      temp[rs$start:rs$end, ]
    }))
  }
  if (!env_import_V1_2$find_sub_tables(rows, cols, df)) {
    env_tables$tables <- c(env_tables$tables, tables)
    return()
  }
  tables <- lapply(tables, \(x) {
    env_import_V1_2$extract_tables(env_tables, x, excel)
  })
}

env_import_V1_2$convert_to_dfs <- function(tables, excel = FALSE) {
  tables <- lapply(tables, function(x) {
    if (nrow(x) > 2) {
      temp <- x[2:nrow(x), ]
      if (excel) {
        names(temp) <- sapply(x[1, ], make.names)
      } else {
        names(temp) <- sapply(env_import_V1_2$trim_outer_quotes(x[1, ]), make.names)
      }
      x <- temp
    }
    x
  })
  lapply(tables, function(x) {
    env_import_V1_2$cast_types_cols(x, excel)
  })
}

env_import_V1_2$read_data_excel <- function(path) {
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
    env_import_V1_2$extract_tables(env_tables, tables, TRUE)
    tables <- env_import_V1_2$convert_to_dfs(env_tables$tables, TRUE)
    res <- c(res, tables)
  }
  res
}

path <- "./development/TestExcel1.xlsx"
path <- "./development/TestExcel2.xlsx"
path <- "./development/TestExcel3.xlsx"
ts <- env_import_V1_2$read_data_excel(path)
ts
