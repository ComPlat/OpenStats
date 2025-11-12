library(tinytest)
library(OpenStats)

test_cast_cols <- function() {
  df <- data.frame(
    a = "1", b = "1.1", c = "Bla"
  )
  res <- OpenStats:::cast_types_cols(df)
  expect_true(is.numeric(res[[1]]))
  expect_true(is.numeric(res[[2]]))
  expect_true(is.factor(res[[3]]))
}
test_cast_cols()

test_is_separator <- function() {
  res1 <- OpenStats:::is_separator(c(1, 2, 3))
  res2 <- OpenStats:::is_separator(c(NA, NA))
  expect_true(!res1)
  expect_true(res2)
}
test_is_separator()

test_scan_rows_or_cols <- function() {
  res_row <- OpenStats:::scan_rows_or_cols(CO2)[[1]]
  res_col <- OpenStats:::scan_rows_or_cols(CO2, FALSE)[[1]]
  expect_equal(list(start = 1, end = 84), res_row)
  expect_equal(list(start = 1, end = 5), res_col)
  expect_equal(c(res_row[[2]], res_col[[2]]), dim(CO2))

  # scan_rows_or_cols find the dims of the most left and most upper table
  path <- system.file("/test_data/MultiTable4.csv", package = "OpenStats")
  raw_content <- OpenStats:::read_raw(path)
  res_row <- OpenStats:::scan_rows_or_cols(raw_content)[[1]]
  res_col <- OpenStats:::scan_rows_or_cols(raw_content, FALSE)[[1]]
  expect_equal(list(start = 1, end = 3), res_row)
  expect_equal(list(start = 1, end = 3), res_col)
}
test_scan_rows_or_cols()

test_find_sub_tables <- function() { # TODO: proceed
  res1 <- OpenStats:::find_sub_tables(c(1, 84), c(1, 5), CO2)
  expect_true(res1)
}
test_find_sub_tables()

test_extract_tables <- function() {
  env_tables <- new.env(parent = emptyenv())
  env_tables$tables <- list()
  path <- system.file("/test_data/MultiTable4.csv", package = "OpenStats")
  raw_content <- OpenStats:::read_raw(path)
  OpenStats:::extract_tables(env_tables, raw_content)
  expect_true(length(env_tables$tables) == 3L) # Is tested in more details elsewhere
}
test_extract_tables()

test_convert_to_df <- function() {
  env_tables <- new.env(parent = emptyenv())
  env_tables$tables <- list()
  path <- system.file("/test_data/MultiTable4.csv", package = "OpenStats")
  raw_content <- OpenStats:::read_raw(path)
  OpenStats:::extract_tables(env_tables, raw_content)
  res <- OpenStats:::convert_to_dfs(env_tables$tables)
  expect_true(length(res) == 3L) # Is tesed in more details elsewhere
}
test_convert_to_df()

test_identify_seperator <- function() {
  comma_path <- system.file("/test_data/CO2.csv", package = "OpenStats")
  semicolon_path <- system.file("/test_data/semicolon_data.csv", package = "OpenStats")
  tab_path <- system.file("/test_data/tab_data.csv", package = "OpenStats")
  wrong_path <- system.file("/test_data/0InvalidHistory.json", package = "OpenStats")
  checks <- c()
  checks[[1]] <- expect_equal(OpenStats:::identify_seperator(comma_path), ",")
  checks[[2]] <- expect_equal(OpenStats:::identify_seperator(semicolon_path), ";")
  checks[[3]] <- expect_equal(OpenStats:::identify_seperator(tab_path), "\t")
  checks[[4]] <- expect_error(OpenStats:::identify_seperator(wrong_path)) # TODO: wrap it in try and check whether inherits from try-error. Otherwise the annoying message pops up
  expect_true(all(unlist(checks)))
}
test_identify_seperator()

test_csv_import <- function() {
  path1 <- system.file("/test_data/CO2.csv", package = "OpenStats")
  path2 <- system.file("/test_data/MultiTable1.csv", package = "OpenStats")
  path3 <- system.file("/test_data/MultiTable2.csv", package = "OpenStats")
  path4 <- system.file("/test_data/MultiTable3.csv", package = "OpenStats")
  path5 <- system.file("/test_data/MultiTable4.csv", package = "OpenStats")
  df1 <- OpenStats:::read_data_csv(path1)[[1]]
  df2 <- OpenStats:::read_data_csv(path2)
  df2_expected <- list(
    data.frame(a = as.factor(c("x1", "x2", "x2", "x1")), b = c(1.3, 2, 2, 1), c = 4),
    data.frame(d = as.factor(c("x1", "x2", "x2", "x1")), e = c(1.1, 2, 2, 1), f = 4)
  )
  df3 <- OpenStats:::read_data_csv(path3)
  df4 <- OpenStats:::read_data_csv(path4)
  df4_expected <- list(
    data.frame(a = as.factor(c("x1", "x2", "x2", "x1")), b = c(1.3, 2, 2, 1), c = 4),
    data.frame(d = as.factor(c("x1", "x2", "x2", "x1")), e = c(1.1, 2, 2, 1), f = 4),
    data.frame(g = c(1, 4), h = c(2, 5), I = c(3, 6))
  )
  df5 <- OpenStats:::read_data_csv(path5)
  df5_expected <- list(
    data.frame(a = as.factor(c("x1", "x1")), b = c(1.3, 1.3), c = 4),
    data.frame(d = as.factor(c("x1", "x2", "x2", "x1")), e = c(1.1, 2, 2, 1), f = 4),
    data.frame(g = c(1, 4, 7, 10), h = c(2, 5, 8, 11), I = c(3, 6, 9, 12))
  )

  checks <- c()

  checks[[1]] <- expect_equal(dim(df1), dim(read.csv(path1, header = TRUE)))

  checks[[2]] <- expect_equal(length(df2), 2)
  checks[[3]] <- expect_equal(df2, df2_expected)

  checks[[4]] <- expect_equal(length(df3), 2)
  checks[[5]] <- expect_equal(df3, df2_expected)

  checks[[6]] <- expect_equal(length(df4), 3)
  checks[[7]] <- expect_equal(df4, df4_expected)

  checks[[8]] <- expect_equal(length(df5), 3)
  checks[[9]] <- expect_equal(df5, df5_expected)

  expect_true(all(unlist(checks)))
}
test_csv_import()

test_read_data_excel <- function() {
  path <- system.file("/test_data/MultiTableExcel.xlsx", package = "OpenStats")
  df <- OpenStats:::read_data_excel(path)
  df
  df_expected <- list(
    data.frame(a = as.factor(c("x1", "x2", "x2", "x1")), b = c(1.1, 2, 2, 1), c = 4),
    data.frame(g = as.factor(c("x1", "x2", "x2", "x1")), h = c(1.1, 2, 2, 1), I = 4, j = 5),
    data.frame(d = c(1, 4), e = c(2, 5), f = c(3, 6)),
    data.frame(k = c(1, 4, 5), l = c(2, 5, 5), m = c(3, 6, 5)),
    data.frame(sheet2 = c(1, 3), sheet2.1 = c(2, 4))
  )
  checks <- c()
  checks[[1]] <- expect_equal(length(df), 5)
  checks[[2]] <- expect_equal(df, df_expected)
  expect_true(all(unlist(checks)))
}
test_read_data_excel()

# Test env_import_V1_2$read_data
# =======================================================================================
# TODO: update test
test_readData <- function() {

  # Test 1: Valid input with an Excel file
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(NULL)
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(NULL)
  test_file <- tempfile(fileext = ".xlsx")
  write.csv(data.frame(a = 1:5, b = letters[1:5]), test_file, row.names = FALSE)
  writexl::write_xlsx(read.csv(test_file), test_file)
  OpenStats:::read_data(test_file, DataModelState, ResultsState)
  expect_equal(class(DataModelState$df), "data.frame")
  expect_equal(nrow(DataModelState$df), 5)
  expect_equal(ncol(DataModelState$df), 2)

  # Test 2: Valid input with a CSV file (comma-separated)
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(NULL)
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(NULL)
  test_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:5, b = letters[1:5]), test_file, row.names = FALSE)
  OpenStats:::read_data(test_file, DataModelState, ResultsState)
  expect_equal(class(DataModelState$df), "data.frame")
  expect_equal(nrow(DataModelState$df), 5)
  expect_equal(ncol(DataModelState$df), 2)

  # Test 3: File exceeds size limit
  test_file <- tempfile()
  write.csv(data.frame(a = 1:(50 * 1024^2 / 2)), test_file, row.names = FALSE)
  expect_error(OpenStats:::read_data(test_file), "File size exceeds the 50 MB limit.")

  # Test 4: File with unknown separator
  test_file <- tempfile()
  writeLines("a|b|c\n1|2|3", test_file)
  expect_error(
    OpenStats:::read_data(test_file),
    "Could not identify the separator. Please upload a file with a known separator."
  )

  # Test 5: File with semicolon separator
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(NULL)
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(NULL)
  test_file <- tempfile()
  writeLines("a;b;c\n1;2;3", test_file)
  result <- OpenStats:::read_data(test_file, DataModelState, ResultsState)
  expect_equal(class(DataModelState$df), "data.frame")
  expect_equal(nrow(DataModelState$df), 2)
  expect_equal(ncol(DataModelState$df), 3)

  # Test 6: File with tab separator
  ResultsState <- OpenStats:::backend_result_state_V1_2$new(NULL)
  ResultsState$bgp$in_backend <- TRUE
  DataModelState <- OpenStats:::backend_data_model_state_V1_2$new(NULL)
  test_file <- tempfile()
  writeLines("a\tb\tc\n1\t2\t3", test_file)
  result <- OpenStats:::read_data(test_file, DataModelState, ResultsState)
  expect_equal(class(DataModelState$df), "data.frame")
  expect_equal(nrow(DataModelState$df), 2)
  expect_equal(ncol(DataModelState$df), 3)

  # Test 7: File with invalid path
  expect_error(OpenStats:::read_data("nonexistent_file.csv"), "File does not exists")

  # Test 8: Data exceeds row or column limits
  test_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(matrix(1, nrow = 1e6 + 1, ncol = 2)), test_file, row.names = FALSE)
  expect_error(OpenStats:::read_data(test_file, DataModelState, ResultsState), "Data exceeds the limit of")

  write.csv(data.frame(matrix(1, nrow = 10, ncol = 1001)), test_file, row.names = FALSE)
  expect_error(OpenStats:::read_data(test_file, DataModelState, ResultsState), "Data exceeds the limit of")

  # Test 9: Empty file
  test_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(), test_file, row.names = FALSE)
  expect_error(
    OpenStats:::read_data(test_file, DataModelState, ResultsState),
    "Could not identify the separator. Please upload a file with a known separator."
  )

  # Test 10: Non-character input for path
  expect_error(OpenStats:::read_data(123), "is.character")
}
test_readData()
