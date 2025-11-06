library(tinytest)

test_identify_seperator <- function() {
  comma_path <- system.file("/test_data/CO2.csv", package = "OpenStats")
  semicolon_path <- system.file("/test_data/semicolon_data.csv", package = "OpenStats")
  tab_path <- system.file("/test_data/tab_data.csv", package = "OpenStats")
  wrong_path <- system.file("/test_data/0InvalidHistory.json", package = "OpenStats")
  checks <- c()
  checks[[1]] <- expect_equal(OpenStats:::env_import_V1_2$identify_seperator(comma_path), ",")
  checks[[2]] <- expect_equal(OpenStats:::env_import_V1_2$identify_seperator(semicolon_path), ";")
  checks[[3]] <- expect_equal(OpenStats:::env_import_V1_2$identify_seperator(tab_path), "\t")
  checks[[4]] <- expect_error(OpenStats:::env_import_V1_2$identify_seperator(wrong_path))
  expect_true(all(unlist(checks)))
}
test_identify_seperator()

test_csv_import <- function() {
  path1 <- system.file("/test_data/CO2.csv", package = "OpenStats")
  path2 <- system.file("/test_data/MultiTable1.csv", package = "OpenStats")
  path3 <- system.file("/test_data/MultiTable2.csv", package = "OpenStats")
  path4 <- system.file("/test_data/MultiTable3.csv", package = "OpenStats")
  path5 <- system.file("/test_data/MultiTable4.csv", package = "OpenStats")
  df1 <- OpenStats:::env_import_V1_2$read_data_csv(path1)[[1]]
  df2 <- OpenStats:::env_import_V1_2$read_data_csv(path2)
  df2_expected <- list(
    data.frame(a = as.factor(c("x1", "x2", "x2", "x1")), b = c(1.3, 2, 2, 1), c = 4),
    data.frame(d = as.factor(c("x1", "x2", "x2", "x1")), e = c(1.1, 2, 2, 1), f = 4)
  )
  df3 <- OpenStats:::env_import_V1_2$read_data_csv(path3)
  df4 <- OpenStats:::env_import_V1_2$read_data_csv(path4)
  df4_expected <- list(
    data.frame(a = as.factor(c("x1", "x2", "x2", "x1")), b = c(1.3, 2, 2, 1), c = 4),
    data.frame(d = as.factor(c("x1", "x2", "x2", "x1")), e = c(1.1, 2, 2, 1), f = 4),
    data.frame(g = c(1, 4), h = c(2, 5), I = c(3, 6))
  )
  df5 <- OpenStats:::env_import_V1_2$read_data_csv(path5)
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
  df <- OpenStats:::env_import_V1_2$read_data_excel(path)
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
