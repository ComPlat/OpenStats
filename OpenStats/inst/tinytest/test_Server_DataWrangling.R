library(tinytest)

trim <- function(s) {
  gsub("\t|\n| ", "", s)
}

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")

app <- OpenStats:::app()
srv <- app$server

# The expression is now built client-side and only reaches the server as a
# string. In tests (OpenStats.background = FALSE) the run handlers read the
# operation from DataWranglingState$code_string, so each test sets that string
# directly instead of clicking the old token-append buttons.

# Seq
# =================================================================
test_seq <- function(app, srv) {
  options(OpenStats.background = FALSE)
  expected_string <- "Seq(1,100,1)"
  expected <- seq(1, 100, 1)
  ex <- NULL
  ex_string <- NULL
  shiny::testServer(srv, {
    DataModelState$df <- CO2
    ResultsState$all_data <- list(df = CO2)
    DataModelState$active_df_name <- "df"
    DataWranglingState$df_name <- "df"
    DataWranglingState$df <- CO2
    DataWranglingState$intermediate_vars <- list(df = CO2)
    session$setInputs(active_tab = "DataWrangling")
    session$flushReact()

    DataWranglingState$code_string <- "Seq(1, 100, 1)"
    session$setInputs(`OP-iv` = "Seq")
    session$setInputs(`OP-run_op_intermediate` = 1)
    ex <<- session$userData$export_iv[[2]]
    ex_string <<- session$userData$export_code_string |> trim()
  })
  expect_equal(ex, expected, info = "Sequence result")
  expect_equal(ex_string, expected_string, info = "Seq code string")
}
test_seq(app, srv)

# DataFrame
# =================================================================
test_df <- function(app, srv) {
  options(OpenStats.background = FALSE)
  expected_string <- "DataFrame(conc,conc)"
  expected <- data.frame(CO2$conc, CO2$conc)
  ex <- NULL
  ex_string <- NULL
  shiny::testServer(srv, {
    DataModelState$df <- CO2
    ResultsState$all_data <- list(df = CO2)
    DataModelState$active_df_name <- "df"
    DataWranglingState$df_name <- "df"
    DataWranglingState$df <- CO2
    DataWranglingState$intermediate_vars <- list(df = CO2)
    session$setInputs(active_tab = "DataWrangling")
    session$flushReact()

    DataWranglingState$code_string <- "DataFrame(conc, conc)"
    session$setInputs(`OP-iv` = "df_new")
    session$setInputs(`OP-run_op_intermediate` = 1)
    ex <<- session$userData$export_iv[[2]]
    ex_string <<- session$userData$export_code_string |> trim()
  })
  Map(function(a, b) {
    all(a == b)
  }, ex, expected) |> unlist() |> all() |> expect_true()
  expect_equal(ex_string, expected_string, info = "df string")
}
test_df(app, srv)

# random functions
# =================================================================
test_random <- function(app, srv) {
  random_funcs <- c(
    "dnorm", "pnorm", "qnorm", "rnorm",
    "dbinom", "pbinom", "qbinom", "rbinom",
    "dpois", "ppois", "rpois",
    "dunif", "punif", "qunif", "runif"
  )
  args <- list(
    0, 0, 0.2, 5,
    "0, 10, 0.5", "0, 10, 0.5", "0, 10, 0.5", "1, 1000, 0.001",
    "0, 2", "0, 2", "20, 0.2",
    "0, 0, 2", "0.2, 0, 3", "0.2, 0, 3", 1
  )
  paste_and_eval <- function(fct, args) {
    eval(str2lang(paste0(fct, "(", args, ")")))
  }
  options(OpenStats.background = FALSE)
  expected <- list()
  ex <- list()

  for (i in seq_len(length(random_funcs))) {
    set.seed(42)
    expected[[random_funcs[[i]]]] <- paste_and_eval(random_funcs[[i]], args[[i]])
    proper <- paste0(toupper(substring(random_funcs[[i]], 1, 1)), substring(random_funcs[[i]], 2))
    arg <- args[[i]]
    shiny::testServer(srv, {
      set.seed(42)
      DataModelState$df <- CO2
      ResultsState$all_data <- list(df = CO2)
      DataModelState$active_df_name <- "df"
      DataWranglingState$df_name <- "df"
      DataWranglingState$df <- CO2
      DataWranglingState$intermediate_vars <- list(df = CO2)
      session$setInputs(active_tab = "DataWrangling")
      session$flushReact()

      DataWranglingState$code_string <- paste0(proper, "(", arg, ")")
      session$setInputs(`OP-iv` = "rand")
      session$setInputs(`OP-run_op_intermediate` = 1)
      ex[[random_funcs[[i]]]] <<- session$userData$export_iv[[2]]
    })
  }
  expect_equal(ex, expected, info = "random result")
}
test_random(app, srv)

# math (unary)
# =================================================================
test_unary <- function(app, srv) {
  options(OpenStats.background = FALSE)
  ex <- list()
  expected <- list(
    log(CO2$uptake), log10(CO2$uptake),
    sqrt(CO2$uptake), exp(CO2$uptake),
    sin(CO2$uptake), cos(CO2$uptake), tan(CO2$uptake),
    sinh(CO2$uptake), cosh(CO2$uptake), tanh(CO2$uptake),
    asin(CO2$uptake), acos(CO2$uptake), atan(CO2$uptake),
    abs(CO2$uptake), ceiling(CO2$uptake), floor(CO2$uptake),
    trunc(CO2$uptake), round(CO2$uptake)
  )
  fns <- c(
    "log", "log10", "sqrt", "exp",
    "sin", "cos", "tan",
    "sinh", "cosh", "tanh",
    "asin", "acos", "atan",
    "abs", "ceiling", "floor",
    "trunc", "round"
  )
  for (i in seq_len(length(fns))) {
    shiny::testServer(srv, {
      DataModelState$df <- CO2
      ResultsState$all_data <- list(df = CO2)
      DataModelState$active_df_name <- "df"
      DataWranglingState$df_name <- "df"
      DataWranglingState$df <- CO2
      DataWranglingState$intermediate_vars <- list(df = CO2)
      session$setInputs(active_tab = "DataWrangling")
      session$flushReact()

      DataWranglingState$code_string <- paste0(fns[[i]], "(uptake)")
      session$setInputs(`OP-iv` = "temp")
      session$setInputs(`OP-run_op_intermediate` = 1)
      ex[[i]] <<- session$userData$export_iv[[2]]
    })
  }
  expect_equal(ex, expected, info = "math stuff")
}
test_unary(app, srv)

# binary
# =================================================================
test_binary <- function(app, srv) {
  options(OpenStats.background = FALSE)
  ex <- list()
  expected <- list(
    CO2$conc + CO2$conc,
    CO2$conc - CO2$conc,
    CO2$conc * CO2$conc,
    CO2$conc / CO2$conc
  )
  ops <- c("+", "-", "*", "/")
  for (i in seq_len(length(ops))) {
    shiny::testServer(srv, {
      DataModelState$df <- CO2
      ResultsState$all_data <- list(df = CO2)
      DataModelState$active_df_name <- "df"
      DataWranglingState$df_name <- "df"
      DataWranglingState$df <- CO2
      DataWranglingState$intermediate_vars <- list(df = CO2)
      session$setInputs(active_tab = "DataWrangling")
      session$flushReact()

      DataWranglingState$code_string <- paste0("conc ", ops[[i]], " conc")
      session$setInputs(`OP-iv` = "temp")
      session$setInputs(`OP-run_op_intermediate` = 1)
      ex[[i]] <<- session$userData$export_iv[[2]]
    })
  }
  expect_equal(ex, expected, info = "binary results")
}
test_binary(app, srv)

# comparison
# =================================================================
test_comparisons <- function(app, srv) {
  options(OpenStats.background = FALSE)
  ex <- list()
  a <- c(1, 2, 2, 2)
  b <- 1:4
  expected <- list(
    a == b, a != b, a > b, a < b, a >= b, a <= b
  )
  ops <- c("==", "!=", ">", "<", ">=", "<=")
  for (i in seq_len(length(ops))) {
    shiny::testServer(srv, {
      DataModelState$df <- CO2
      ResultsState$all_data <- list(df = CO2)
      DataModelState$active_df_name <- "df"
      DataWranglingState$df_name <- "df"
      DataWranglingState$df <- CO2
      DataWranglingState$intermediate_vars <- list(df = CO2)
      session$setInputs(active_tab = "DataWrangling")
      session$flushReact()

      DataWranglingState$code_string <- "C(1, 2, 2, 2)"
      session$setInputs(`OP-iv` = "a")
      session$setInputs(`OP-run_op_intermediate` = 1)

      DataWranglingState$code_string <- "C(1, 2, 3, 4)"
      session$setInputs(`OP-iv` = "b")
      session$setInputs(`OP-run_op_intermediate` = 2)

      DataWranglingState$code_string <- paste0("a ", ops[[i]], " b")
      session$setInputs(`OP-iv` = "temp")
      session$setInputs(`OP-run_op_intermediate` = 3)
      ex[[i]] <<- session$userData$export_iv[[4]]
    })
  }
  expect_equal(ex, expected, info = "comparisons results")
}
test_comparisons(app, srv)

# stats
# =================================================================
test_stats <- function(app, srv) {
  options(OpenStats.background = FALSE)
  ex <- list()
  a <- 1:4
  expected <- list(
    mean(a), median(a), min(a), max(a), sum(a), sd(a)
  )
  fns <- c("Mean", "Median", "Min", "Max", "Sum", "SD")
  for (i in seq_len(length(fns))) {
    shiny::testServer(srv, {
      DataModelState$df <- CO2
      ResultsState$all_data <- list(df = CO2)
      DataModelState$active_df_name <- "df"
      DataWranglingState$df_name <- "df"
      DataWranglingState$df <- CO2
      DataWranglingState$intermediate_vars <- list(df = CO2)
      session$setInputs(active_tab = "DataWrangling")
      session$flushReact()

      DataWranglingState$code_string <- "C(1, 2, 3, 4)"
      session$setInputs(`OP-iv` = "a")
      session$setInputs(`OP-run_op_intermediate` = 1)

      DataWranglingState$code_string <- paste0(fns[[i]], "(a)")
      session$setInputs(`OP-iv` = "temp")
      session$setInputs(`OP-run_op_intermediate` = 2)
      ex[[i]] <<- session$userData$export_iv[[3]]
    })
  }
  expect_equal(ex, expected, info = "stats results")
}
test_stats(app, srv)

# indexing
# =================================================================
test_indexing <- function(app, srv) {
  options(OpenStats.background = FALSE)
  CO2 <- as.data.frame(CO2) # required to remove some special classes: "nfnGroupedData" "nmGroupedData" "groupedData"
  ex <- list()
  expected <- list(
    CO2[, c("conc", "conc", "uptake")],
    CO2[CO2$conc > 700, ]
  )
  shiny::testServer(srv, {
    DataModelState$df <- CO2
    ResultsState$all_data <- list(df = CO2)
    DataModelState$active_df_name <- "df"
    DataWranglingState$df_name <- "df"
    DataWranglingState$df <- CO2
    DataWranglingState$intermediate_vars <- list(df = CO2)
    session$setInputs(active_tab = "DataWrangling")
    session$flushReact()

    DataWranglingState$code_string <- "get_cols(df, conc, conc, uptake)"
    session$setInputs(`OP-iv` = "df_new1")
    session$setInputs(`OP-run_op_intermediate` = 1)
    ex[[1]] <<- session$userData$export_iv[[2]]

    DataWranglingState$code_string <- "get_rows(df, conc > 700)"
    session$setInputs(`OP-iv` = "df_new2")
    session$setInputs(`OP-run_op_intermediate` = 2)
    ex[[2]] <<- session$userData$export_iv[[3]]
  })
  expect_equal(ex, expected, info = "indexing")
}
test_indexing(app, srv)

# string functions
# =================================================================
test_string_ops <- function(app, srv) {
  options(OpenStats.background = FALSE)
  ex <- list()
  expected <- list("a b", "ab", "A", "a")
  shiny::testServer(srv, {
    DataModelState$df <- CO2
    ResultsState$all_data <- list(df = CO2)
    DataModelState$active_df_name <- "df"
    DataWranglingState$df_name <- "df"
    DataWranglingState$df <- CO2
    DataWranglingState$intermediate_vars <- list(df = CO2)
    session$setInputs(active_tab = "DataWrangling")
    session$flushReact()

    DataWranglingState$code_string <- "C('a')"
    session$setInputs(`OP-iv` = "a")
    session$setInputs(`OP-run_op_intermediate` = 1)

    DataWranglingState$code_string <- "C('b')"
    session$setInputs(`OP-iv` = "b")
    session$setInputs(`OP-run_op_intermediate` = 2)

    DataWranglingState$code_string <- "paste(a, b)"
    session$setInputs(`OP-iv` = "res1")
    session$setInputs(`OP-run_op_intermediate` = 3)
    ex[[1]] <<- session$userData$export_iv[[4]]

    DataWranglingState$code_string <- "paste0(a, b)"
    session$setInputs(`OP-iv` = "res2")
    session$setInputs(`OP-run_op_intermediate` = 4)
    ex[[2]] <<- session$userData$export_iv[[5]]

    DataWranglingState$code_string <- "toupper(a)"
    session$setInputs(`OP-iv` = "res3")
    session$setInputs(`OP-run_op_intermediate` = 5)
    ex[[3]] <<- session$userData$export_iv[[6]]

    DataWranglingState$code_string <- "tolower(res3)"
    session$setInputs(`OP-iv` = "res4")
    session$setInputs(`OP-run_op_intermediate` = 6)
    ex[[4]] <<- session$userData$export_iv[[7]]
  })
  expect_equal(ex, expected, info = "string stuff")
}
test_string_ops(app, srv)

# casts
# =================================================================
test_casts <- function(app, srv) {
  options(OpenStats.background = FALSE)
  ex <- list()
  a <- c("10.5", "1.4", "1.3", 3.14)
  expected <- list(
    as.integer(a), as.numeric(a), as.factor(a), as.character(a)
  )
  fns <- c("as.int", "as.real", "as.fact", "as.char")
  for (i in seq_len(length(fns))) {
    shiny::testServer(srv, {
      DataModelState$df <- CO2
      ResultsState$all_data <- list(df = CO2)
      DataModelState$active_df_name <- "df"
      DataWranglingState$df_name <- "df"
      DataWranglingState$df <- CO2
      DataWranglingState$intermediate_vars <- list(df = CO2)
      session$setInputs(active_tab = "DataWrangling")
      session$flushReact()

      DataWranglingState$code_string <- "C('10.5', '1.4', '1.3', 3.14)"
      session$setInputs(`OP-iv` = "a")
      session$setInputs(`OP-run_op_intermediate` = 1)

      DataWranglingState$code_string <- paste0(fns[[i]], "(a)")
      session$setInputs(`OP-iv` = "temp")
      session$setInputs(`OP-run_op_intermediate` = 2)
      ex[[i]] <<- session$userData$export_iv[[3]]
    })
  }
  expect_equal(ex, expected, info = "casts")
}
test_casts(app, srv)
