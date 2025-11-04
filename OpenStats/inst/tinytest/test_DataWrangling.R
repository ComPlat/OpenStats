sync_code <- function(session) {
  session$setInputs(`OP-editable_code` = session$userData$export_code_string)
  session$flushReact()
}

trim <- function(s) {
  gsub("\t|\n| ", "", s)
}

coverage_test <- nzchar(Sys.getenv("R_COVR"))
run_test <- function(f) {
  if (coverage_test) f(app, srv, FALSE) else f(app, srv, TRUE)
}

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

app <- OpenStats:::app()
srv <- app$server

# Seq tests
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
    session$setInputs(`OP-seq` = 1)
    current_string <- session$userData$export_code_string
    session$setInputs(`OP-editable_code` = paste0(current_string, "1, 100, 1"))
    session$setInputs(`OP-bracket_close` = 1)
    session$setInputs(`OP-iv` = "Seq")
    session$setInputs(`OP-run_op_intermediate` = 1)
    ex <<- session$userData$export_iv[[2]]
    ex_string <<- session$userData$export_code_string |> trim()
  })
  tinytest::expect_equal(ex, expected)
  tinytest::expect_equal(ex_string, expected_string)
}
test_seq(app, srv)

# dataframe tests
# =================================================================
test_df <- function(app, srv) {
  options(OpenStats.background = FALSE)
  expected_string <- "DataFrame(conc,conc)"
  expected <- data.frame(CO2$conc, CO2$conc)
  ex <- NULL
  shiny::testServer(srv, {

    DataModelState$df <- CO2
    ResultsState$all_data <- list(df = CO2)
    DataModelState$active_df_name <- "df"
    DataWranglingState$df_name <- "df"
    DataWranglingState$df <- CO2
    DataWranglingState$intermediate_vars <- list(df = CO2)

    session$setInputs(active_tab = "DataWrangling")

    session$flushReact()
    session$setInputs(`OP-df` = 1); sync_code(session)
    session$setInputs(`OP-colnames_conc_0` = 1); sync_code(session)
    session$setInputs(`OP-comma` = 1); sync_code(session)
    session$setInputs(`OP-colnames_conc_0` = 2); sync_code(session)
    session$setInputs(`OP-bracket_close` = 1); sync_code(session)
    session$setInputs(`OP-iv` = "df_new"); sync_code(session)
    session$setInputs(`OP-run_op_intermediate` = 1)
    ex <<- session$userData$export_iv[[2]]
    ex_string <<- session$userData$export_code_string |> trim()
  })
  tinytest::expect_equal(ex, expected)
  tinytest::expect_equal(ex_string, expected_string)
}
test_df(app, srv)

# random tests
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
    rf <- paste0("OP-", random_funcs[[i]])
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
      do.call(session$setInputs, setNames(list(1), rf))
      session$setInputs(rf = 1); sync_code(session)
      current_string <- session$userData$export_code_string
      session$setInputs(`OP-editable_code` = paste0(current_string, arg));
      session$setInputs(`OP-bracket_close` = 1); sync_code(session)
      session$setInputs(`OP-iv` = "rand"); sync_code(session)
      session$setInputs(`OP-run_op_intermediate` = 1)

      ex[[random_funcs[[i]]]] <<- session$userData$export_iv[[2]]
    })

  }
  tinytest::expect_equal(ex, expected)
}
test_random(app, srv)

# math
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
  functions <- c(
    "OP-log", "OP-log10", "OP-sqrt", "OP-exp",
    "OP-sin", "OP-cos", "OP-tan",
    "OP-sinh", "OP-cosh", "OP-tanh",
    "OP-asin", "OP-acos", "OP-atan",
    "OP-abs", "OP-ceil", "OP-floor",
    "OP-trunc", "OP-round"
  )
  for (i in seq_len(length(functions))) {
    f <- functions[[i]]
    shiny::testServer(srv, {
      DataModelState$df <- CO2
      ResultsState$all_data <- list(df = CO2)
      DataModelState$active_df_name <- "df"
      DataWranglingState$df_name <- "df"
      DataWranglingState$df <- CO2
      DataWranglingState$intermediate_vars <- list(df = CO2)

      session$setInputs(active_tab = "DataWrangling")

      session$flushReact()
      do.call(session$setInputs, setNames(list(1), f))
      session$setInputs(f = 1); sync_code(session)
      session$setInputs(`OP-colnames_uptake_0` = 1); sync_code(session)
      session$setInputs(`OP-bracket_close` = 1); sync_code(session)
      session$setInputs(`OP-iv` = "temp"); sync_code(session)
      session$setInputs(`OP-run_op_intermediate` = 1)
      ex[[i]] <<- session$userData$export_iv[[2]]
    })
  }
  tinytest::expect_equal(ex, expected)
}
test_unary(app, srv)

test_binary <- function(app, srv) {
  options(OpenStats.background = FALSE)
  ex <- list()
  expected <- list(
    CO2$conc + CO2$conc,
    CO2$conc - CO2$conc,
    CO2$conc * CO2$conc,
    CO2$conc / CO2$conc

  )
  functions <- c(
    "OP-add", "OP-sub", "OP-mul", "OP-div"
  )
  for (i in seq_len(length(functions))) {
    f <- functions[[i]]
    shiny::testServer(srv, {
      DataModelState$df <- CO2
      ResultsState$all_data <- list(df = CO2)
      DataModelState$active_df_name <- "df"
      DataWranglingState$df_name <- "df"
      DataWranglingState$df <- CO2
      DataWranglingState$intermediate_vars <- list(df = CO2)

      session$setInputs(active_tab = "DataWrangling")

      session$flushReact()
      session$setInputs(`OP-colnames_conc_0` = 1); sync_code(session)
      do.call(session$setInputs, setNames(list(1), f))
      session$setInputs(f = 1); sync_code(session)
      session$setInputs(`OP-colnames_conc_0` = 2); sync_code(session)
      current_string <- session$userData$export_code_string
      session$setInputs(`OP-iv` = "temp"); sync_code(session)
      session$setInputs(`OP-run_op_intermediate` = 1)
      ex[[i]] <<- session$userData$export_iv[[2]]
    })
  }
  tinytest::expect_equal(ex, expected)
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
  functions <- c(
    "OP-eq", "OP-not_eq", "OP-larger",
    "OP-smaller", "OP-larger_eq", "OP-smaller_eq"
  )
  for (i in seq_len(length(functions))) {
    f <- functions[[i]]
    shiny::testServer(srv, {
      DataModelState$df <- CO2
      ResultsState$all_data <- list(df = CO2)
      DataModelState$active_df_name <- "df"
      DataWranglingState$df_name <- "df"
      DataWranglingState$df <- CO2
      DataWranglingState$intermediate_vars <- list(df = CO2)

      session$setInputs(active_tab = "DataWrangling")

      session$flushReact()
      session$setInputs(`OP-c` = 1)
      current_string <- session$userData$export_code_string
      session$setInputs(`OP-editable_code` = paste0(current_string, "1, 2, 2, 2"))
      session$setInputs(`OP-bracket_close` = 1); sync_code(session)
      session$setInputs(`OP-iv` = "a");
      session$setInputs(`OP-run_op_intermediate` = 1);
      session$userData$export_code_string <- ""; sync_code(session)

      session$flushReact()
      session$setInputs(`OP-c` = 1)
      current_string <- session$userData$export_code_string
      session$setInputs(`OP-editable_code` = paste0(current_string, "1, 2, 3, 4"))
      session$setInputs(`OP-bracket_close` = 1); sync_code(session)
      session$setInputs(`OP-iv` = "b");
      session$setInputs(`OP-run_op_intermediate` = 2);
      session$userData$export_code_string <- ""; sync_code(session)

      session$flushReact()
      session$setInputs(`OP-intermediate_vars_a_0` = 1); sync_code(session)

      do.call(session$setInputs, setNames(list(1), f))
      session$setInputs(f = 1); sync_code(session)

      session$setInputs(`OP-intermediate_vars_b_0` = 2);

      session$setInputs(`OP-iv` = "temp"); sync_code(session)
      session$setInputs(`OP-run_op_intermediate` = 1)
      ex[[i]] <<- session$userData$export_iv[[4]]
    })
  }
  tinytest::expect_equal(ex, expected)
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
  functions <- c(
    "OP-mean", "OP-median", "OP-min",
    "OP-max", "OP-sum", "OP-sd"
  )
  for (i in seq_len(length(functions))) {
    f <- functions[[i]]
    shiny::testServer(srv, {
      DataModelState$df <- CO2
      ResultsState$all_data <- list(df = CO2)
      DataModelState$active_df_name <- "df"
      DataWranglingState$df_name <- "df"
      DataWranglingState$df <- CO2
      DataWranglingState$intermediate_vars <- list(df = CO2)

      session$setInputs(active_tab = "DataWrangling")

      session$flushReact()
      session$setInputs(`OP-c` = 1)
      current_string <- session$userData$export_code_string
      session$setInputs(`OP-editable_code` = paste0(current_string, "1, 2, 3, 4"))
      session$setInputs(`OP-bracket_close` = 1); sync_code(session)
      session$setInputs(`OP-iv` = "a");
      session$setInputs(`OP-run_op_intermediate` = 1);
      session$userData$export_code_string <- ""; sync_code(session)

      do.call(session$setInputs, setNames(list(1), f))
      session$setInputs(f = 1); sync_code(session)
      session$setInputs(`OP-intermediate_vars_a_0` = 1); sync_code(session)
      session$setInputs(`OP-bracket_close` = 1); sync_code(session)

      session$setInputs(`OP-iv` = "temp"); sync_code(session)
      session$setInputs(`OP-run_op_intermediate` = 1)
      ex[[i]] <<- session$userData$export_iv[[3]]
    })
  }
  tinytest::expect_equal(ex, expected)
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

    session$setInputs(`OP-get_cols` = 1); sync_code(session)
    session$setInputs(`OP-colnames_df_0` = 1); sync_code(session)
    session$setInputs(`OP-comma` = 1); sync_code(session)
    session$setInputs(`OP-colnames_conc_0` = 1); sync_code(session)
    session$setInputs(`OP-comma` = 1); sync_code(session)
    session$setInputs(`OP-colnames_conc_0` = 1); sync_code(session)
    session$setInputs(`OP-comma` = 1); sync_code(session)
    session$setInputs(`OP-colnames_uptake_0` = 1); sync_code(session)
    session$setInputs(`OP-bracket_close` = 1); sync_code(session)
    session$setInputs(`OP-iv` = "df_new1"); sync_code(session)
    session$setInputs(`OP-run_op_intermediate` = 1)

    ex[[1]] <<- session$userData$export_iv[[2]]

    session$userData$export_code_string <- ""; sync_code(session)
    session$setInputs(`OP-get_rows` = 1); sync_code(session)
    session$setInputs(`OP-colnames_df_0` = 1); sync_code(session)
    session$setInputs(`OP-comma` = 1); sync_code(session)
    current_string <- session$userData$export_code_string
    session$setInputs(`OP-editable_code` = paste0(current_string, "conc > 700"))
    session$setInputs(`OP-bracket_close` = 1); sync_code(session)
    current_string <- session$userData$export_code_string

    session$setInputs(`OP-iv` = "df_new2"); sync_code(session)
    session$setInputs(`OP-run_op_intermediate` = 2)

    ex[[2]] <<- session$userData$export_iv[[3]]
  })
  tinytest::expect_equal(ex, expected)
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
    session$setInputs(`OP-c` = 1)
    current_string <- session$userData$export_code_string
    session$setInputs(`OP-editable_code` = paste(current_string, "'a'"));
    session$setInputs(`OP-bracket_close` = 1); sync_code(session)
    session$setInputs(`OP-iv` = "a");
    session$setInputs(`OP-run_op_intermediate` = 1);
    session$userData$export_code_string <- ""; sync_code(session)

    session$setInputs(`OP-c` = 1)
    current_string <- session$userData$export_code_string
    session$setInputs(`OP-editable_code` = paste(current_string, "'b'"));
    session$setInputs(`OP-bracket_close` = 1); sync_code(session)
    session$setInputs(`OP-iv` = "b");
    session$setInputs(`OP-run_op_intermediate` = 1);
    session$userData$export_code_string <- ""; sync_code(session)

    session$setInputs(`OP-paste` = 1); sync_code(session)
    session$setInputs(`OP-intermediate_vars_a_0` = 1); sync_code(session)
    session$setInputs(`OP-comma` = 1); sync_code(session)
    session$setInputs(`OP-intermediate_vars_b_0` = 1); sync_code(session)
    session$setInputs(`OP-bracket_close` = 1); sync_code(session)
    session$setInputs(`OP-iv` = "res1");
    session$setInputs(`OP-run_op_intermediate` = 1);
    session$userData$export_code_string <- ""; sync_code(session)
    ex[[1]] <<- session$userData$export_iv[[4]]

    session$setInputs(`OP-paste0` = 1); sync_code(session)
    session$setInputs(`OP-intermediate_vars_a_0` = 1); sync_code(session)
    session$setInputs(`OP-comma` = 1); sync_code(session)
    session$setInputs(`OP-intermediate_vars_b_0` = 1); sync_code(session)
    session$setInputs(`OP-bracket_close` = 1); sync_code(session)
    session$setInputs(`OP-iv` = "res2");
    session$setInputs(`OP-run_op_intermediate` = 1);
    session$userData$export_code_string <- ""; sync_code(session)
    ex[[2]] <<- session$userData$export_iv[[5]]

    session$setInputs(`OP-toupper` = 1); sync_code(session)
    session$setInputs(`OP-intermediate_vars_a_0` = 1); sync_code(session)
    session$setInputs(`OP-bracket_close` = 1); sync_code(session)
    session$setInputs(`OP-iv` = "res3");
    session$setInputs(`OP-run_op_intermediate` = 1);
    session$userData$export_code_string <- ""; sync_code(session)
    ex[[3]] <<- session$userData$export_iv[[6]]

    session$setInputs(`OP-tolower` = 1); sync_code(session)
    session$setInputs(`OP-intermediate_vars_res3_0` = 1); sync_code(session)
    session$setInputs(`OP-bracket_close` = 1); sync_code(session)
    session$setInputs(`OP-iv` = "res4");
    session$setInputs(`OP-run_op_intermediate` = 1);
    session$userData$export_code_string <- ""; sync_code(session)
    ex[[4]] <<- session$userData$export_iv[[7]]

  })
  tinytest::expect_equal(ex, expected)
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
  functions <- c(
    "OP-as_int", "OP-as_real", "OP-as_fact", "OP-as_char"
  )
  for (i in seq_len(length(functions))) {
    f <- functions[[i]]
    shiny::testServer(srv, {
      DataModelState$df <- CO2
      ResultsState$all_data <- list(df = CO2)
      DataModelState$active_df_name <- "df"
      DataWranglingState$df_name <- "df"
      DataWranglingState$df <- CO2
      DataWranglingState$intermediate_vars <- list(df = CO2)

      session$setInputs(active_tab = "DataWrangling")

      session$flushReact()
      session$setInputs(`OP-c` = 1)
      current_string <- session$userData$export_code_string
      session$setInputs(`OP-editable_code` = paste0(current_string, "'10.5', '1.4', '1.3', 3.14"))
      session$setInputs(`OP-bracket_close` = 1); sync_code(session)
      session$setInputs(`OP-iv` = "a");
      session$setInputs(`OP-run_op_intermediate` = 1);
      session$userData$export_code_string <- ""; sync_code(session)

      do.call(session$setInputs, setNames(list(1), f))
      session$setInputs(f = 1); sync_code(session)
      session$setInputs(`OP-intermediate_vars_a_0` = 1); sync_code(session)
      session$setInputs(`OP-bracket_close` = 1); sync_code(session)

      session$setInputs(`OP-iv` = "temp"); sync_code(session)
      session$setInputs(`OP-run_op_intermediate` = 1)
      ex[[i]] <<- session$userData$export_iv[[3]]
    })
  }
  tinytest::expect_equal(ex, expected)
}
test_casts(app, srv)
