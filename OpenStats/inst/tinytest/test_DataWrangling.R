if (!identical(Sys.getenv("NOT_CRAN"), "true")) exit_file("Skip on CRAN")
if (!identical(Sys.getenv("RUN_UI_TESTS"), "true")) exit_file("UI tests disabled")

library(shinytest2)
library(tinytest)
library(OpenStats)

trim <- function(s) {
  gsub("\t|\n| ", "", s)
}

wait <- function(app) {
  try(app$wait_for_idle(), silent = TRUE)
}

reset <- function(app) {
  app$set_inputs(`OP-editable_code` = "")
  wait(app)
}

app <- OpenStats:::app()
# Create app
app <- shiny::shinyApp(app$ui, app$server)
app <- AppDriver$new(app)
wait(app)
app$upload_file(
  file = system.file("/test_data/CO2.csv", package = "OpenStats")
)
wait(app)
app$set_inputs(`conditionedPanels` = "DataWrangling")
wait(app)

# Seq tests
# =================================================================
app$click("OP-seq")
wait(app)
content <- app$get_values()$input[["OP-editable_code"]]
app$set_inputs(
  `OP-editable_code` = paste0( content, "1, 100, 1")
)
app$click("OP-bracket_close")
wait(app)
app$set_inputs(`OP-iv` = "Seq")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
content <- app$get_values()$input[["OP-editable_code"]] |> trim()
expect_equal(content, "Seq(1,100,1)")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$Seq, seq(1, 100, 1))

# dataframe tests
# =================================================================
reset(app)
app$click("OP-df")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-comma")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
content <- app$get_values()$input[["OP-editable_code"]] |> trim()
wait(app)
app$set_inputs(`OP-iv` = "df_new")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
content <- app$get_values()$input[["OP-editable_code"]] |> trim()
expect_equal(content, "DataFrame(conc,conc)")
iv_list <- app$get_values()$export[["OP-iv_list"]]
df_new <- data.frame(CO2$conc, CO2$conc)
names(df_new) <- c("conc", "conc")
expect_equal(iv_list$df_new, df_new)
reset(app)

# random tests
# =================================================================
random_funcs <- c(
  "dnorm", "pnorm", "qnorm",
  "dbinom", "pbinom", "qbinom",
  "dpois", "ppois",
  "dunif", "punif", "qunif"
)
args <- list(
  0, 0, 0.2,
  "0, 10, 0.5", "0, 10, 0.5", "0, 10, 0.5",
  "0, 2", "0, 2",
  "0, 0, 2", "0.2, 0, 3", "0.2, 0, 3"
)

checks <- c()
for (i in seq_along(random_funcs)) {
  expr <- paste0(random_funcs[i], "(", args[[i]], ")")
  res <- eval(parse(text = expr))
  func <- paste0("OP-", random_funcs[i])
  app$set_inputs(`OP-editable_code` = "")
  wait(app)
  app$click(paste0(func))
  wait(app)
  app$set_inputs(
    `OP-editable_code` = paste0(app$get_values()$input[["OP-editable_code"]], args[[i]])
  )
  wait(app)
  app$click("OP-bracket_close")
  wait(app)
  app$set_inputs(`OP-iv` = func)
  wait(app)
  app$click("OP-run_op_intermediate")
  wait(app)
  iv_list <- app$get_values()$export[["OP-iv_list"]]
  output <- iv_list[[make.names(func)]]
  checks <- c(checks, expect_equal(output, res))
}
expect_true(all(checks))

# NOTE: this is necessary as no seed can be set
validate_distribution <- function(values, dist, ...) {
  if (dist == "normal") {
    one <- expect_true(abs(mean(values) - 0) < 0.1, info = "Normal mean should be close to 0")
    two <- expect_true(abs(sd(values) - 1) < 0.1, info = "Normal sd should be close to 1")
    return(all(one, two))
  } else if (dist == "uniform") {
    one <- expect_true(all(values >= 0 & values <= 1), info = "Uniform values should be in [0,1]")
    two <- expect_true(abs(mean(values) - 0.5) < 0.1, info = "Uniform mean should be close to 0.5")
    return(all(one, two))
  } else if (dist == "pois") {
    one <- expect_true(abs(mean(values) - 2) < 0.1, info = "Uniform mean should be close to 0.5")
    return(one)
    # NOTE: set lambda to 2
  } else if (dist == "binom") {
    # Assuming size = 10 trials, and probability of success = 0.5
    n_trials <- 10
    prob_success <- 0.5
    one <- expect_true(abs(mean(values) - (n_trials * prob_success)) < 0.5,
      info = "Binomial mean should be close to n * p"
    )
    two <- expect_true(abs(var(values) - (n_trials * prob_success * (1 - prob_success))) < 0.5,
      info = "Binomial variance should be close to n * p * (1 - p)"
    )
    return(all(one, two))
  }
}
random_funcs <- c(
  "norm", "unif", "pois", "binom"
)
args <- list(
  "10000", "10001", "10002, 2", "10000, 10, 0.5"
)
checks <- c()
for (i in seq_along(random_funcs)) {
  func <- paste0("OP-r", random_funcs[i])
  reset(app)
  wait(app)
  app$click(paste0(func))
  wait(app)
  app$set_inputs(
    `OP-editable_code` =
      paste0(app$get_values()$input[["OP-editable_code"]], args[[i]])
  )
  wait(app)
  app$click("OP-bracket_close")
  wait(app)
  app$set_inputs(`OP-iv` = func)
  wait(app)
  app$click("OP-run_op_intermediate")
  wait(app)
  iv_list <- app$get_values()$export[["OP-iv_list"]]
  wait(app)
  output <- iv_list[[make.names(func)]]
  checks <- c(checks, validate_distribution(output, random_funcs[i]))
}
expect_true(all(checks))
reset(app)

# Test arithmetic operations
# =================================================================
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-add")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$set_inputs(`OP-iv` = "Plus")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
content <- app$get_values()$input[["OP-editable_code"]] |> trim()
expect_equal(content, "conc+conc")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$Plus, CO2$conc + CO2$conc)

reset(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-sub")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$set_inputs(`OP-iv` = "MINUS")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
content <- app$get_values()$input[["OP-editable_code"]] |> trim()
expect_equal(content, "conc-conc")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$MINUS, CO2$conc - CO2$conc)

reset(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-div")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$set_inputs(`OP-iv` = "DIVIDE")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
content <- app$get_values()$input[["OP-editable_code"]] |> trim()
expect_equal(content, "conc/conc")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$DIVIDE, CO2$conc / CO2$conc)

reset(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-mul")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$set_inputs(`OP-iv` = "MULTIPLY")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
content <- app$get_values()$input[["OP-editable_code"]] |> trim()
expect_equal(content, "conc*conc")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$MULTIPLY, CO2$conc * CO2$conc)

reset(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-add")
wait(app)
app$click("OP-bracket_open")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-mul")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$set_inputs(`OP-iv` = "ARITHMETIC")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
content <- app$get_values()$input[["OP-editable_code"]] |> trim()
expect_equal(content, "conc+(conc*conc)")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$ARITHMETIC, CO2$conc + (CO2$conc * CO2$conc))

reset(app)
app$click("OP-get_elem")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-comma")
wait(app)
app$set_inputs(`OP-editable_code` = paste0(app$get_values()$input[["OP-editable_code"]], " 1"))
app$click("OP-bracket_close")
app$set_inputs(`OP-iv` = "GET_ELEM_AND_COMMA")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
content <- app$get_values()$input[["OP-editable_code"]] |> trim()
expect_equal(content, "get_elem(conc,1)")
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$GET_ELEM_AND_COMMA, CO2$conc[1])

# Test math functions
# =================================================================
reset(app)
app$click("OP-log")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-add")
wait(app)
app$click("OP-log10")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-add")
wait(app)
app$click("OP-sqrt")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-add")
wait(app)
app$click("OP-exp")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-bracket_close")
app$set_inputs(`OP-iv` = "MATH1")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
content <- app$get_values()$input[["OP-editable_code"]] |> trim()
expect_equal(content, "log(conc)+log10(conc)+sqrt(conc)+exp(conc)")
iv_list <- app$get_values()$export[["OP-iv_list"]]
conc <- CO2$conc
expect_equal(iv_list$MATH1, log(conc) + log10(conc) + sqrt(conc) + exp(conc))

reset(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-exponent")
wait(app)
app$set_inputs(
  `OP-editable_code` = paste0(app$get_values()$input[["OP-editable_code"]], " 2")
)
app$click("OP-bracket_close")
wait(app)
app$click("OP-add")
wait(app)
app$click("OP-sin")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-add")
wait(app)
app$click("OP-cos")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-add")
wait(app)
app$click("OP-tan")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-add")
wait(app)
app$click("OP-sinh")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-add")
wait(app)
app$click("OP-cosh")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-add")
wait(app)
app$click("OP-tanh")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$set_inputs(`OP-iv` = "MATH2")
wait(app)
app$click("OP-run_op_intermediate")
content <- app$get_values()$input[["OP-editable_code"]] |> trim()
expect_equal(
  content,
  "conc^(2)+sin(conc)+cos(conc)+tan(conc)+sinh(conc)+cosh(conc)+tanh(conc)"
)
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list$MATH2, eval(parse(text = content)))


app$set_inputs(`OP-editable_code` = "1")
app$set_inputs(`OP-iv` = "One")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
reset(app)
app$click("OP-asin")
wait(app)
app$click("OP-intermediate_vars_One_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-add")
wait(app)
app$click("OP-acos")
wait(app)
app$click("OP-intermediate_vars_One_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-add")
wait(app)
app$click("OP-atan")
wait(app)
app$click("OP-intermediate_vars_One_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$set_inputs(`OP-iv` = "MATH3")
wait(app)
content <- app$get_values()$input[["OP-editable_code"]] |> trim()
app$click("OP-run_op_intermediate")
expect_equal(
  content,
  "asin(One)+acos(One)+atan(One)"
)
iv_list <- app$get_values()$export[["OP-iv_list"]]
One <- 1
expect_equal(iv_list$MATH3, eval(parse(text = content)))


app$set_inputs(`OP-editable_code` = "-1.5")
wait(app)
app$set_inputs(`OP-iv` = "Num")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
reset(app)
app$click("OP-abs")
wait(app)
app$click("OP-intermediate_vars_Num_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-add")
app$click("OP-ceil")
wait(app)
app$click("OP-intermediate_vars_Num_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-add")
app$click("OP-floor")
wait(app)
app$click("OP-intermediate_vars_Num_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-add")
wait(app)
app$click("OP-trunc")
wait(app)
app$click("OP-intermediate_vars_Num_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-add")
wait(app)
app$click("OP-round")
wait(app)
app$click("OP-intermediate_vars_Num_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$set_inputs(`OP-iv` = "MATH4")
wait(app)
content <- app$get_values()$input[["OP-editable_code"]] |> trim()
wait(app)
app$click("OP-run_op_intermediate")
expect_equal(
  content, "abs(Num)+ceiling(Num)+floor(Num)+trunc(Num)+round(Num)"
)
iv_list <- app$get_values()$export[["OP-iv_list"]]
Num <- -1.5
expect_equal(iv_list$MATH4, eval(parse(text = content)))

# Test comparison operations
# =================================================================
reset(app)
app$set_inputs(`OP-editable_code` = "C(1, 2, 2, 2)")
wait(app)
app$set_inputs(`OP-iv` = "Vec1")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
app$set_inputs(`OP-editable_code` = "C(1, 2, 3, 4)")
wait(app)
app$set_inputs(`OP-iv` = "Vec2")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
Vec1 <- c(1, 2, 2, 2)
Vec2 <- c(1, 2, 3, 4)
operations <- c(
  "OP-eq", "OP-not_eq", "OP-larger",
  "OP-smaller", "OP-larger_eq", "OP-smaller_eq"
)
checks <- c()
for (i in operations) {
  app$set_inputs(`OP-editable_code` = "")
  wait(app)
  app$click("OP-intermediate_vars_Vec1_0")
  wait(app)
  app$click(i)
  wait(app)
  app$click("OP-intermediate_vars_Vec2_0")
  wait(app)
  app$set_inputs(`OP-iv` = i)
  wait(app)
  content <- app$get_values()$input[["OP-editable_code"]]
  wait(app)
  app$click("OP-run_op_intermediate")
  wait(app)
  iv_list <- app$get_values()$export[["OP-iv_list"]]
  checks <- c(checks, expect_equal(iv_list[[make.names(i)]], eval(parse(text = content))))
}
expect_true(all(checks))

# Test statistical operations and utilities
# =================================================================
operations <- c(
  "OP-mean", "OP-median", "OP-min",
  "OP-max", "OP-sum", "OP-sd"
)
Mean <- OpenStats:::Mean
Median <- OpenStats:::Median
Min <- OpenStats:::Min
Max <- OpenStats:::Max
Sum <- OpenStats:::Sum
SD <- OpenStats:::SD
conc <- CO2$conc
checks <- c()
for (i in operations) {
  reset(app)
  app$click(i)
  wait(app)
  app$set_inputs(`OP-iv` = i)
  wait(app)
  app$click("OP-colnames_conc_0")
  wait(app)
  app$click("OP-bracket_close")
  wait(app)
  content <- app$get_values()$input[["OP-editable_code"]] |> trim()
  wait(app)
  app$click("OP-run_op_intermediate")
  wait(app)
  iv_list <- app$get_values()$export[["OP-iv_list"]]
  checks <- c(checks, expect_equal(iv_list[[make.names(i)]], eval(parse(text = content))))
}
expect_true(all(checks))

reset(app)
app$click("OP-get_rows")
wait(app)
app$set_inputs(`OP-iv` = "get_rows")
wait(app)
app$click("OP-colnames_df_0")
wait(app)
app$click("OP-comma")
wait(app)
app$set_inputs(`OP-editable_code` = paste0(app$get_values()$input[["OP-editable_code"]], " conc == 95"))
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
iv_list <- app$get_values()$export[["OP-iv_list"]]
expect_equal(iv_list[["get_rows"]]$uptake, CO2[conc == 95, "uptake"])

reset(app)
app$click("OP-get_cols")
wait(app)
app$set_inputs(`OP-iv` = "get_cols")
wait(app)
app$click("OP-colnames_df_0")
wait(app)
app$click("OP-comma")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-comma")
wait(app)
app$click("OP-colnames_conc_0")
wait(app)
app$click("OP-comma")
wait(app)
app$click("OP-colnames_uptake_0")
wait(app)
app$click("OP-bracket_close")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
iv_list <- app$get_values()$export[["OP-iv_list"]]
wait(app)
expect_true(all(iv_list[["get_cols"]] == CO2[, c("conc", "conc", "uptake")]))

# Test string functions
# =================================================================
app$set_inputs(`OP-editable_code` = 'C("A", "B", "C")')
wait(app)
app$set_inputs(`OP-iv` = "S1")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
app$set_inputs(`OP-editable_code` = 'C("d", "e", "f")')
wait(app)
app$set_inputs(`OP-iv` = "S2")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
S1 <- c("A", "B", "C")
S2 <- c("d", "e", "f")
operations <- c(
  "OP-paste", "OP-paste0"
)
checks <- c()
for (i in operations) {
  app$set_inputs(`OP-editable_code` = "")
  wait(app)
  app$click(i)
  wait(app)
  app$click("OP-intermediate_vars_S1_0")
  wait(app)
  app$click("OP-comma")
  wait(app)
  app$click("OP-intermediate_vars_S2_0")
  wait(app)
  app$click("OP-bracket_close")
  wait(app)
  app$set_inputs(`OP-iv` = i)
  wait(app)
  content <- app$get_values()$input[["OP-editable_code"]]
  wait(app)
  app$click("OP-run_op_intermediate")
  wait(app)
  iv_list <- app$get_values()$export[["OP-iv_list"]]
  checks <- c(checks, expect_equal(iv_list[[make.names(i)]], eval(parse(text = content))))
}
expect_true(all(checks))

operations <- c(
  "OP-tolower", "OP-toupper"
)
checks <- c()
for (i in operations) {
  reset(app)
  wait(app)
  app$click(i)
  wait(app)
  app$click("OP-intermediate_vars_S1_0")
  wait(app)
  app$click("OP-bracket_close")
  wait(app)
  app$set_inputs(`OP-iv` = i)
  wait(app)
  content <- app$get_values()$input[["OP-editable_code"]]
  wait(app)
  app$click("OP-run_op_intermediate")
  wait(app)
  iv_list <- app$get_values()$export[["OP-iv_list"]]
  checks <- c(checks, expect_equal(iv_list[[make.names(i)]], eval(parse(text = content))))
}
expect_true(all(checks))

# Test casts
# =================================================================
app$set_inputs(`OP-editable_code` = 'C("10.5", "1.4", "1.3", 3.14)')
wait(app)
app$set_inputs(`OP-iv` = "S3")
wait(app)
app$click("OP-run_op_intermediate")
wait(app)
S3 <- c("10.5", "1.4", "1.3", 3.14)
operations <- c(
  "OP-as_int", "OP-as_real", "OP-as_fact", "OP-as_char"
)
as.int <- as.integer
as.real <- as.numeric
as.fact <- as.factor
as.char <- as.character
checks <- c()
for (i in operations) {
  app$set_inputs(`OP-editable_code` = "")
  wait(app)
  app$click(i)
  wait(app)
  app$click("OP-intermediate_vars_S3_0")
  wait(app)
  app$click("OP-bracket_close")
  wait(app)
  app$set_inputs(`OP-iv` = i)
  wait(app)
  content <- app$get_values()$input[["OP-editable_code"]]
  wait(app)
  app$click("OP-run_op_intermediate")
  wait(app)
  iv_list <- app$get_values()$export[["OP-iv_list"]]
  checks <- c(checks, expect_equal(iv_list[[make.names(i)]], eval(parse(text = content))))
}
expect_true(all(checks))

wait(app)
app$stop()
