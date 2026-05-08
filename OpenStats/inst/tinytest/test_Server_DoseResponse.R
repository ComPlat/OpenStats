df_continous <- read.csv(system.file("/test_data/DoseResponse.csv", package = "OpenStats"))
df_binomial <- read.csv(system.file("/test_data/DoseResponseBinomial.csv", package = "OpenStats"))
names(df_binomial)[c(1, 4)] <- c("names", "units")
df_poisson <- read.csv(system.file("/test_data/DoseResponsePoisson.csv", package = "OpenStats"))
names(df_poisson)[c(1, 4)] <- c("names", "units")

# Expected
# =================================================================
expected_continous <- OpenStats:::ic(
  df_continous, 50, "abs", "conc",
  "names", "units",
  FALSE, FALSE, "continuous"
)
dfs <- lapply(expected_continous, function(x) {
  if (is.list(x)) {
    return(x[[1]])
  }
})
expected_continous <- do.call(rbind, dfs)

expected_binomial <- OpenStats:::ic(
  df_binomial, 50, "abs", "conc",
  "names", "units",
  FALSE, FALSE, "binomial"
)
dfs <- lapply(expected_binomial, function(x) {
  if (is.list(x)) {
    return(x[[1]])
  }
})
expected_binomial <- do.call(rbind, dfs)

expected_poisson <- OpenStats:::ic(
  df_poisson, 50, "abs", "conc", "names", "units",
  FALSE, FALSE, "Poisson"
)
dfs <- lapply(expected_poisson, function(x) {
  if (is.list(x)) {
    return(x[[1]])
  }
})
expected_poisson <- do.call(rbind, dfs)

# Helper
# =================================================================
sync_code <- function(session) {
  session$setInputs(`OP-editable_code` = session$userData$export_code_string)
  session$flushReact()
}

coverage_test <- nzchar(Sys.getenv("R_COVR"))

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

app <- OpenStats:::app()
srv <- app$server

# dose response in server
# =================================================================
test_dose_response <- function(app, srv, type, df) {
  options(OpenStats.background = FALSE)
  ex <- NULL
  shiny::testServer(srv, {
    DataModelState$df <- df
    DataModelState$formula <- new("LinearFormula", formula = abs ~ conc)
    session$setInputs(`DOSERESPONSE-ic_percentage` = 50)
    session$setInputs(`DOSERESPONSE-substanceNames` = "names")
    session$setInputs(`DOSERESPONSE-unitNames` = "units")
    session$setInputs(`DOSERESPONSE-yTransform` = FALSE)
    session$setInputs(`DOSERESPONSE-xTransform` = FALSE)
    session$setInputs(`DOSERESPONSE-type` = type)
    session$setInputs(`DOSERESPONSE-ic50` = 1)

    t0 <- Sys.time()
    l0 <- length(ResultsState$all_data)
    repeat {
      ResultsState$bgp$tick(ResultsState, DataModelState, DataWranglingState)
      session$flushReact()
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 30) break
      Sys.sleep(0.05)
    }
    ex <<- session$userData$export
  })
  ex
}

# compare it
# =================================================================
VERSION <- 1.2
res <- test_dose_response(app, srv, "continuous", df_continous)
res_df <- res[[1]]@df
equal <- Map(function(a, b) {
  a <- a[!is.na(a)]
  b <- b[!is.na(b)]
  all(a == b)
}, res_df, expected_continous) |> unlist() |> all()
expect_true(equal)

# binomial
res <- test_dose_response(app, srv, "binomial", df_binomial)
res_df <- res[[1]]@df
equal <- Map(function(a, b) {
  a <- a[!is.na(a)]
  b <- b[!is.na(b)]
  all(a == b)
}, res_df, expected_binomial) |> unlist() |> all()
expect_true(equal)

# poisson
res <- test_dose_response(app, srv, "Poisson", df_poisson)
res_df <- res[[1]]@df
equal <- Map(function(a, b) {
  a <- a[!is.na(a)]
  b <- b[!is.na(b)]
  all(a == b)
}, res_df, expected_poisson) |> unlist() |> all()
expect_true(equal)
