df <- read.csv(system.file("/test_data/DoseResponse.csv", package = "OpenStats"))

# Expected
# =================================================================
expected <- OpenStats:::env_lc_V1_2$ic50(
  df, "abs", "conc",
  "names",
  FALSE, FALSE
)
dfs <- lapply(expected, function(x) {
  if (is.list(x)) {
    return(x[[1]])
  }
})
expected <- do.call(rbind, dfs)

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
test_dose_response <- function(app, srv) {
  options(OpenStats.background = FALSE)
  ex <- NULL
  shiny::testServer(srv, {
    DataModelState$df      <- df
    DataModelState$formula <- new("LinearFormula", formula = abs ~ conc)
    session$setInputs(`DOSERESPONSE-substanceNames` = "names")
    session$setInputs(`DOSERESPONSE-yTransform` = FALSE)
    session$setInputs(`DOSERESPONSE-xTransform` = FALSE)
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
res <- test_dose_response (app, srv)
res_df <- res[[1]]@df

equal <- Map(function(a, b) {
  a <- a[!is.na(a)]
  b <- b[!is.na(b)]
  all(a == b)
}, res_df, expected) |> unlist() |> all()
expect_true(equal)
