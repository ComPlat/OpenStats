coverage_test <- nzchar(Sys.getenv("R_COVR"))

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

app <- OpenStats:::app()
srv <- app$server

test_main_server <- function(app, srv) {
  options(OpenStats.background = FALSE)
  checks <- logical(2)
  shiny::testServer(srv, {
    tmp <- tempfile(fileext = ".csv")
    utils::write.csv(as.data.frame(CO2), tmp, row.names = FALSE)
    session$setInputs(conditionedPanels = "Data")
    session$setInputs(file = data.frame(name="CO2.csv", size=file.info(tmp)$size,
      type="text/csv", datapath=tmp))
    session$flushReact()
    checks[1] <<- is.data.frame(DataModelState$df)
    checks[2] <<- length(ResultsState$history) == 1 
  })
  expect_true(all(checks))
}
test_main_server(app, srv)

test_main_server2 <- function(app, srv) {
  checks <- logical(1)
  shiny::testServer(srv, {
    DataModelState$df <- as.data.frame(CO2)
    ResultsState$all_data <- list(df = DataModelState$df, df2 = head(DataModelState$df))
    session$setInputs(`tables-dropdown` = "df2")
    session$flushReact()
    checks[1] <<- (DataModelState$active_df_name == "df2")
  })
  expect_true(all(checks))
}
test_main_server2(app, srv)
