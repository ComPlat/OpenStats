df_continous <- read.csv(system.file("/test_data/DoseResponse.csv", package = "OpenStats"))
library(tinytest)
app <- OpenStats:::app()
srv <- app$server
VERSION <- 1.2

test_dose_response <- function(app, srv, type, df) {
  options(OpenStats.background = FALSE)
  ex <- NULL
  shiny::testServer(srv, {
    for (i in 1) {
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
        Sys.sleep(0.005)
      }
      ex <<- session$userData$export
    }
  })
  ex
}

start <- Sys.time()
res <- test_dose_response(app, srv, "continuous", df_continous)
end <- Sys.time()
print(end - start)
