coverage_test <- nzchar(Sys.getenv("R_COVR"))
run_test <- function(f) {
  if (coverage_test) f(app, srv, FALSE) else f(app, srv, TRUE)
}

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

app <- OpenStats:::app()
srv <- app$server

test_shapiro_on_data <- function(app, srv) {
  options(OpenStats.background = FALSE)
  expected <- rbind(
    broom::tidy(shapiro.test(CO2[CO2$Treatment == "nonchilled","uptake"])),
    broom::tidy(shapiro.test(CO2[CO2$Treatment == "chilled","uptake"]))
  )
  expected$variable <- c("nonchilled","chilled")
  expected$`Normal distributed` <- expected$p.value > 0.05
  ex <- NULL
  shiny::testServer(srv, {
    DataModelState$df      <- CO2
    DataModelState$formula <- new("LinearFormula", formula = uptake ~ Treatment)

    session$flushReact()
    session$setInputs(`ASS-shapiro` = 1)

    ex <<- session$userData$export[[1]]
    attr(ex, "rendered") <<- NULL
  })
   expect_equal(ex, expected)
}
test_shapiro_on_data(app, srv)

test_shapiro_residuals <- function(app, srv) {
  options(OpenStats.background = FALSE)
  fit <- lm(uptake ~ Treatment, data = CO2)
  r <- resid(fit)
  expected <- broom::tidy(shapiro.test(r))
  expected$`Residuals normal distributed` <- expected$p.value > 0.05
  ex <- NULL
  shiny::testServer(srv, {
    DataModelState$df      <- CO2
    DataModelState$formula <- new("LinearFormula", formula = uptake ~ Treatment)

    session$flushReact()
    session$setInputs(`ASS-shapiroResiduals` = 1)

    ex <<- session$userData$export[[1]]
    attr(ex, "rendered") <<- NULL
  })
   expect_equal(ex, expected)
}
test_shapiro_residuals(app, srv)

test_levene <- function(app, srv) {
  options(OpenStats.background = FALSE)
  expected <- broom::tidy(car::leveneTest(uptake ~ Treatment,
    data = CO2, center = "mean"
  ))
  expected$`Variance homogenity` <- expected$p.value > 0.05
  ex <- NULL
  shiny::testServer(srv, {
    DataModelState$df      <- CO2
    DataModelState$formula <- new("LinearFormula", formula = uptake ~ Treatment)

    session$flushReact()
    session$setInputs(`ASS-center` = "mean")
    session$setInputs(`ASS-levene` = 1)

    ex <<- session$userData$export[[1]]
    attr(ex, "rendered") <<- NULL
  })
   expect_equal(ex, expected)
}
test_levene(app, srv)

test_diagnose_plot <- function(app, srv, in_background) {
  options(OpenStats.background = in_background)
  ib <- getOption("OpenStats.background", TRUE)
  got <- NULL
  shiny::testServer(srv, {
    DataModelState$df      <- CO2
    DataModelState$formula <- new("LinearFormula", formula = uptake ~ Treatment)
    session$flushReact()

    res_name <- sprintf("%d Diagnostic plot", ResultsState$counter + 1)

    session$setInputs(`ASS-DiagnosticPlot` = 1)

    t0 <- Sys.time()
    l0 <- length(ResultsState$all_data)
    repeat {
      ResultsState$bgp$tick(ResultsState, DataModelState, DataWranglingState)
      session$flushReact()

      if (nzchar(ResultsState$bgp$running_status) &&
          grepl("Error|Canceled", ResultsState$bgp$running_status, ignore.case = TRUE)) {
        stop(paste("bgp:", ResultsState$bgp$running_status))
      }

      ex <- session$userData$export
      if (!is.null(ex) && res_name %in% names(ex)) break

      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 30)
        stop(paste("timeout; have keys:", paste(names(ex %||% list()), collapse = ",")))

      Sys.sleep(0.05)
    }

    got <<- session$userData$export[[res_name]]
    attr(got, "rendered") <<- NULL
  })
   expect_true(inherits(got, "plot"))
}
# run_test(test_diagnose_plot)
