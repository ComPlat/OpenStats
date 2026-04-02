coverage_test <- nzchar(Sys.getenv("R_COVR"))
run_test <- function(f) {
  if (coverage_test) f(app, srv, FALSE) else f(app, srv, TRUE)
}

coverage_test <- nzchar(Sys.getenv("R_COVR"))

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

app <- OpenStats:::app()
srv <- app$server

run_stats_test <- function(method, CO2) {
  if (method == "TESTS-aovTest") {
    expected <- broom::tidy(aov(uptake ~ conc * Treatment + Type, data = CO2))
    expected <- cbind(expected, row.names(expected))
    names(expected)[ncol(expected)] <- paste0("conc * Treatment + Type", collapse = ".")
    expected
  } else if (method == "TESTS-kruskalTest") {
    expected <- broom::tidy(kruskal.test(uptake ~ conc, data = CO2))
    expected <- cbind(expected, row.names(expected))
    names(expected)[ncol(expected)] <- paste0("conc")
    expected
  } else if (method == "HSD") {
    aov_res <- aov(uptake ~ conc, data = CO2)
    expected <- agricolae::HSD.test(aov_res,
      trt = "conc",
      alpha = 0.05, group = TRUE, unbalanced = FALSE
    )$groups
    expected <- cbind(expected, row.names(expected))
    names(expected)[ncol(expected)] <- paste0("conc")
    expected
  } else if (method == "kruskalTest") {
    expected <- with(CO2, agricolae::kruskal(CO2[, "uptake"], CO2[, "conc"]),
      alpha = 0.05, p.adj = "holm", group = TRUE
    )$groups
    names(expected)[1] <- "uptake"
    expected <- cbind(expected, row.names(expected))
    names(expected)[ncol(expected)] <- paste0("conc")
    expected
  } else if (method == "LSD") {
    aov_res <- aov(uptake ~ conc, data = CO2)
    expected <- agricolae::LSD.test(aov_res,
      trt = "conc",
      alpha = 0.05, p.adj = "holm", group = TRUE
    )$groups
    expected <- cbind(expected, row.names(expected))
    names(expected)[ncol(expected)] <- paste0("conc")
    expected
  } else if (method == "scheffe") {
    aov_res <- aov(uptake ~ conc, data = CO2)
    expected <- agricolae::scheffe.test(
      aov_res,
      trt = "conc", alpha = 0.05, group = TRUE
    )$groups
    expected <- cbind(expected, row.names(expected))
    names(expected)[ncol(expected)] <- paste0("conc")
    expected
  } else if (method == "REGW") {
    aov_res <- aov(uptake ~ conc, data = CO2)
    expected <- agricolae::REGW.test(
      aov_res,
      trt = "conc", alpha = 0.05, group = TRUE
    )$groups
    expected <- cbind(expected, row.names(expected))
    names(expected)[ncol(expected)] <- paste0("conc")
    expected
  }
}

choices <- c("TESTS-aovTest", "TESTS-kruskalTest", "HSD", "kruskalTest", "LSD", "scheffe", "REGW")
for (choice in choices) {
  test_stats_tests <- function(app, srv, in_background) {
    options(OpenStats.background = in_background)

    CO2$Treatment <- as.factor(CO2$Treatment)
    CO2$Type <- as.factor(CO2$Type)
    CO2$conc <- as.factor(CO2$conc)
    expected <- try(run_stats_test(choice, CO2))

    ib <- getOption("OpenStats.background", TRUE)
    got <- NULL
    shiny::testServer(srv, {
      DataModelState$df      <- CO2
      if (choice == "TESTS-aovTest") {
        DataModelState$formula <- new("LinearFormula", formula = uptake ~ conc * Treatment + Type)
      } else {
        DataModelState$formula <- new("LinearFormula", formula = uptake ~ conc)
      }

      session$flushReact()

      counter <- ResultsState$counter

      session$setInputs(`TESTS-pval` = 0.05)
      session$setInputs(`TESTS-design` = "ba")
      session$setInputs(`TESTS-padj` = "holm")
      if (choice %in% c("TESTS-aovTest", "TESTS-kruskalTest")) {
        do.call(session$setInputs, setNames(list(2), choice))
        session$setInputs(choice = 1)
      } else {
        session$setInputs(`TESTS-PostHocTests` = choice)
        session$setInputs(`TESTS-PostHocTest` = 1)
      }

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
        if (!is.null(ex) && (counter < ResultsState$counter)) break

        if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 20)
        stop(paste("timeout; have keys:", paste(names(ex %||% list()), collapse = ",")))

        Sys.sleep(0.05)
      }

      got <<- session$userData$export[[ResultsState$counter]]
      attr(got, "rendered") <<- NULL
    })
    expect_equal(expected, got)
  }
  run_test(test_stats_tests)
}
