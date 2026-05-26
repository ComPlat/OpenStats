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

# -----------------------------------------------------------------------------------
# Linear mixed model pairwise comparison
# -----------------------------------------------------------------------------------
run_linear_mixed_pairwise <- function(CO2, method, p_val) {
  model <- lmerTest::lmer(uptake ~ conc + (1 | Type), data = CO2)
  trend <- emmeans::emtrends(model, specs = ~ 1, var = "conc")
  trend <- as.data.frame(trend)
  trend$trend_variable <- "conc"
  res <- trend
  rownames(res) <- NULL
  res
}

test_linear_mixed_pairwise <- function(app, srv, in_background) {
  options(OpenStats.background = in_background)

  CO2$Type <- as.factor(CO2$Type)
  expected <- try(run_linear_mixed_pairwise(CO2, "tukey", 0.05))

  got <- NULL
  shiny::testServer(srv, {
    DataModelState$df      <- CO2
    DataModelState$formula <- new("LinearMixedFormula", formula = uptake ~ conc + (1 | Type))

    session$flushReact()

    counter <- ResultsState$counter

    session$setInputs(`TESTS-CorrectionMethod` = "tukey")
    session$setInputs(`TESTS-pval` = 0.05)
    session$setInputs(`TESTS-pairwise_test_linear_mixed` = 1)

    t0 <- Sys.time()
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
run_test(test_linear_mixed_pairwise)

# -----------------------------------------------------------------------------------
# Linear mixed model ANOVA
# -----------------------------------------------------------------------------------
run_linear_mixed_aov <- function(CO2) {
  m <- lmerTest::lmer(uptake ~ conc + (1 | Type), data = CO2)
  broom::tidy(anova(m)) |> as.data.frame()
}

test_linear_mixed_aov <- function(app, srv, in_background) {
  options(OpenStats.background = in_background)

  CO2$Type <- as.factor(CO2$Type)
  expected <- try(run_linear_mixed_aov(CO2))

  got <- NULL
  shiny::testServer(srv, {
    DataModelState$df      <- CO2
    DataModelState$formula <- new("LinearMixedFormula", formula = uptake ~ conc + (1 | Type))

    session$flushReact()

    counter <- ResultsState$counter

    session$setInputs(`TESTS-aovTest` = 1)

    t0 <- Sys.time()
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
run_test(test_linear_mixed_aov)

# -----------------------------------------------------------------------------------
# Linear mixed model permutation ANOVA
# -----------------------------------------------------------------------------------
run_linear_mixed_perm_aov <- function(CO2, perm, seed) {
  set.seed(seed)
  fit <- permutes::perm.lmer(uptake ~ conc + (1 | Type), data = CO2, np = perm, type = "anova")
  as.data.frame(fit)
}

test_linear_mixed_perm_aov <- function(app, srv, in_background) {
  options(OpenStats.background = in_background)

  perm <- 1000L
  seed <- 954388L
  CO2$Type <- as.factor(CO2$Type)
  expected <- try(run_linear_mixed_perm_aov(CO2, perm, seed))

  got <- NULL
  shiny::testServer(srv, {
    DataModelState$df      <- CO2
    DataModelState$formula <- new("LinearMixedFormula", formula = uptake ~ conc + (1 | Type))

    session$flushReact()

    counter <- ResultsState$counter

    session$setInputs(`TESTS-perm` = perm)
    session$setInputs(`TESTS-Seed` = seed)
    session$setInputs(`TESTS-PermANOVATest` = 1)

    t0 <- Sys.time()
    repeat {
      ResultsState$bgp$tick(ResultsState, DataModelState, DataWranglingState)
      session$flushReact()

      if (nzchar(ResultsState$bgp$running_status) &&
        grepl("Error|Canceled", ResultsState$bgp$running_status, ignore.case = TRUE)) {
        stop(paste("bgp:", ResultsState$bgp$running_status))
      }

      ex <- session$userData$export
      if (!is.null(ex) && (counter < ResultsState$counter)) break

      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 60)
        stop(paste("timeout; have keys:", paste(names(ex %||% list()), collapse = ",")))

      Sys.sleep(0.05)
    }

    got <<- session$userData$export[[ResultsState$counter]]
    attr(got, "rendered") <<- NULL
  })
  got <- as.data.frame(got)
  for (i in seq_along(expected)) {
    expect_equal(got[[i]], expected[[i]])
  }
}
run_test(test_linear_mixed_perm_aov)
