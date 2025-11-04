coverage_test <- nzchar(Sys.getenv("R_COVR"))
run_test <- function(f) {
  if (coverage_test) f(app, srv, FALSE) else f(app, srv, TRUE)
}

sync_code <- function(session) {
  session$setInputs(`OP-editable_code` = session$userData$export_code_string)
  session$flushReact()
}

coverage_test <- nzchar(Sys.getenv("R_COVR"))

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

app <- OpenStats:::app()
srv <- app$server

run_posthoc_glm <- function(method) {
  family <- "Gamma"
  link_fct <- "inverse"
  family <- str2lang(paste0("stats::", family, "(\"", link_fct, "\")"))
  formula <- uptake ~ conc * Treatment + Type
  f_split <- OpenStats:::split_formula(formula)
  rhs_vars <- OpenStats:::vars_rhs(f_split$right_site)
  df_temp <- OpenStats:::num_to_factor(CO2, rhs_vars)
  if (any(apply(CO2, 2, is.numeric))) {
    warning(paste0("Found numeric predictors and converted them to factors"))
  }
  model <- glm(formula, data = df_temp, family = eval(family))
  emm <- emmeans::emmeans(model, rhs_vars)
  fit <- pairs(emm, adjust = method)
  as.data.frame(fit)
}

choices <- c(
  "tukey",
  "sidak",
  "bonferroni",
  "scheffe",
  "none",
  "fdr",
  "holm",
  "hochberg",
  "hommel"
)

for (choice in choices) {
  test_glm_stats_tests <- function(app, srv, in_background) {
    options(OpenStats.background = in_background)

    CO2$Treatment <- as.factor(CO2$Treatment)
    CO2$Type <- as.factor(CO2$Type)
    CO2$conc <- as.factor(CO2$conc)
    expected <- run_posthoc_glm(choice)

    ib <- getOption("OpenStats.background", TRUE)
    got <- NULL
    shiny::testServer(srv, {
      DataModelState$df      <- CO2
      DataModelState$formula <- new(
        "GeneralisedLinearFormula",
        formula = uptake ~ conc * Treatment + Type,
        family = "Gamma", link_fct = "inverse"
      )
      session$flushReact()

      counter <- ResultsState$counter

      session$setInputs(`TESTS-PostHocEmmeans` = choice)
      session$setInputs(`TESTS-PostHocEmmeansTest` = 1)
      session$setInputs(`TESTS-padj` = "holm")

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
    print(tinytest::expect_equal(expected, got))
  }
  run_test(test_glm_stats_tests)
}
