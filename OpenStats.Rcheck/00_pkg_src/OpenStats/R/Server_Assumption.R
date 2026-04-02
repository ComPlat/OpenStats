assServer <- function(id, DataModelState, ResultsState) {
  shiny::moduleServer(id, function(input, output, session) {

    # React to model type
    output[["shapiroUI"]] <- shiny::renderUI({
      message <- check_assumptions(DataModelState)
      if (!is.null(message)) {
        return(
          info_div(message)
        )
      }
      if(inherits(DataModelState$formula, "LinearFormula") || inherits(DataModelState$formula, "GeneralisedLinearFormula")) {
        htmltools::div(
          htmltools::h4(htmltools::strong("Test of normal distribution")),
          shiny::actionButton("ASS-shapiro",
            "Shapiro test for individual groups",
            title =
            "Use this test if you have a formula like 'response ~ pred1 * pred2' (two-way ANOVA) to check normality of values within each group."
          ),
          class = "var-box-output"
        )
      }
    })
    output[["shapiroResidualsUI"]] <- shiny::renderUI({
      shiny::req(!is.null(DataModelState$df))
      shiny::req(is.data.frame(DataModelState$df))
      shiny::req(DataModelState$formula)
      if(inherits(DataModelState$formula, "LinearFormula")) {
        htmltools::div(
          htmltools::h4(htmltools::strong("Test of normal distribution")),
          shiny::actionButton("ASS-shapiroResiduals", "Shapiro test for residuals of linear model",
            title =
            "Use this test if you have a formula like 'response ~ predictor1' to check normality of the residuals of the linear model."
          ),
          class = "var-box-output"
        )
      }
    })
    output[["LeveneUI"]] <- shiny::renderUI({
      shiny::req(!is.null(DataModelState$df))
      shiny::req(is.data.frame(DataModelState$df))
      shiny::req(DataModelState$formula)

      if (!is_one_way_aov(DataModelState)) {
        res <- htmltools::div(
          htmltools::h4(htmltools::strong("Test of variance homogenity")),
          info_div("The Levene test can only be applied for formulas with one predictor"),
          class = "var-box-output"
        )
        return(res)
      }
      pred_is_numeric <- is.numeric(DataModelState$df[[DataModelState$formula@formula[[3]] ]]) 
      if (pred_is_numeric) {
        res <- htmltools::div(
          htmltools::h4(htmltools::strong("Test of variance homogenity")),
          info_div("The Levene test can only be applied for formulas with none numeric predictors"),
          class = "var-box-output"
        )
        return(res)
      }

      if(inherits(DataModelState$formula, "LinearFormula")) {
        htmltools::div(
          htmltools::h4(htmltools::strong("Test of variance homogenity")),
          shiny::actionButton(shiny::NS(id, "levene"), "Levene test"),
          shiny::selectInput(shiny::NS(id, "center"), "Data center of each group: mean or median", # The same is true for center
            c(
              "Mean" = "mean",
              "Median" = "median"
            ),
            selectize = FALSE
          ),
          class = "var-box-output"
        )
      }
    })
    output[["DiagnosticPlotUI"]] <- shiny::renderUI({

      if(inherits(DataModelState$formula, "LinearFormula") || inherits(DataModelState$formula, "GeneralisedLinearFormula")) {
        htmltools::div(
          htmltools::h4(htmltools::strong("Visual tests")),
          shiny::actionButton("ASS-DiagnosticPlot", "diagnostic plots"),
          class = "var-box-output"
        )
      }
    })

    runShapiro <- function() {
      df <- DataModelState$df
      print_req(is.data.frame(df), "The dataset is missing")
      print_form(DataModelState$formula)
      res <- try({
        sod <- get_shapiro_on_data()$new(DataModelState$df,DataModelState$formula)
        sod$validate()
        sod$eval(ResultsState)
      })
      if (inherits(res, "try-error")) {
        err <- conditionMessage(attr(res, "condition"))
        print_req(FALSE, err)
      }
    }

    shiny::observeEvent(input$shapiro, {
      runShapiro()
    })

    runShapiroResiduals <- function() {
      df <- DataModelState$df
      print_req(is.data.frame(df), "The dataset is missing")
      print_form(DataModelState$formula)
      res <- try({
        sor <- get_shapiro_on_residuals()$new(DataModelState$df,DataModelState$formula)
        sor$validate()
        sor$eval(ResultsState)
      }, silent = TRUE)

      if (inherits(res, "try-error")) {
        err <- conditionMessage(attr(res, "condition"))
        print_err(err)
      }
    }
    shiny::observeEvent(input$shapiroResiduals, {
      runShapiroResiduals()
    })

    runLevene <- function() {
      df <- DataModelState$df
      print_req(is.data.frame(df), "The dataset is missing")
      print_form(DataModelState$formula)
      res <- try({
        l <- get_levene()$new(DataModelState$df,DataModelState$formula, input$center)
        l$validate()
        l$eval(ResultsState)
      }, silent = TRUE)
      if (inherits(res, "try-error")) {
        err <- conditionMessage(attr(res, "condition"))
        print_err(err)
      }
    }
    shiny::observeEvent(input$levene, {
      runLevene()
    })

    runDiagnosticPlot <- function() {
      df <- DataModelState$df
      print_req(is.data.frame(df), "The dataset is missing")
      print_form(DataModelState$formula)
      p <- try({
        dp <- get_diagnostic_plot()$new(DataModelState$df, DataModelState$formula)
        dp$validate()
        dp$eval(ResultsState)
      }, silent = TRUE)
      if (inherits(p, "try-error")) {
        err <- conditionMessage(attr(p, "condition"))
        print_err(err)
      }
    }
    shiny::observeEvent(input$DiagnosticPlot, {
      runDiagnosticPlot()
    })
  })

}
