testsSidebarUI <- function(id) {
  tabPanel(
    "Tests",
    br(),
    uiOutput(NS(id, "SidebarTestsUI")),
    uiOutput(NS(id, "padjUI"))
  )
}

testsUI <- function(id) {
  fluidRow(
    uiOutput(NS(id, "tabs"))
  )
}

testsServer <- function(id, DataModelState, ResultsState) {
  moduleServer(id, function(input, output, session) {
    # Render tabs
    output$tabs <- renderUI({
      tabs <- list()

      if (is.null(DataModelState$formula) || inherits(DataModelState$formula, "LinearFormula")) {
        tabs[[length(tabs) + 1]] <- tabPanel("Two groups", br())
      }

      tabs[[length(tabs) + 1]] <- tabPanel("More than two groups", br())
      tabs[[length(tabs) + 1]] <- tabPanel("Posthoc tests", br())

      do.call(tabsetPanel, c(tabs, id = NS(id, "TestsConditionedPanels")))
    })
    # Render Sidebar
    output$SidebarTestsUI <- renderUI({
      req(input$TestsConditionedPanels)
      message <- check_statistical_tests(DataModelState)
      if (!is.null(message)) {
        return(
          info_div(message)
        )
      }
      if (input$TestsConditionedPanels == "Two groups" && (is.null(DataModelState$formula) || inherits(DataModelState$formula, "LinearFormula"))) {
        div(
          sliderInput(NS(id, "confLevel"), "Confidence level of the interval",
            min = 0, max = 1, value = 0.95
          ),
          selectInput(
            NS(id, "altHyp"), "Alternative hypothesis",
            c(
              "Two sided" = "two.sided",
              "Less" = "less",
              "Greater" = "greater"
            )
          ),
          selectInput(
            NS(id, "varEq"), "Are the two variances treated as equal or not?",
            c(
              "Equal" = "eq",
              "Not equal" = "noeq"
            )
          ),
          actionButton(NS(id, "tTest"), "t test")
        )
      } else if (input$TestsConditionedPanels == "More than two groups") {
        div(
          actionButton(NS(id, "aovTest"), "ANOVA",
            title = "Use ANOVA (Analysis of Variance) when comparing the means of more than two groups, assuming the data is normally distributed and variances are equal across groups. For more information see the Assumption tab"
          ),
          actionButton(NS(id, "kruskalTest"), "Kruskal-Wallis Test",
            title = "Use the Kruskal-Wallis test when comparing more than two groups but the assumptions of normality or equal variances are not met. It is a non-parametric test. For more information see the Assumption tab"
          )
        )
      } else if (input$TestsConditionedPanels == "Posthoc tests" && (is.null(DataModelState$formula) || inherits(DataModelState$formula, "LinearFormula"))) {
        div(
          selectInput(NS(id, "PostHocTests"), "Choose a Post Hoc test",
            choices = c(
              "Tukey HSD" = "HSD", "Kruskal Wallis post hoc test" = "kruskalTest",
              "Least significant difference test" = "LSD",
              "Scheffe post hoc test" = "scheffe", "REGW post hoc test" = "REGW"
            )
          ),
          actionButton(NS(id, "PostHocTest"), "run test"),
          sliderInput(NS(id, "pval"), "P-value",
            min = 0, max = 0.15, value = 0.05
          ),
          selectInput(
            NS(id, "design"), "Design",
            c(
              "Balanced" = "ba",
              "Unbalanced" = "ub"
            )
          )
        )
      } else if (input$TestsConditionedPanels == "Posthoc tests" && inherits(DataModelState$formula, "GeneralisedLinearFormula")) {
        div(
          selectInput(NS(id, "PostHocEmmeans"), "Choose an adjustment method",
            choices = c(
              "tukey" = "tukey",
              "sidak" = "sidak",
              "bonferroni" = "bonferroni",
              "scheffe" = "scheffe",
              "none" = "none",
              "fdr" = "fdr",
              "holm" = "holm",
              "hochberg" = "hochberg",
              "hommel" = "hommel"
            )
          ),
          actionButton(NS(id, "PostHocEmmeansTest"), "run PostHoc test")
        )
      }
    })

    # Render p adjustment methods
    output[["padjUI"]] <- renderUI({
      req(input$TestsConditionedPanels == "Posthoc tests")
      req(input$PostHocTests)
      req(inherits(DataModelState$formula, "LinearFormula"))
      if (input$PostHocTests == "kruskalTest" || input$PostHocTests == "LSD") {
        return(
          selectInput(NS(id, "padj"), "Adjusted p method",
            c(
              "Holm" = "holm",
              "Hommel" = "hommel",
              "Hochberg" = "hochberg",
              "Bonferroni" = "bonferroni",
              "BH" = "BH",
              "BY" = "BY",
              "fdr" = "fdr"
            ),
            selectize = FALSE
          )
        )
      }
    })

    tTest <- function() {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      print_form(DataModelState$formula)

      res <- try({
        tt <- t_test_V1_2$new(DataModelState$df,DataModelState$formula, input$varEq, input$confLevel, input$altHyp)
        tt$validate()
        tt$eval(ResultsState)
      })
      if (inherits(res, "try-error")) {
        err <- conditionMessage(attr(res, "condition"))
        print_err(err)
      }
    }
    observeEvent(input$tTest, {
      tTest()
    })

    conductTests <- function(method) {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      print_form(DataModelState$formula)

      res <- try({
        st <- statistical_tests_V1_2$new(
          DataModelState$df,DataModelState$formula, input$design, input$pval, input$padj
        )
        st$validate()
        st$eval(ResultsState, method)
      }, silent = TRUE)
      if (inherits(res, "try-error")) {
        err <- conditionMessage(attr(res, "condition"))
        err <- paste0(err, "\n", "Test did not run successfully")
        print_err(err)
      } else if (is.null(res)) {
        err <- "Test did not run successfully"
        print_err(err)
      }
    }

    observeEvent(input$aovTest, {
      conductTests("aov")
    })

    observeEvent(input$kruskalTest, {
      conductTests("kruskal")
    })

    observeEvent(input$PostHocTest, {
      conductTests(input$PostHocTests)
    })

    observeEvent(input$PostHocEmmeansTest, {
      conductTests(input$PostHocEmmeans)
    })
  })

}
