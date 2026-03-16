# -----------------------------------------------------------------------------------
# Server which renders tbas and parametric vs. non parametric
# -----------------------------------------------------------------------------------
testsUIServer <- function(id, DataModelState, ResultsState) {
  shiny::moduleServer(id, function(input, output, session) {
    # Render parametric vs non parametric
    output$parametricUI <- shiny::renderUI({
      if (inherits(DataModelState$formula, "LinearFormula")) {
        htmltools::div(
          shiny::radioButtons(
            shiny::NS(id, "ParametricOrNonParametric"),
            "Test family",
            choices = c(
              "Non-parametric" = "non_parametric",
              "Parametric" = "parametric"
            ),
            inline = TRUE
          ),
          class = "var-box-output"
        )
      }
    })
    # Render tabs
    output$tabs <- shiny::renderUI({
      n_groups <- calc_n_covariates(DataModelState)
      tabs <- list()

      if (n_groups == 2L || n_groups == 0 || inherits(DataModelState$formula, "OptimFormula")) {
        if (is.null(DataModelState$formula) || inherits(DataModelState$formula, "LinearFormula")) {
          tabs[[length(tabs) + 1]] <- shiny::tabPanel("Two groups", htmltools::br())
        }
        res <- do.call(shiny::tabsetPanel, c(tabs, id = shiny::NS(id, "TestsConditionedPanels")))
        if (n_groups == 2L && !inherits(DataModelState$formula, "OptimFormula")) {
          return(res)
        }
      }

      tabs[[length(tabs) + 1]] <- shiny::tabPanel("More than two groups", htmltools::br())
      tabs[[length(tabs) + 1]] <- shiny::tabPanel("Multiple Comparisons", htmltools::br())
      do.call(shiny::tabsetPanel, c(tabs, id = shiny::NS(id, "TestsConditionedPanels")))
    })

  })
}

# -----------------------------------------------------------------------------------
# Server which renders the sidebar based on model, parametric or non parametric
# -----------------------------------------------------------------------------------
testsUISidebarServer <- function(id, DataModelState, ResultsState) {
  shiny::moduleServer(id, function(input, output, session) {

    # Render Sidebar
    parametric_two_groups <- function() {
      htmltools::div(
        shiny::sliderInput(shiny::NS(id, "confLevel"), "Confidence level of the interval",
          min = 0, max = 1, value = 0.95
        ),
        shiny::selectInput(shiny::NS(id, "altHyp"), "Alternative hypothesis", alt_hyp_2_groups()),
        shiny::selectInput(
          shiny::NS(id, "varEq"), "Are the two variances treated as equal or not?",
          c(
            "Equal" = "eq",
            "Not equal" = "noeq"
          )
        ),
        shiny::actionButton(
          shiny::NS(id, "tTest"),
          "t test",
          title = "Independent two-sample t-test. Compares the means of two independent groups. Paired t-tests are currently not supported."
        ),
        class = "var-box-output"
      )
    }
    parametric_more_than_two_groups <- function() {
      ui_elements <- list()
      ui_elements[[length(ui_elements) + 1L]] <- htmltools::div(
        shiny::actionButton(
          shiny::NS(id, "aovTest"),
          "ANOVA",
          title = "Use ANOVA (Analysis of Variance) when comparing the means of more than two independent groups. ANOVA assumes independent observations, approximately normally distributed residuals, and similar variances across groups. For more information see the Assumption tab."
        )
      )
      if (is_one_way_aov(DataModelState)) {
        ui_elements[[length(ui_elements) + 1L]] <- htmltools::br()
        ui_elements[[length(ui_elements) + 1L]] <- htmltools::div(
          shiny::actionButton(
            shiny::NS(id, "welchaovTest"),
            "Welch ANOVA",
            title = "Use Welch ANOVA (Analysis of Variance) when comparing the means of more than two independent groups. The Welch ANOVA assumes independent observations, approximately normally distributed residuals. But the variances can differ across the groups. For more information see the Assumption tab."
          )
        )
      }
      htmltools::div(
        do.call(htmltools::tagList, ui_elements),
        class = "var-box-output"
      )
    }

    pairwise_comparisons_ui <- function() {
      if (input$ParametricOrNonParametric == "parametric") {
        title_button <- "Performs pairwise comparisons between group means using pairwise t-tests with p-value adjustment. Paired tests are currently not supported."
        title <- htmltools::div(
          htmltools::h3("Pairwise Comparisons using:"),
          htmltools::h5("T-Tests")
        )
      } else {
        title <- htmltools::div(
          htmltools::h3("Pairwise Comparisons using:"),
          htmltools::h5("Wilcoxon-rank-sum-tests")
        )
        title_button <- "Performs pairwise comparisons between group means using pairwise Wilcoxon rank-sum tests with p-value adjustment. Paired tests are currently not supported."
      }
      htmltools::div(
        title,
        shiny::selectInput(shiny::NS(id, "padjPairwise"), "Adjusted p method", p_value_correction_methods(), selectize = FALSE),
        shiny::selectInput(shiny::NS(id, "altHypPairwise"), "Alternative hypothesis", alt_hyp_2_groups()),
        shiny::sliderInput(shiny::NS(id, "pvalPairwise"), "P-value",
          min = 0, max = 0.15, value = 0.05
        ),
        shiny::actionButton(
          shiny::NS(id, "pairwise_test"),
          "Run pairwise comparisons",
          title = title_button
        ),
        class = "var-box-output"
      )
    }
    parametric_multiple_comparisons <- function() {
      ui_elements <- list()
      if (is_one_way_aov(DataModelState)) {
        ui_elements[[length(ui_elements) + 1L]] <- htmltools::div(
          htmltools::h3("PostHoc Tests"),
          shiny::selectInput(shiny::NS(id, "PostHocTests"), "Choose a Post Hoc test",
            choices = c(
              "Tukey HSD" = "HSD",
              "Least significant difference test" = "LSD",
              "Scheffe post hoc test" = "scheffe", "REGW post hoc test" = "REGW"
            )
          ),
          shiny::actionButton(shiny::NS(id, "PostHocTest"), "run test"),
          shiny::sliderInput(shiny::NS(id, "pval"), "P-value",
            min = 0, max = 0.15, value = 0.05
          ),
          shiny::selectInput(
            shiny::NS(id, "design"), "Design",
            c(
              "Balanced" = "ba",
              "Unbalanced" = "ub"
            )
          ),
          shiny::uiOutput(shiny::NS(id, "padjUI")),
          class = "var-box-output"
        )
        ui_elements[[length(ui_elements) + 1L]] <- htmltools::br()
      }
      ui_elements[[length(ui_elements) + 1L]] <- pairwise_comparisons_ui()
      htmltools::div(
        do.call(htmltools::tagList, ui_elements)
      )
    }
    parametric_sidebar <- function() {
      # Parametric linear model
      if (input$TestsConditionedPanels == "Two groups") {
        parametric_two_groups()
      } else if (input$TestsConditionedPanels == "More than two groups") {
        parametric_more_than_two_groups()
      } else if (input$TestsConditionedPanels == "Multiple Comparisons") {
        parametric_multiple_comparisons()
      }
    }

    glm_sidebar <- function() {
      if (input$TestsConditionedPanels == "More than two groups") {
        htmltools::div(
          shiny::actionButton(
            shiny::NS(id, "aovTest"),
            "Analysis of Deviance (GLM)",
            title = "Performs an analysis of deviance for the generalized linear model. Tests the contribution of model terms using likelihood-ratio or deviance tests."
          )
        )
      }
      else if (input$TestsConditionedPanels == "Multiple Comparisons") {
        htmltools::div(
          htmltools::h3("Pairwise Comparisons"),
          shiny::selectInput(shiny::NS(id, "PostHocEmmeans"), "Choose an adjustment method",
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
          shiny::actionButton(shiny::NS(id, "PostHocEmmeansTest"), "run PostHoc test")
        )
      }
    }

    non_parametric_two_groups <- function() {
      htmltools::div(
        shiny::sliderInput(shiny::NS(id, "confLevel"), "Confidence level of the interval",
          min = 0, max = 1, value = 0.95
        ),
        shiny::selectInput(shiny::NS(id, "altHyp"), "Alternative hypothesis", alt_hyp_2_groups()),
        shiny::actionButton(
          shiny::NS(id, "wilcoxTest"),
          "Wilcoxon test",
          title = "Wilcoxon rank-sum test (Mann–Whitney test). A non-parametric alternative to the independent two-sample t-test for comparing two independent groups. Paired tests are currently not supported."
        ),
        class = "var-box-output"
      )
    }

    non_parametric_more_than_two_groups <- function() {
      ui_elements <- list()
      if (is_one_way_aov(DataModelState)) {
        ui_elements[[length(ui_elements) + 1L]] <- htmltools::div(
          shiny::actionButton(shiny::NS(id, "kruskalTest"), "Kruskal-Wallis Test",
            title = "Use the Kruskal-Wallis test when comparing more than two groups but the assumptions of normality or equal variances are not met. It is a non-parametric test. For more information see the Assumption tab"
          ),
          class = "var-box-output"
        )
      }
      if (problem_small_enough_for_permutation_ANOVA(DataModelState)) {
        ui_elements[[length(ui_elements) + 1L]] <- htmltools::div(
          shiny::selectInput(
            shiny::NS(id, "perm"), "Permutation p-value method",
            choices = c(
              "Exact (all permutations)" = "Exact",
              "Monte Carlo approximation" = "Prob",
              "Sequential permutation (SPR)" = "SPR"
            ),
            selectize = FALSE
          ),
          htmltools::div(
            shiny::actionButton(
              shiny::NS(id, "PermANOVATest"),
              "Permutation ANOVA",
              title = "Uses permutation resampling to compute the p-value for the ANOVA test statistic. Useful when the normality assumption is doubtful. Observations must still be independent and exchangeable under the null hypothesis."
            )
          ),
          class = "var-box-output"
        )
      }
      if (length(ui_elements) >= 1L) {
        htmltools::div(
          do.call(htmltools::tagList, ui_elements),
        )
      } else {
        info_div(
          "No tests are available. As more than one predictor is used in the statistical model the Kruskal-Wallis cannot be used. Moreover, the problem is too complicated for the permutation ANOVA."
        )
      }
    }
    non_parametric_multiple_comparisons <- function() {
      ui_elements <- list()
      if (is_one_way_aov(DataModelState)) {
        ui_elements[[length(ui_elements) + 1L]] <- htmltools::div(
          htmltools::h3("PostHoc Tests"),
          shiny::selectInput(shiny::NS(id, "PostHocTests"), "Choose a Post Hoc test",
            choices = c(
              "Kruskal Wallis post hoc test" = "kruskalTest"
              # "Dunn post hoc test" = "dunnTest" # Postponed; available packages are: FSA, PMCMRplus, or dunn.test
              # FSA imports dunn.test. PMCMRplus has many many PostHocTests
            )
          ),
          shiny::actionButton(shiny::NS(id, "PostHocTest"), "run test"),
          shiny::sliderInput(shiny::NS(id, "pval"), "P-value",
            min = 0, max = 0.15, value = 0.05
          ),
          shiny::selectInput(
            shiny::NS(id, "design"), "Design",
            c(
              "Balanced" = "ba",
              "Unbalanced" = "ub"
            )
          ),
          shiny::uiOutput(shiny::NS(id, "padjUI")),
          class = "var-box-output"
        )
        ui_elements[[length(ui_elements) + 1L]] <- htmltools::br()
      }
      ui_elements[[length(ui_elements) + 1L]] <- pairwise_comparisons_ui()
      htmltools::div(
        do.call(htmltools::tagList, ui_elements)
      )
    }
    non_parametric_sidebar <- function() {
      if (!is.null(DataModelState$formula) && inherits(DataModelState$formula, "LinearFormula")) {
        if (input$TestsConditionedPanels == "Two groups") {
          non_parametric_two_groups()
        } else if (input$TestsConditionedPanels == "More than two groups") {
          non_parametric_more_than_two_groups()
        } else if (input$TestsConditionedPanels == "Multiple Comparisons" && (is.null(DataModelState$formula) || inherits(DataModelState$formula, "LinearFormula"))) {
          non_parametric_multiple_comparisons()
        }
      }
    }

    output$SidebarTestsUI <- shiny::renderUI({
      shiny::req(input$TestsConditionedPanels)
      message <- check_statistical_tests(DataModelState)
      if (!is.null(message)) {
        return(
          info_div(message)
        )
      }

      # ---------------------------------------------------------------------------------------------------------------------
      # Parametric tests for linear models and the tests for other models such as glm
      # ---------------------------------------------------------------------------------------------------------------------

      if (!is.null(input$ParametricOrNonParametric) && input$ParametricOrNonParametric == "parametric" &&
        (is.null(DataModelState$formula) || inherits(DataModelState$formula, "LinearFormula"))) {
        parametric_sidebar()
      } else if (inherits(DataModelState$formula, "GeneralisedLinearFormula")) {
        glm_sidebar()
      }
      else if (!is.null(input$ParametricOrNonParametric) && input$ParametricOrNonParametric == "non_parametric") {
        non_parametric_sidebar()
      }

    })

    # Render p adjustment methods
    output[["padjUI"]] <- shiny::renderUI({
      shiny::req(input$TestsConditionedPanels == "Multiple Comparisons")
      shiny::req(input$PostHocTests)
      shiny::req(inherits(DataModelState$formula, "LinearFormula"))
      if (input$PostHocTests == "kruskalTest" || input$PostHocTests == "LSD") {
        return(
          shiny::selectInput(shiny::NS(id, "padj"), "Adjusted p method", p_value_correction_methods(), selectize = FALSE)
        )
      }
    })
  })
}

# -----------------------------------------------------------------------------------
# Server which reacts on triggering run tests
# -----------------------------------------------------------------------------------
testsServer <- function(id, DataModelState, ResultsState) {
  shiny::moduleServer(id, function(input, output, session) {

    # --------------------------------------------------------------------------
    # 2 Groups
    # --------------------------------------------------------------------------
    # Parametric
    tTest <- function() {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      print_form(DataModelState$formula)

      res <- try({
        tt <- get_ttest()$new(DataModelState$df,DataModelState$formula, input$varEq, input$confLevel, input$altHyp)
        tt$validate()
        tt$eval(ResultsState)
      }, silent = TRUE)
      if (inherits(res, "try-error")) {
        err <- conditionMessage(attr(res, "condition"))
        print_err(err)
      }
    }
    shiny::observeEvent(input$tTest, {
      tTest()
    })
    # Non-Parametric
    wilcoxRankSumTest <- function() {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      print_form(DataModelState$formula)

      res <- try({
        w <- get_wilcox_rank_sum()$new(DataModelState$df,DataModelState$formula, input$altHyp)
        w$validate()
        w$eval(ResultsState)
      }, silent = TRUE)
      if (inherits(res, "try-error")) {
        err <- conditionMessage(attr(res, "condition"))
        print_err(err)
      }
    }
    shiny::observeEvent(input$wilcoxTest, {
      wilcoxRankSumTest()
    })

    # --------------------------------------------------------------------------
    # > 2 Groups
    # --------------------------------------------------------------------------
    conductTests <- function(method) {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      print_form(DataModelState$formula)

      res <- try({
        st <- get_statistical_tests()$new(
          DataModelState$df,DataModelState$formula, input$design, input$pval, input$padj
        )
        st$validate()
        st$eval(ResultsState, method)
      }, silent = TRUE)
      if (inherits(res, "try-error")) {
        err <- conditionMessage(attr(res, "condition"))
        err <- paste0(err, "\n", "Test did not run successfully")
        print_err(err)
      }
    }
    # Parametric
    shiny::observeEvent(input$aovTest, {
      conductTests("aov")
    })
    shiny::observeEvent(input$welchaovTest, {
      conductTests("welch_aov")
    })
    # Non-Parametric
    shiny::observeEvent(input$kruskalTest, {
      conductTests("kruskal")
    })
    # Permutation ANOVA
    conductPermutationANOVA <- function() {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      print_form(DataModelState$formula)

      res <- try({
        st <- get_permutation_anova()$new(
          DataModelState$df, DataModelState$formula, input$perm
        )
        st$validate()
        st$eval(ResultsState)
      }, silent = TRUE)
      if (inherits(res, "try-error")) {
        err <- conditionMessage(attr(res, "condition"))
        err <- paste0(err, "\n", "Test did not run successfully")
        print_err(err)
      }
    }
    shiny::observeEvent(input$PermANOVATest, {
      conductPermutationANOVA()
    })

    # --------------------------------------------------------------------------
    # Multiple Comparisons
    # --------------------------------------------------------------------------
    conductPairwiseComparison <- function() {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      print_form(DataModelState$formula)
      res <- try({
        st <- get_pairwise_tests()$new(
          DataModelState$df, DataModelState$formula, input$pvalPairwise,
          input$altHypPairwise, input$padjPairwise, input$ParametricOrNonParametric
        )
        st$validate()
        st$eval(ResultsState)
      }, silent = TRUE)
      if (inherits(res, "try-error")) {
        err <- conditionMessage(attr(res, "condition"))
        err <- paste0(err, "\n", "Test did not run successfully")
        print_err(err)
      }
    }
    shiny::observeEvent(input$pairwise_test, {
      conductPairwiseComparison()
    })
    shiny::observeEvent(input$PostHocTest, {
      conductTests(input$PostHocTests)
    })

    shiny::observeEvent(input$PostHocEmmeansTest, {
      conductTests(input$PostHocEmmeans)
    })
  })
}
