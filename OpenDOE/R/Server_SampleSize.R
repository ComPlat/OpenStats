predictor_combinations <- function(predictors) {
  grid <- expand.grid(predictors, stringsAsFactors = FALSE)
  apply(grid, 1, paste, collapse = " | ")
}

sampleSizeServer <- function(id, State) {
  shiny::moduleServer(id, function(input, output, session) {

    # Test-selection wizard
    # ======================================================================
    current_node <- shiny::reactiveVal(NULL)

    wizard_group_count <- shiny::reactive({
      predictor_names <- names(State$predictors)
      if (length(predictor_names) == 0L) return(NA_integer_)
      nrow(expand.grid(State$predictors[predictor_names]))
    })
    wizard_start_node <- shiny::reactive({
      n <- wizard_group_count()
      if (is.na(n)) return(NULL)
      test_selection_tree_start_node(test_selection_tree, n)
    })

    current_leaf <- shiny::reactive({
      node <- current_node()
      if (inherits(node, "Test")) node else NULL
    })

    lapply(seq_len(test_selection_tree_max_answer_slots), function(i) {
      shiny::observeEvent(input[[test_selection_tree_answer_input_id(i)]], {
        node <- current_node()
        if (!is.null(node) && i <= length(node$answers)) {
          current_node(node$answers[[i]]$next_node)
        }
      }, ignoreInit = TRUE)
    })

    shiny::observe({
      start <- wizard_start_node()
      if (is.null(start)) return()
      node <- current_node()
      if (is.null(node) || !identical(test_selection_tree_branch_of(node, test_selection_tree), start)) {
        current_node(start)
      }
    })

    shiny::observeEvent(input$wizard_back, {
      node <- current_node()
      start <- wizard_start_node()
      if (!is.null(node) && !is.null(start) && !identical(node, start)) {
        current_node(node$parent_answer$parent_node)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$wizard_reset, {
      start <- wizard_start_node()
      if (!is.null(start)) current_node(start)
    }, ignoreInit = TRUE)

    shiny::observeEvent(current_leaf(), {
      shiny::updateTabsetPanel(session, "conditionedPanels", selected = current_leaf()$tab)
    }, ignoreNULL = TRUE)

    output$wizard_ui <- shiny::renderUI({
      if (length(State$predictors) == 0L) return(NULL)
      node <- current_node()
      if (is.null(node)) return(NULL)

      crumbs <- test_selection_tree_breadcrumb(node, test_selection_tree)
      breadcrumb_ui <- if (length(crumbs) > 0L) {
        htmltools::div(
          class = "doe-wizard-breadcrumb",
          lapply(crumbs, function(c) htmltools::span(class = "doe-wizard-crumb", c))
        )
      }
      step_ui <- if (inherits(node, "Test")) {
        alt_note <- if (isTRUE(node$closed_form_alt) && length(State$predictors) == 1L) {
          htmltools::p(
            class = "doe-wizard-alt-note",
            "Two valid options here: the Power analysis tab (closed-form, fast, exact) or the tab above (Monte Carlo simulation, more flexible)."
          )
        }
        htmltools::div(
          class = "doe-wizard-result",
          htmltools::h4(node$name),
          htmltools::p(node$description),
          alt_note
        )
      } else {
        htmltools::div(
          class = "doe-wizard-step",
          htmltools::h4(node$question),
          htmltools::div(
            class = "doe-wizard-answers",
            lapply(seq_along(node$answers), function(i) {
              shiny::actionButton(test_selection_tree_answer_button_id(i), label = node$answers[[i]]$label)
            })
          )
        )
      }
      controls_ui <- if (!identical(node, wizard_start_node())) {
        htmltools::div(
          class = "doe-wizard-controls",
          shiny::actionButton("SAMPLESIZE-wizard_back", "← Back"),
          shiny::actionLink("SAMPLESIZE-wizard_reset", "↺ start over", style = "margin-left: 16px;")
        )
      }
      htmltools::div(class = "doe-wizard", breadcrumb_ui, step_ui, controls_ui)
    })

    # Power analysis
    # ======================================================================
    power_analysis_sidebar <- function() {
      predictor_names <- names(State$predictors)
      if (length(predictor_names) > 1L) return()
      primary <- predictor_names[[1L]]
      n_levels <- length(State$predictors[[primary]])
      if (n_levels == 2L) {
        shiny::tags$div(
          class = "doe-box",
          shiny::h4("Power analysis: t-test"),
          shiny::numericInput("SAMPLESIZE-cohens_d", "Effect size (Cohen's d)", value = 0.8, step = 0.1),
          shiny::numericInput("SAMPLESIZE-sig_level_ttest", "Significance level", value = 0.05, step = 0.01),
          shiny::numericInput("SAMPLESIZE-desired_power_ttest", "Desired power", value = 0.8, step = 0.05),
          shiny::actionButton("SAMPLESIZE-calc_ttest", "Calculate")
        )
      } else {
        shiny::tags$div(
          class = "doe-box",
          shiny::h4("Power analysis: ANOVA"),
          shiny::numericInput("SAMPLESIZE-cohens_f", "Effect size (Cohen's f)", value = 0.8, step = 0.05),
          shiny::numericInput("SAMPLESIZE-sig_level_anova", "Significance level", value = 0.05, step = 0.01),
          shiny::numericInput("SAMPLESIZE-desired_power_anova", "Desired power", value = 0.8, step = 0.05),
          shiny::actionButton("SAMPLESIZE-calc_anova", "Calculate")
        )
      }
    }

    # Pairwise comparisons (Monte Carlo, test picked by the wizard leaf --
    # t-Test (Student), Welch t-Test or Wilcoxon rank-sum)
    # ======================================================================
    pairwise_sidebar <- function(leaf) {
      if (is.null(leaf)) return(NULL)
      predictor_names <- names(State$predictors)
      selected <- input$selected_predictors
      selected <- selected[selected %in% predictor_names]
      if (length(selected) == 0L) selected <- predictor_names
      combo_labels <- predictor_combinations(State$predictors[selected])
      default_n <- length(combo_labels)
      n_groups <- input$n_groups
      if (is.null(n_groups) || is.na(n_groups) || n_groups < 1L) n_groups <- default_n
      group_inputs <- lapply(seq_len(n_groups), function(i) {
        default_label <- if (i <= length(combo_labels)) combo_labels[[i]] else paste("Group", i)
        shiny::fluidRow(
          shiny::column(4, shiny::textInput(paste0("SAMPLESIZE-group_name_", i), "Name", value = default_label)),
          shiny::column(4, shiny::numericInput(paste0("SAMPLESIZE-group_mean_", i), "Mean", value = 0)),
          shiny::column(4, shiny::numericInput(paste0("SAMPLESIZE-group_sd_", i), "SD", value = 1, min = 0))
        )
      })
      shiny::tagList(
        shiny::selectInput(
          "SAMPLESIZE-selected_predictors", "Predictors",
          choices = predictor_names, selected = selected, multiple = TRUE
        ),
        shiny::numericInput("SAMPLESIZE-n_groups", "Number of groups", value = n_groups, min = 1L, step = 1L),
        shiny::tags$div(
          class = "doe-box",
          shiny::h4(paste0("Pairwise comparisons (", leaf$name, ")")),
          group_inputs,
          shiny::selectInput(
            "SAMPLESIZE-mcc", "Multiplicity correction",
            choices = c("holm", "hochberg", "bonferroni", "none"), selected = "holm"
          ),
          shiny::selectInput(
            "SAMPLESIZE-family", "Power definition",
            choices = c("any", "all"), selected = "any"
          ),
          shiny::numericInput("SAMPLESIZE-alpha", "Significance level", value = 0.05, step = 0.01),
          shiny::numericInput("SAMPLESIZE-power_target_mc", "Desired power", value = 0.8, step = 0.05),
          shiny::numericInput("SAMPLESIZE-nsim", "Simulations per step", value = 2000, step = 100),
          shiny::numericInput("SAMPLESIZE-n_min", "Minimum n per group", value = 2, step = 1),
          shiny::numericInput("SAMPLESIZE-n_max", "Maximum n per group", value = 500, step = 10),
          shiny::numericInput("SAMPLESIZE-seed", "Seed", value = 42, step = 1),
          shiny::actionButton("SAMPLESIZE-calc_mc", "Calculate")
        )
      )
    }

    # Monte Carlo: ANOVA (Student only -- multi-predictor, no Welch
    # equivalent exists, see Randomization::determine_sample_size)
    # ======================================================================
    mc_anova_sidebar <- function() {
      predictor_names <- names(State$predictors)
      cell_grid <- expand.grid(State$predictors[predictor_names], stringsAsFactors = FALSE)
      if (is.null(State$mc_anova_means) || length(State$mc_anova_means) != nrow(cell_grid)) {
        State$mc_anova_means <- rep(1, nrow(cell_grid))
      }
      sd_model <- input$sd_model
      if (is.null(sd_model)) sd_model <- "cv"
      sd_input <- if (sd_model == "sd") {
        shiny::numericInput("SAMPLESIZE-sd_value", "Standard deviation (same for every cell)", value = 1, step = 0.1, min = 0)
      } else {
        shiny::numericInput("SAMPLESIZE-cv", "Coefficient of variation", value = 0.25, step = 0.05, min = 0)
      }

      target_effect <- input$target_effect_mc_anova
      if (is.null(target_effect) || !(target_effect %in% predictor_names)) target_effect <- predictor_names[[1]]

      shiny::tags$div(
        class = "doe-box",
        shiny::h4("Monte Carlo: ANOVA (Student)"),
        shiny::selectInput(
          "SAMPLESIZE-target_effect_mc_anova", "Target effect (drives sample size search)",
          choices = predictor_names, selected = target_effect
        ),
        shiny::tags$p("Power for every effect is reported; n is chosen to reach the target power for the effect above only."),
        shiny::tags$strong("Cell means"),
        shiny::tags$p("One mean per predictor combination -- double-click the \"mean\" cell to edit."),
        DT::DTOutput("SAMPLESIZE-mean_table"),
        shiny::radioButtons(
          "SAMPLESIZE-sd_model", "Variance model",
          choices = c("Proportional (constant CV)" = "cv", "Constant SD" = "sd"),
          selected = sd_model
        ),
        sd_input,

        shiny::tags$hr(),
        shiny::numericInput("SAMPLESIZE-sig_level_mc_anova", "Significance level", value = 0.05, step = 0.01),
        shiny::numericInput("SAMPLESIZE-power_target_mc_anova", "Desired power", value = 0.8, step = 0.05),
        shiny::numericInput("SAMPLESIZE-nsim_anova", "Simulations per step", value = 2000, step = 100),
        shiny::numericInput("SAMPLESIZE-n_min_anova", "Minimum n per cell", value = 3, step = 1),
        shiny::numericInput("SAMPLESIZE-n_max_anova", "Maximum n per cell", value = 50, step = 1),
        shiny::numericInput("SAMPLESIZE-seed_anova", "Seed", value = 42, step = 1),
        shiny::actionButton("SAMPLESIZE-calc_mc_anova", "Calculate")
      )
    }

    output$mean_table <- DT::renderDT({
      predictor_names <- names(State$predictors)
      cell_grid <- expand.grid(State$predictors[predictor_names], stringsAsFactors = FALSE)
      if (is.null(State$mc_anova_means) || length(State$mc_anova_means) != nrow(cell_grid)) {
        State$mc_anova_means <- rep(1, nrow(cell_grid))
      }
      cell_grid$mean <- State$mc_anova_means
      DT::datatable(
        cell_grid, rownames = FALSE, options = list(dom = "t", paging = FALSE),
        editable = list(target = "cell", disable = list(columns = seq_len(ncol(cell_grid) - 1L) - 1L))
      )
    }, server = TRUE)

    shiny::observeEvent(input$mean_table_cell_edit, {
      predictor_names <- names(State$predictors)
      cell_grid <- expand.grid(State$predictors[predictor_names], stringsAsFactors = FALSE)
      cell_grid$mean <- State$mc_anova_means
      cell_grid <- DT::editData(cell_grid, input$mean_table_cell_edit, rownames = FALSE)
      State$mc_anova_means <- cell_grid$mean
    })

    # Monte Carlo: one-way test (single predictor only, see
    # Randomization::determine_sample_size_one_way) -- Kruskal-Wallis or
    # Welch-ANOVA, whichever the wizard leaf points at. Same sidebar either
    # way, only the title and the injected test differ.
    # ======================================================================
    mc_one_way_sidebar <- function(leaf) {
      if (is.null(leaf)) return(NULL)
      primary <- names(State$predictors)[[1]]
      levels_vec <- State$predictors[[primary]]
      if (is.null(State$mc_one_way_means) || length(State$mc_one_way_means) != length(levels_vec)) {
        State$mc_one_way_means <- rep(1, length(levels_vec))
      }
      sd_model <- input$sd_model_one_way
      if (is.null(sd_model)) sd_model <- "cv"
      sd_input <- if (sd_model == "sd") {
        shiny::numericInput("SAMPLESIZE-sd_value_one_way", "Standard deviation (same for every level)", value = 1, step = 0.1, min = 0)
      } else {
        shiny::numericInput("SAMPLESIZE-cv_one_way", "Coefficient of variation", value = 0.25, step = 0.05, min = 0)
      }

      shiny::tags$div(
        class = "doe-box",
        shiny::h4(paste0("Monte Carlo: ", leaf$name)),
        shiny::tags$strong("Level means"),
        shiny::tags$p("One mean per level -- double-click the \"mean\" cell to edit."),
        DT::DTOutput("SAMPLESIZE-mean_table_one_way"),
        shiny::radioButtons(
          "SAMPLESIZE-sd_model_one_way", "Variance model",
          choices = c("Proportional (constant CV)" = "cv", "Constant SD" = "sd"),
          selected = sd_model
        ),
        sd_input,

        shiny::tags$hr(),
        shiny::numericInput("SAMPLESIZE-sig_level_one_way", "Significance level", value = 0.05, step = 0.01),
        shiny::numericInput("SAMPLESIZE-power_target_one_way", "Desired power", value = 0.8, step = 0.05),
        shiny::numericInput("SAMPLESIZE-nsim_one_way", "Simulations per step", value = 2000, step = 100),
        shiny::numericInput("SAMPLESIZE-n_min_one_way", "Minimum n per level", value = 3, step = 1),
        shiny::numericInput("SAMPLESIZE-n_max_one_way", "Maximum n per level", value = 50, step = 1),
        shiny::numericInput("SAMPLESIZE-seed_one_way", "Seed", value = 42, step = 1),
        shiny::actionButton("SAMPLESIZE-calc_one_way", "Calculate")
      )
    }

    output$mean_table_one_way <- DT::renderDT({
      primary <- names(State$predictors)[[1]]
      levels_vec <- State$predictors[[primary]]
      if (is.null(State$mc_one_way_means) || length(State$mc_one_way_means) != length(levels_vec)) {
        State$mc_one_way_means <- rep(1, length(levels_vec))
      }
      level_grid <- data.frame(level = levels_vec, mean = State$mc_one_way_means)
      DT::datatable(
        level_grid, rownames = FALSE, options = list(dom = "t", paging = FALSE),
        editable = list(target = "cell", disable = list(columns = 0))
      )
    }, server = TRUE)

    shiny::observeEvent(input$mean_table_one_way_cell_edit, {
      primary <- names(State$predictors)[[1]]
      levels_vec <- State$predictors[[primary]]
      level_grid <- data.frame(level = levels_vec, mean = State$mc_one_way_means)
      level_grid <- DT::editData(level_grid, input$mean_table_one_way_cell_edit, rownames = FALSE)
      State$mc_one_way_means <- level_grid$mean
    })

    # Subtabs (main panel) + sidebar, both driven by the same
    # predictor/level state plus the wizard's current leaf
    # ======================================================================
    sample_size_visible_tabs <- function(State, leaf) {
      n_predictors <- length(State$predictors)
      if (n_predictors == 0L) return(character(0))
      if (is.null(leaf)) return(character(0))
      tabs <- leaf$tab
      if (n_predictors == 1L && isTRUE(leaf$closed_form_alt)) {
        tabs <- c(tabs, "Power analysis")
      }
      tabs
    }
    output$tabs_ui <- shiny::renderUI({
      leaf <- current_leaf()
      tabs <- sample_size_visible_tabs(State, leaf)
      if (length(tabs) == 0L) {
        return(shiny::tags$p("Define at least one predictor first."))
      }
      panels <- lapply(tabs, function(t) {
        shiny::tabPanel(t, shiny::tags$p("Configure inputs in the sidebar."))
      })
      do.call(shiny::tabsetPanel, c(panels, list(id = "SAMPLESIZE-conditionedPanels")))
    })

    output$sidebar_ui <- shiny::renderUI({
      if (length(State$predictors) == 0L) {
        return(shiny::tags$p("Define at least one predictor first."))
      }
      predictor_names <- names(State$predictors)
      leaf <- current_leaf()
      # input$conditionedPanels is sticky: when the wizard resets (leaf
      # becomes NULL) tabs_ui stops rendering a tabsetPanel altogether, so
      # no new value is ever sent for this input and it keeps pointing at
      # whichever subtab was selected before the reset. Only trust it while
      # it names a tab that's actually visible right now.
      visible_tabs <- sample_size_visible_tabs(State, leaf)
      subtab <- input$conditionedPanels
      if (!is.null(subtab) && !(subtab %in% visible_tabs)) subtab <- NULL
      body <- if (is.null(subtab)) {
        NULL
      } else if (subtab == "Power analysis") {
        power_analysis_sidebar()
      } else if (subtab == "Monte Carlo: anova") {
        mc_anova_sidebar()
      } else if (subtab == "Monte Carlo: one-way test") {
        mc_one_way_sidebar(leaf)
      } else if (subtab == "Pairwise comparisons") {
        pairwise_sidebar(leaf)
      }

      shiny::tagList(body)
    })

    # React to run tests
    # ======================================================================
    shiny::observeEvent(input$calc_ttest, {
      shiny::req(length(names(State$predictors)) == 1L)
      params <- list(
        predictors = State$predictors,
        primary_factor = names(State$predictors)[[1L]],
        cohens_d = input$cohens_d,
        sig_level = input$sig_level_ttest,
        desired_power = input$desired_power_ttest
      )
      res <- try(do.call(run_two_sample_ttest, params), silent = TRUE)
      if (inherits(res, "try-error")) {
        print_err(conditionMessage(attr(res, "condition")))
        return(invisible())
      }
      State$n_per_level <- res
      add_result(State, "ttest", "Power analysis: t-test", params, methods::new("sampleSizeResult", n = res))
    })

    # Power analysis
    # ======================================================================
    shiny::observeEvent(input$calc_anova, {
      shiny::req(length(names(State$predictors)) == 1L)
      params <- list(
        predictors = State$predictors,
        primary_factor = names(State$predictors)[[1L]],
        cohens_f = input$cohens_f,
        sig_level = input$sig_level_anova,
        desired_power = input$desired_power_anova
      )
      res <- try(do.call(run_one_way_anova, params), silent = TRUE)
      if (inherits(res, "try-error")) {
        print_err(conditionMessage(attr(res, "condition")))
        return(invisible())
      }
      State$n_per_level <- res
      add_result(State, "anova", "Power analysis: ANOVA", params, methods::new("sampleSizeResult", n = res))
    })

    # Pairwise comparisons -- which underlying test runs is entirely down to
    # the wizard's current leaf (leaf$run_type picks the run_* wrapper,
    # which has the matching Randomization test_fn baked in).
    # ======================================================================
    shiny::observeEvent(input$calc_mc, {
      leaf <- current_leaf()
      if (is.null(leaf)) {
        print_err("Answer the test-selection questions above first.")
        return(invisible())
      }

      n_groups <- input$n_groups
      if (is.null(n_groups) || is.na(n_groups) || n_groups < 2L) {
        print_err("Provide at least two groups")
        return(invisible())
      }

      group_names <- vapply(seq_len(n_groups), function(i) input[[paste0("group_name_", i)]], character(1))
      means <- stats::setNames(
        vapply(seq_len(n_groups), function(i) input[[paste0("group_mean_", i)]], numeric(1)),
        group_names
      )
      sds <- stats::setNames(
        vapply(seq_len(n_groups), function(i) input[[paste0("group_sd_", i)]], numeric(1)),
        group_names
      )

      params <- list(
        means = means, sds = sds,
        power_target = input$power_target_mc, alpha = input$alpha,
        mcc = input$mcc, family = input$family,
        nsim = input$nsim, n_min = input$n_min, n_max = input$n_max,
        seed = input$seed
      )

      fun <- switch(leaf$run_type,
        mc = run_estimate_sample_size,
        mc_wilcoxon = run_estimate_sample_size_wilcoxon,
        mc_student_t = run_estimate_sample_size_student_t
      )
      label <- paste0("Pairwise comparisons (", leaf$name, ")")

      State$mc_running <- TRUE
      State$bgp$start(
        fun = fun,
        args = params,
        on_success = function(res) {
          if (is.na(res$n)) {
            print_err(res$message)
            return(invisible())
          }
          State$n_per_level <- res$n
          add_result(State, leaf$run_type, label, params, methods::new("sampleSizeResult", n = res$n))
        },
        on_finally = function() State$mc_running <- FALSE
      )
    })

    # Monte carlo ANOVA (Student, unaffected by the wizard)
    # ======================================================================
    shiny::observeEvent(input$calc_mc_anova, {
      predictor_names <- names(State$predictors)

      alphas <- stats::setNames(rep(input$sig_level_mc_anova, length(predictor_names)), predictor_names)
      formula_obj <- stats::formula(paste("values ~", paste(predictor_names, collapse = " + ")))

      sd_model <- input$sd_model
      if (is.null(sd_model)) sd_model <- "cv"
      cv <- if (sd_model == "cv") input$cv else NULL
      sd <- if (sd_model == "sd") input$sd_value else NULL

      params <- list(
        levels = State$predictors,
        mu = State$mc_anova_means,
        cv = cv,
        sd = sd,
        formula = formula_obj,
        alphas = alphas,
        target_effect = input$target_effect_mc_anova,
        power_target = input$power_target_mc_anova,
        seed = input$seed_anova,
        nsim = input$nsim_anova,
        n_min = input$n_min_anova,
        n_max = input$n_max_anova
      )

      State$mc_running <- TRUE
      State$bgp$start(
        fun = run_determine_sample_size,
        args = params,
        on_success = function(res) {
          if (is.na(res$n)) {
            print_err(res$message)
            return(invisible())
          }
          State$n_per_level <- res$n
          add_result(State, "mc_anova", "Monte Carlo: ANOVA", params, methods::new("sampleSizeResult", n = res$n))
        },
        on_finally = function() State$mc_running <- FALSE
      )
    })

    # Monte Carlo: one-way test -- Kruskal-Wallis or Welch-ANOVA, whichever
    # the wizard leaf points at (leaf$run_type picks the run_* wrapper).
    # ======================================================================
    shiny::observeEvent(input$calc_one_way, {
      leaf <- current_leaf()
      if (is.null(leaf)) {
        print_err("Answer the test-selection questions above first.")
        return(invisible())
      }

      primary <- names(State$predictors)[[1]]

      sd_model <- input$sd_model_one_way
      if (is.null(sd_model)) sd_model <- "cv"
      cv <- if (sd_model == "cv") input$cv_one_way else NULL
      sd <- if (sd_model == "sd") input$sd_value_one_way else NULL

      params <- list(
        level = State$predictors[[primary]],
        mu = State$mc_one_way_means,
        cv = cv,
        sd = sd,
        alpha = input$sig_level_one_way,
        power_target = input$power_target_one_way,
        seed = input$seed_one_way,
        nsim = input$nsim_one_way,
        n_min = input$n_min_one_way,
        n_max = input$n_max_one_way
      )

      fun <- switch(leaf$run_type,
        mc_kruskal = run_determine_sample_size_kruskal,
        mc_welch_anova = run_determine_sample_size_welch_anova
      )
      label <- paste0("Monte Carlo: ", leaf$name)

      State$mc_running <- TRUE
      State$bgp$start(
        fun = fun,
        args = params,
        on_success = function(res) {
          if (is.na(res$n)) {
            print_err(res$message)
            return(invisible())
          }
          State$n_per_level <- res$n
          add_result(State, leaf$run_type, label, params, methods::new("sampleSizeResult", n = res$n))
        },
        on_finally = function() State$mc_running <- FALSE
      )
    })

    # show state and offer cancel
    # ======================================================================
    output$result_box <- shiny::renderUI({
      if (isTRUE(State$mc_running)) {
        shiny::tags$div(
          class = "doe-box",
          shiny::tags$p("Calculating..."),
          shiny::actionButton("SAMPLESIZE-cancel_mc", "Cancel", class = "btn-danger")
        )
      }
    })

    shiny::observeEvent(input$cancel_mc, {
      State$bgp$cancel()
    }, ignoreInit = TRUE)

  })
}
