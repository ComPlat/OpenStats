predictor_combinations <- function(predictors) {
  grid <- expand.grid(predictors, stringsAsFactors = FALSE)
  apply(grid, 1, paste, collapse = " | ")
}

sampleSizeServer <- function(id, State) {
  shiny::moduleServer(id, function(input, output, session) {

    # Power analysis
    # ======================================================================
    power_analysis_sidebar <- function() {
      n_predictors <- length(State$predictors)
      if (n_predictors == 0L || n_predictors > 2L) {
        return(shiny::tags$p(
          "Power analysis is only available for one or two predictors."
        ))
      }
      predictor_names <- names(State$predictors)
      primary <- input$primary_factor
      if (is.null(primary) || !(primary %in% predictor_names)) primary <- predictor_names[[1]]
      n_levels <- length(State$predictors[[primary]])
      shiny::tagList(
        shiny::selectInput(
          "SAMPLESIZE-primary_factor", "Primary factor",
          choices = predictor_names, selected = primary
        ),
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
            shiny::numericInput("SAMPLESIZE-cohens_f", "Effect size (Cohen's f)", value = 0.25, step = 0.05),
            shiny::numericInput("SAMPLESIZE-sig_level_anova", "Significance level", value = 0.05, step = 0.01),
            shiny::numericInput("SAMPLESIZE-desired_power_anova", "Desired power", value = 0.8, step = 0.05),
            shiny::actionButton("SAMPLESIZE-calc_anova", "Calculate")
          )
        }
      )
    }

    # Monte Carlo: multiple comparisons
    # ======================================================================
    mc_multiple_comparison_sidebar <- function() {
      if (length(State$predictors) == 0L) {
        return(shiny::tags$p("Define at least one predictor first."))
      }
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
          shiny::h4("Monte Carlo: multiple comparison"),
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

    # Monte Carlo: ANOVA
    # ======================================================================
    mc_anova_sidebar <- function() {
      if (length(State$predictors) == 0L) {
        return(shiny::tags$p("Define at least one predictor first."))
      }
      predictor_names <- names(State$predictors)
      mean_inputs <- lapply(predictor_names, function(pred) {
        levels <- State$predictors[[pred]]
        level_inputs <- lapply(levels, function(lvl) {
          safe <- make.names(paste(pred, lvl, sep = "_"))
          shiny::column(3, shiny::numericInput(paste0("SAMPLESIZE-mean_", safe), paste(pred, "=", lvl), value = 1))
        })
        shiny::tagList(
          shiny::tags$strong(pred),
          shiny::fluidRow(level_inputs)
        )
      })
      sd_model <- input$sd_model
      if (is.null(sd_model)) sd_model <- "cv"
      sd_input <- if (sd_model == "sd") {
        shiny::numericInput("SAMPLESIZE-sd_value", "Standard deviation (same for every cell)", value = 1, step = 0.1, min = 0)
      } else {
        shiny::numericInput("SAMPLESIZE-cv", "Coefficient of variation", value = 0.25, step = 0.05, min = 0)
      }

      interaction_possible <- FALSE
      if (length(State$predictors) >= 2L) interaction_possible <- TRUE

      interaction_a <- input$interaction_a
      interaction_b <- input$interaction_b
      interaction_choices <- c("(none)", predictor_names)
      if (is.null(interaction_a)) interaction_a <- "(none)"
      if (is.null(interaction_b)) interaction_b <- "(none)"
      interaction_grid <- NULL
      if (interaction_a != "(none)" && interaction_b != "(none)" && interaction_a != interaction_b) {
        levels_a <- State$predictors[[interaction_a]]
        levels_b <- State$predictors[[interaction_b]]
        cells <- expand.grid(a = levels_a, b = levels_b, stringsAsFactors = FALSE)
        cell_inputs <- lapply(seq_len(nrow(cells)), function(i) {
          la <- cells$a[[i]]
          lb <- cells$b[[i]]
          safe <- make.names(paste(interaction_a, la, interaction_b, lb, sep = "_"))
          shiny::column(3, shiny::numericInput(
            paste0("SAMPLESIZE-interaction_", safe),
            paste0(interaction_a, "=", la, ", ", interaction_b, "=", lb),
            value = 1
          ))
        })
        interaction_grid <- shiny::tagList(
          shiny::tags$p("Multiplier per cell (1 = no interaction effect)."),
          shiny::fluidRow(cell_inputs)
        )
      }

      interaction_UI <- NULL
      if (interaction_possible) {
        interaction_UI <- htmltools::div(
          shiny::tags$hr(),
          shiny::tags$strong("Interaction (optional, 2-way only)"),
          shiny::fluidRow(
            shiny::column(6, shiny::selectInput("SAMPLESIZE-interaction_a", "Predictor A", choices = interaction_choices, selected = interaction_a)),
            shiny::column(6, shiny::selectInput("SAMPLESIZE-interaction_b", "Predictor B", choices = interaction_choices, selected = interaction_b))
          )
        )
      }

      shiny::tags$div(
        class = "doe-box",
        shiny::h4("Monte Carlo: anova (main effects only)"),
        shiny::tags$p("Relative mean per level; the first level of each predictor is the baseline (= 1)."),
        mean_inputs,
        shiny::radioButtons(
          "SAMPLESIZE-sd_model", "Variance model",
          choices = c("Proportional (constant CV)" = "cv", "Constant SD" = "sd"),
          selected = sd_model
        ),
        sd_input,

        interaction_UI,
        interaction_grid,

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

    output$sidebar_ui <- shiny::renderUI({
      subtab <- input$conditionedPanels
      if (is.null(subtab) || subtab == "Power analysis") {
        power_analysis_sidebar()
      } else if (subtab == "Monte Carlo: multiple comparison") {
        mc_multiple_comparison_sidebar()
      } else {
        mc_anova_sidebar()
      }
    })

    # React to run tests
    # ======================================================================
    shiny::observeEvent(input$calc_ttest, {
      params <- list(
        predictors = State$predictors,
        primary_factor = input$primary_factor,
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
      params <- list(
        predictors = State$predictors,
        primary_factor = input$primary_factor,
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

    # Multiple comparison
    # ======================================================================
    shiny::observeEvent(input$calc_mc, {
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

      State$mc_running <- TRUE
      State$bgp$start(
        fun = run_estimate_sample_size,
        args = params,
        on_success = function(res) {
          if (is.na(res$n)) {
            print_err(res$message)
            return(invisible())
          }
          State$n_per_level <- res$n
          add_result(State, "mc", "Monte Carlo: multiple comparison", params, methods::new("sampleSizeResult", n = res$n))
        },
        on_finally = function() State$mc_running <- FALSE
      )
    })

    # Monte carlo ANOVA
    # ======================================================================
    shiny::observeEvent(input$calc_mc_anova, {
      predictor_names <- names(State$predictors)

      means <- lapply(predictor_names, function(pred) {
        levels <- State$predictors[[pred]]
        vapply(levels, function(lvl) {
          safe <- make.names(paste(pred, lvl, sep = "_"))
          input[[paste0("mean_", safe)]]
        }, numeric(1))
      })
      names(means) <- predictor_names

      alphas <- stats::setNames(rep(input$sig_level_mc_anova, length(predictor_names)), predictor_names)
      formula_obj <- stats::formula(paste("values ~", paste(predictor_names, collapse = " + ")))

      interactions <- list()
      interaction_a <- input$interaction_a
      interaction_b <- input$interaction_b
      if (!is.null(interaction_a) && !is.null(interaction_b) &&
          interaction_a != "(none)" && interaction_b != "(none)" && interaction_a != interaction_b) {
        levels_a <- State$predictors[[interaction_a]]
        levels_b <- State$predictors[[interaction_b]]
        cells <- expand.grid(a = levels_a, b = levels_b, stringsAsFactors = FALSE)
        for (i in seq_len(nrow(cells))) {
          la <- cells$a[[i]]
          lb <- cells$b[[i]]
          safe <- make.names(paste(interaction_a, la, interaction_b, lb, sep = "_"))
          value <- input[[paste0("interaction_", safe)]]
          if (!is.null(value) && value != 1) {
            mask <- str2lang(paste0(interaction_a, ' == "', la, '" & ', interaction_b, ' == "', lb, '"'))
            interactions[[length(interactions) + 1L]] <- list(mask = mask, value = value)
          }
        }
      }

      sd_model <- input$sd_model
      if (is.null(sd_model)) sd_model <- "cv"
      cv <- if (sd_model == "cv") input$cv else NULL
      sd <- if (sd_model == "sd") input$sd_value else NULL

      params <- list(
        levels = State$predictors,
        means = means,
        cv = cv,
        sd = sd,
        interactions = interactions,
        formula = formula_obj,
        alphas = alphas,
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
