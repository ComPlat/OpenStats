if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

options(OpenDOE.background = FALSE)

app <- OpenDOE:::app()
srv <- app$server

test_ttest_flow <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B")
    session$setInputs(`PREDICTORS-add_predictor` = 1)
    session$setInputs(`PREDICTORS-add_to_results` = 1)

    session$setInputs(`SAMPLESIZE-primary_factor` = "treatment")
    session$setInputs(`SAMPLESIZE-cohens_d` = 0.8)
    session$setInputs(`SAMPLESIZE-sig_level_ttest` = 0.05)
    session$setInputs(`SAMPLESIZE-desired_power_ttest` = 0.8)
    session$setInputs(`SAMPLESIZE-calc_ttest` = 1)

    checks[1] <<- identical(names(State$results), c("1", "2"))
    checks[2] <<- inherits(State$results[["1"]], "predictorTable")
    checks[3] <<- inherits(State$results[["2"]], "sampleSizeResult")
    checks[4] <<- State$results[["2"]]@n == State$n_per_level
    checks[5] <<- attr(State$results[["2"]], "label") == "2 Power analysis: t-test"
    checks[6] <<- identical(names(State$history), c("1", "2"))
    checks[7] <<- State$history[["2"]]$type == "ttest"
    checks[8] <<- State$history[["2"]]$params$cohens_d == 0.8
  })
  expect_true(all(checks))
}
test_ttest_flow(srv)

test_anova_flow <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B,C")
    session$setInputs(`PREDICTORS-add_predictor` = 1)

    session$setInputs(`SAMPLESIZE-primary_factor` = "treatment")
    session$setInputs(`SAMPLESIZE-cohens_f` = 0.25)
    session$setInputs(`SAMPLESIZE-sig_level_anova` = 0.05)
    session$setInputs(`SAMPLESIZE-desired_power_anova` = 0.8)
    session$setInputs(`SAMPLESIZE-calc_anova` = 1)

    checks[1] <<- identical(names(State$results), "1")
    checks[2] <<- inherits(State$results[["1"]], "sampleSizeResult")
    checks[3] <<- State$history[["1"]]$type == "anova"
  })
  expect_true(all(checks))
}
test_anova_flow(srv)

# Walks the wizard (Backend_TestSelectionTree.R) to a given leaf by
# clicking answer-slot i at each step, e.g. walk_wizard(session, 1, 2) for
# (wizard auto-start) -> answer 1 -> answer 2. Mirrors clicking through the
# UI one question at a time; the auto-selected root answer ("1-2"/">2") is
# never clicked, only the questions the wizard actually shows.
walk_wizard <- function(session, ...) {
  for (i in c(...)) {
    id <- paste0("SAMPLESIZE-wizard_answer_", i)
    inputs <- stats::setNames(list(1), id)
    do.call(session$setInputs, inputs)
  }
}

test_mc_flow <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B,C")
    session$setInputs(`PREDICTORS-add_predictor` = 1)

    # >2 levels -> Paarweise Vergleiche (MCC) -> normal ja -> VH nein -> Welch
    walk_wizard(session, 2, 1, 2)

    session$setInputs(`SAMPLESIZE-n_groups` = 3)
    session$setInputs(`SAMPLESIZE-group_name_1` = "A", `SAMPLESIZE-group_mean_1` = 0, `SAMPLESIZE-group_sd_1` = 1)
    session$setInputs(`SAMPLESIZE-group_name_2` = "B", `SAMPLESIZE-group_mean_2` = 1, `SAMPLESIZE-group_sd_2` = 1)
    session$setInputs(`SAMPLESIZE-group_name_3` = "C", `SAMPLESIZE-group_mean_3` = 2, `SAMPLESIZE-group_sd_3` = 1)
    session$setInputs(`SAMPLESIZE-mcc` = "holm", `SAMPLESIZE-family` = "any")
    session$setInputs(`SAMPLESIZE-alpha` = 0.05, `SAMPLESIZE-power_target_mc` = 0.8)
    session$setInputs(`SAMPLESIZE-nsim` = 200, `SAMPLESIZE-n_min` = 2, `SAMPLESIZE-n_max` = 50)
    session$setInputs(`SAMPLESIZE-seed` = 42)
    session$setInputs(`SAMPLESIZE-calc_mc` = 1)

    checks[1] <<- identical(names(State$results), "1")
    checks[2] <<- inherits(State$results[["1"]], "sampleSizeResult")
    checks[3] <<- State$history[["1"]]$type == "mc"
    checks[4] <<- !isTRUE(State$mc_running)
  })
  expect_true(all(checks))
}
test_mc_flow(srv)

test_mc_student_t_flow <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B")
    session$setInputs(`PREDICTORS-add_predictor` = 1)

    # 1-2 levels -> normal ja -> VH ja (previously-missing Student t-Test leaf)
    walk_wizard(session, 1, 1)

    session$setInputs(`SAMPLESIZE-n_groups` = 2)
    session$setInputs(`SAMPLESIZE-group_name_1` = "A", `SAMPLESIZE-group_mean_1` = 0, `SAMPLESIZE-group_sd_1` = 1)
    session$setInputs(`SAMPLESIZE-group_name_2` = "B", `SAMPLESIZE-group_mean_2` = 2, `SAMPLESIZE-group_sd_2` = 1)
    session$setInputs(`SAMPLESIZE-mcc` = "none", `SAMPLESIZE-family` = "any")
    session$setInputs(`SAMPLESIZE-alpha` = 0.05, `SAMPLESIZE-power_target_mc` = 0.8)
    session$setInputs(`SAMPLESIZE-nsim` = 200, `SAMPLESIZE-n_min` = 2, `SAMPLESIZE-n_max` = 60)
    session$setInputs(`SAMPLESIZE-seed` = 42)
    session$setInputs(`SAMPLESIZE-calc_mc` = 1)

    checks[1] <<- identical(names(State$results), "1")
    checks[2] <<- inherits(State$results[["1"]], "sampleSizeResult")
    checks[3] <<- State$history[["1"]]$type == "mc_student_t"
    checks[4] <<- !isTRUE(State$mc_running)
    checks[5] <<- grepl("Student", attr(State$results[["1"]], "label"), fixed = TRUE)
  })
  expect_true(all(checks))
}
test_mc_student_t_flow(srv)

test_sample_size_visible_tabs <- function() {
  visible <- OpenDOE:::sample_size_visible_tabs
  leaf_at <- function(...) OpenDOE:::test_selection_tree_find(OpenDOE:::test_selection_tree, ...)
  checks <- logical(0)

  student_t <- leaf_at("1-2", "yes", "yes")
  welch_t <- leaf_at("1-2", "yes", "no")
  student_anova <- leaf_at(">2", "Omnibus test", "yes", "yes")
  welch_anova <- leaf_at(">2", "Omnibus test", "yes", "no")

  state1 <- list(predictors = list(treatment = c("A", "B")))

  # Student t-test leaf (closed_form_alt): Power analysis is offered
  # alongside the Monte Carlo tab the leaf routes to.
  checks[1] <- setequal(visible(state1, student_t), c("Power analysis", "Pairwise comparisons"))

  # Welch t-test leaf has no closed-form alternative -- Monte Carlo only.
  checks[2] <- identical(visible(state1, welch_t), "Pairwise comparisons")

  # No leaf chosen yet: the wizard hasn't decided which test applies, so
  # nothing is shown -- Power analysis is only reachable through the tree now.
  checks[3] <- identical(visible(state1, NULL), character(0))

  # >1 predictor: no closed-form Power analysis even for the Student ANOVA
  # leaf (power.anova.test only covers one-way designs), Monte Carlo only.
  state3 <- list(predictors = list(a = c("x", "y"), b = c("1", "2"), c = c("lo", "hi")))
  checks[4] <- identical(visible(state3, student_anova), "Monte Carlo: anova")
  checks[5] <- identical(visible(state3, NULL), character(0))

  # Student ANOVA leaf, single predictor: both options offered.
  checks[6] <- setequal(visible(state1, student_anova), c("Power analysis", "Monte Carlo: anova"))

  # Welch-ANOVA leaf has no closed-form alternative -- Monte Carlo only.
  checks[7] <- identical(visible(state1, welch_anova), "Monte Carlo: one-way test")

  # No predictors defined yet: nothing to show.
  checks[8] <- identical(visible(list(predictors = list()), NULL), character(0))

  expect_true(all(checks))
}
test_sample_size_visible_tabs()

test_mc_wilcoxon_flow <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B,C")
    session$setInputs(`PREDICTORS-add_predictor` = 1)

    # >2 levels -> Paarweise Vergleiche (MCC) -> nicht normalverteilt
    walk_wizard(session, 2, 2)

    session$setInputs(`SAMPLESIZE-n_groups` = 3)
    session$setInputs(`SAMPLESIZE-group_name_1` = "A", `SAMPLESIZE-group_mean_1` = 0, `SAMPLESIZE-group_sd_1` = 1)
    session$setInputs(`SAMPLESIZE-group_name_2` = "B", `SAMPLESIZE-group_mean_2` = 1, `SAMPLESIZE-group_sd_2` = 1)
    session$setInputs(`SAMPLESIZE-group_name_3` = "C", `SAMPLESIZE-group_mean_3` = 2, `SAMPLESIZE-group_sd_3` = 1)
    session$setInputs(`SAMPLESIZE-mcc` = "holm", `SAMPLESIZE-family` = "any")
    session$setInputs(`SAMPLESIZE-alpha` = 0.05, `SAMPLESIZE-power_target_mc` = 0.8)
    session$setInputs(`SAMPLESIZE-nsim` = 200, `SAMPLESIZE-n_min` = 2, `SAMPLESIZE-n_max` = 50)
    session$setInputs(`SAMPLESIZE-seed` = 42)
    session$setInputs(`SAMPLESIZE-calc_mc` = 1)

    checks[1] <<- identical(names(State$results), "1")
    checks[2] <<- inherits(State$results[["1"]], "sampleSizeResult")
    checks[3] <<- State$history[["1"]]$type == "mc_wilcoxon"
    checks[4] <<- !isTRUE(State$mc_running)
    checks[5] <<- grepl("Wilcoxon", attr(State$results[["1"]], "label"), fixed = TRUE)
  })
  expect_true(all(checks))
}
test_mc_wilcoxon_flow(srv)

test_mc_kruskal_flow <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B,C")
    session$setInputs(`PREDICTORS-add_predictor` = 1)

    # calc_one_way reads State$mc_one_way_means directly (normally defaulted
    # by mc_one_way_sidebar()'s render, which testServer never triggers) --
    # set it explicitly, mirroring what the UI would have produced.
    State$mc_one_way_means <- c(1, 2, 3)

    # >2 levels -> Omnibus-Test -> nicht normalverteilt (Kruskal-Wallis)
    walk_wizard(session, 1, 2)

    session$setInputs(`SAMPLESIZE-sig_level_one_way` = 0.05)
    session$setInputs(`SAMPLESIZE-power_target_one_way` = 0.8)
    session$setInputs(`SAMPLESIZE-nsim_one_way` = 200)
    session$setInputs(`SAMPLESIZE-n_min_one_way` = 3)
    session$setInputs(`SAMPLESIZE-n_max_one_way` = 60)
    session$setInputs(`SAMPLESIZE-seed_one_way` = 42)
    session$setInputs(`SAMPLESIZE-sd_model_one_way` = "sd")
    session$setInputs(`SAMPLESIZE-sd_value_one_way` = 1)
    session$setInputs(`SAMPLESIZE-calc_one_way` = 1)

    checks[1] <<- identical(names(State$results), "1")
    checks[2] <<- inherits(State$results[["1"]], "sampleSizeResult")
    checks[3] <<- State$history[["1"]]$type == "mc_kruskal"
    checks[4] <<- !isTRUE(State$mc_running)
  })
  expect_true(all(checks))
}
test_mc_kruskal_flow(srv)

test_mc_welch_anova_flow <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B,C")
    session$setInputs(`PREDICTORS-add_predictor` = 1)

    State$mc_one_way_means <- c(1, 2, 3)

    # >2 levels -> Omnibus-Test -> normal ja -> VH nein (previously-missing
    # Welch-ANOVA leaf)
    walk_wizard(session, 1, 1, 2)

    session$setInputs(`SAMPLESIZE-sig_level_one_way` = 0.05)
    session$setInputs(`SAMPLESIZE-power_target_one_way` = 0.8)
    session$setInputs(`SAMPLESIZE-nsim_one_way` = 200)
    session$setInputs(`SAMPLESIZE-n_min_one_way` = 3)
    session$setInputs(`SAMPLESIZE-n_max_one_way` = 60)
    session$setInputs(`SAMPLESIZE-seed_one_way` = 42)
    session$setInputs(`SAMPLESIZE-sd_model_one_way` = "sd")
    session$setInputs(`SAMPLESIZE-sd_value_one_way` = 1)
    session$setInputs(`SAMPLESIZE-calc_one_way` = 1)

    checks[1] <<- identical(names(State$results), "1")
    checks[2] <<- inherits(State$results[["1"]], "sampleSizeResult")
    checks[3] <<- State$history[["1"]]$type == "mc_welch_anova"
    checks[4] <<- !isTRUE(State$mc_running)
  })
  expect_true(all(checks))
}
test_mc_welch_anova_flow(srv)

test_wizard_back_and_reset <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B")
    session$setInputs(`PREDICTORS-add_predictor` = 1)

    walk_wizard(session, 1, 2)
    html_leaf <- as.character(output[["SAMPLESIZE-wizard_ui"]])
    checks[1] <<- any(grepl("Welch", html_leaf, fixed = TRUE))

    session$setInputs(`SAMPLESIZE-wizard_back` = 1)
    html_back <- as.character(output[["SAMPLESIZE-wizard_ui"]])
    checks[2] <<- any(grepl("Variances homogeneous", html_back, fixed = TRUE))
    checks[3] <<- !any(grepl("doe-wizard-result", html_back, fixed = TRUE))

    session$setInputs(`SAMPLESIZE-wizard_reset` = 1)
    html_reset <- as.character(output[["SAMPLESIZE-wizard_ui"]])
    # Reset returns to the auto-derived starting point ("Normally
    # distributed?" for this 2-level predictor), not the root "Number of
    # levels?" question -- that one is never asked, see
    # wizard_start_node() in Server_SampleSize.R.
    checks[4] <<- any(grepl("Normally distributed", html_reset, fixed = TRUE))
    checks[5] <<- !any(grepl("Number of levels", html_reset, fixed = TRUE))
  })
  expect_true(all(checks))
}
test_wizard_back_and_reset(srv)

test_wizard_skips_levels_question <- function(srv) {
  checks <- logical(0)
  shiny::testServer(srv, {
    session$setInputs(`PREDICTORS-predictor_name` = "treatment")
    session$setInputs(`PREDICTORS-predictor_levels` = "A,B,C")
    session$setInputs(`PREDICTORS-add_predictor` = 1)

    # >2 levels, derived from the single predictor's 3 levels -- first
    # visible question should already be "Goal?", not "Number of levels?".
    html <- as.character(output[["SAMPLESIZE-wizard_ui"]])
    checks[1] <<- any(grepl("Goal?", html, fixed = TRUE))
    checks[2] <<- !any(grepl("Number of levels", html, fixed = TRUE))
    checks[3] <<- any(grepl("(automatic)", html, fixed = TRUE))
  })
  expect_true(all(checks))
}
test_wizard_skips_levels_question(srv)
