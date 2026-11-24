library(tinytest)

eln_row <- function(uuid, ...) {
  values <- list(...)
  list(uuid = uuid, cells = lapply(names(values), function(n) list(id = paste0("id-", n), name = n, value = values[[n]])))
}

# Shape of development/labimotion_example_ext_app.json
animal_planning_mock <- function() {
  list(
    id = "2", request_id = "SSC-1",
    element_info = list(id = "140", name = "New Animal Study Protocol", short_label = "CHI-AT1",
                        klass_uuid = "klass-1", version = "1.33"),
    Selection_Controls = list(name = "Define the Controls and Comparators", data = list(
      eln_row("c1", Name = "Cd44 ft/ft", Type = "Genetic Control", Description = "Cd44 ft/ft without",
              `In case of Failure` = "")
    )),
    Outcome_Measures_Table = list(name = "Outcome Measures", data = list(
      eln_row("o1", `Outcome Measure` = "Tumor volume", `Outcome Role` = "Primary Outcome",
              `Outcome Type` = "Continuous", Unit = "mm3"),
      eln_row("o2", `Outcome Measure` = "Tumor weight", `Outcome Role` = "Secondary Outcome",
              `Outcome Type` = "Continuous", Unit = "mg")
    )),
    Predictors = list(name = "Predictor Variables", data = list(
      eln_row("p1", Variable = "Genotype", `Predictor Role` = "Primary Factor", `Predictor Type` = "Categorial",
              `Value / Range` = "Cd44 ft/ft, Cd44 ko ,", Unit = "", `Model Term` = "Fixed Effect"),
      eln_row("p2", Variable = "Cage", `Predictor Role` = "Confounder", `Predictor Type` = "Categorial",
              `Value / Range` = "1", `Model Term` = "Random Effect")
    ))
  )
}

test_parse_eln_animal_planning <- function() {
  res <- OpenDOE:::parse_eln_animal_planning(animal_planning_mock())
  expect_equal(res$eln$request_id, "SSC-1")
  expect_equal(res$eln$klass_uuid, "klass-1")
  expect_equal(res$response, "Tumor volume")

  genotype <- res$predictors[[1]]
  expect_equal(genotype$uuid, "p1")
  expect_equal(genotype$role, "primary_factor")
  expect_equal(genotype$type, "factor")
  expect_equal(genotype$levels, c("Cd44 ft/ft", "Cd44 ko"))
  expect_true(is.na(genotype$unit))
  expect_equal(genotype$model_term, "fixed")
  expect_equal(res$predictors[[2]]$model_term, "random")

  expect_equal(vapply(res$outcomes, `[[`, "", "uuid"), c("o1", "o2"))
  expect_equal(res$outcomes[[2]]$role, "secondary")

  expect_equal(res$controls[[1]]$uuid, "c1")
  expect_equal(res$controls[[1]]$type, "genetic")
  expect_true(is.na(res$controls[[1]]$predictor))
  expect_true(is.na(res$controls[[1]]$in_case_of_failure))

  expect_equal(res$notes, "Random effect Cage is not supported yet and ignored")
}
test_parse_eln_animal_planning()

test_parse_eln_animal_planning_lenient <- function() {
  x <- animal_planning_mock()
  x$Selection_Controls <- NULL
  x$Predictors$data[[1]]$cells[[3]]$value <- "Something new"
  x$Predictors$data[[2]]$cells[[1]]$value <- ""
  x$Outcome_Measures_Table$data[[1]]$cells[[2]]$value <- "Secondary Outcome"
  x$Outcome_Measures_Table$data[[2]]$cells[[3]]$value <- "Ordinal"
  res <- OpenDOE:::parse_eln_animal_planning(x)

  expect_equal(res$controls, list())
  expect_true(is.na(res$predictors[[1]]$type))
  expect_true(is.na(res$predictors[[2]]$name))
  expect_true(is.na(res$response))
  expect_true("Predictor Genotype has no known type" %in% res$notes)
  expect_true("Random effect unnamed predictor is not supported yet and ignored" %in% res$notes)
  expect_true("No primary outcome defined" %in% res$notes)
}
test_parse_eln_animal_planning_lenient()

test_eln_controls_start_unassigned <- function() {
  x <- animal_planning_mock()
  x$Selection_Controls$data <- rep(x$Selection_Controls$data, 3)
  x$Selection_Controls$data[[2]]$cells[[2]]$value <- "Negativ control"
  x$Selection_Controls$data[[3]]$cells[[2]]$value <- "Negative control"
  res <- OpenDOE:::parse_eln_animal_planning(x)
  expect_true(all(is.na(vapply(res$controls, `[[`, "", "predictor"))))
  expect_equal(vapply(res$controls, `[[`, "", "type"), c("genetic", "negative", "negative"))
}
test_eln_controls_start_unassigned()

test_import_eln_sample_size_keeps_doe_flow_off <- function() {
  options(OpenDOE.background = FALSE)
  app <- OpenDOE:::app()
  checks <- logical(0)
  shiny::testServer(app$server, {
    res <- OpenDOE:::import_eln_sample_size(State, animal_planning_mock())
    session$flushReact()
    checks[1] <<- identical(State$eln_sample_size$eln$request_id, "SSC-1")
    checks[2] <<- identical(res$response, "Tumor volume")
    checks[3] <<- is.null(State$eln)
    checks[4] <<- identical(names(State$predictors), "Genotype")
    checks[5] <<- identical(State$predictor_types, c(Genotype = "categorical"))
    checks[6] <<- is.na(State$eln_sample_size$controls[[1]]$level)
  })
  expect_true(all(checks))
}
test_import_eln_sample_size_keeps_doe_flow_off()

test_eln_sample_size_predictors_filter <- function() {
  x <- animal_planning_mock()
  x$Predictors$data <- c(x$Predictors$data, list(
    eln_row("p3", Variable = "Age", `Predictor Role` = "Covariate", `Predictor Type` = "Continuous", `Value / Range` = "4, 8"),
    eln_row("p4", Variable = "Sex", `Predictor Role` = "Secondary Factor", `Predictor Type` = "Categorial",
            `Value / Range` = "m, f, m", `Model Term` = "none"),
    eln_row("p5", Variable = "Sex", `Predictor Role` = "Secondary Factor", `Predictor Type` = "Ordinal", `Value / Range` = "x, y"),
    eln_row("p6", Variable = "Diet", `Predictor Role` = "Secondary Factor", `Predictor Type` = "Categorial")
  ))
  parsed <- OpenDOE:::parse_eln_animal_planning(x)
  predictors <- OpenDOE:::eln_sample_size_predictors(parsed)
  expect_equal(names(predictors), c("Genotype", "Sex"))
  expect_equal(predictors$Sex, c("m", "f"))
  expect_true("Continuous predictor Age is not supported yet and ignored" %in% parsed$notes)
  expect_equal(OpenDOE:::eln_incomplete_predictors(list(a = "x", b = c("x", "y"))), "a")
}
test_eln_sample_size_predictors_filter()

test_eln_assign_control <- function() {
  base <- list(Treatment = "DrugA", Sex = c("m", "f"))
  controls <- list(list(name = "Vehicle", predictor = "Treatment", level = "Vehicle"))
  moved <- OpenDOE:::eln_assign_control(controls, 1, "Sex", base)
  expect_equal(moved[[1]]$predictor, "Sex")
  expect_equal(moved[[1]]$level, "Vehicle")
  cleared <- OpenDOE:::eln_assign_control(controls, 1, "", base)
  expect_true(is.na(cleared[[1]]$predictor))
  expect_true(is.na(cleared[[1]]$level))
  expect_identical(OpenDOE:::eln_assign_control(controls, 2, "Sex", base), controls)
  expect_identical(OpenDOE:::eln_assign_control(controls, 1, NULL, base), controls)

  preset <- OpenDOE:::eln_set_control_levels(
    list(list(name = "Vehicle", predictor = "Treatment", level = NA_character_),
         list(name = "Other", predictor = NA_character_, level = NA_character_),
         list(name = "Cage control", predictor = "Cage", level = NA_character_)),
    base
  )
  expect_equal(preset[[1]]$level, "Vehicle")
  expect_true(is.na(preset[[2]]$level))
  expect_true(is.na(preset[[3]]$level))
}
test_eln_assign_control()

test_eln_merge_controls <- function() {
  base <- list(Treatment = "DrugA", Sex = c("m", "f"))
  controls <- list(
    list(name = "Vehicle", predictor = "Treatment", level = "Vehicle"),
    list(name = "DrugA", predictor = "Treatment", level = "DrugA"),
    list(name = "Other", predictor = NA_character_, level = NA_character_)
  )
  merged <- OpenDOE:::eln_merge_controls(base, controls)
  expect_equal(merged$Treatment, c("DrugA", "Vehicle"))
  expect_equal(merged$Sex, c("m", "f"))
  expect_identical(OpenDOE:::eln_merge_controls(base, controls[3]), base)
}
test_eln_merge_controls()

test_eln_sample_size_output_mc_anova <- function() {
  ess <- OpenDOE:::parse_eln_animal_planning(animal_planning_mock())
  levels <- list(Genotype = c("Cd44 ft/ft", "Cd44 ko"))
  ess$controls <- OpenDOE:::eln_assign_control(ess$controls, 1, "Genotype", levels)
  params <- list(
    levels = levels, mu = c(10, 14), cv = 0.25, formula = values ~ Genotype,
    alphas = c(Genotype = 0.05), target_effect = "Genotype", power_target = 0.8,
    seed = 42, nsim = 200, n_min = 3, n_max = 50
  )
  result <- methods::new("sampleSizeResult", n = 6, power = OpenDOE:::sample_size_power(
    list(n = 6, power = 0.86, power_se = 0.01, power_by_effect = c(Genotype = 0.86), data = data.frame())
  ))
  out <- OpenDOE:::eln_sample_size_output(ess, result, list(type = "mc_anova", params = params), list(), software = list())

  expect_equal(out$request_id, "SSC-1")
  expect_equal(out$element_info$klass_uuid, "klass-1")
  o <- out$Output
  expect_equal(o$display$model, "n_way_anova")
  expect_equal(o$display$result, list(n_per_group = 6, n_total = 12, power = 0.86))
  expect_equal(o$display$key_settings$alpha, 0.05)
  expect_equal(o$sample_size$settings$formula, "`Tumor volume` ~ Genotype")
  expect_equal(vapply(o$analysis_plan, `[[`, "", "type"), c("Version", "CreateFormula", "ShapiroOnResiduals", "LeveneTest", "ANOVA"))
  expect_equal(o$analysis_plan[[2]]$formula, "`Tumor volume` ~ Genotype")
  expect_true(inherits(stats::as.formula(o$analysis_plan[[2]]$formula), "formula"))
  expect_equal(o$sample_size$settings$cell_means$mean, c(10, 14))
  expect_equal(o$sample_size$settings$variance_model, "cv")
  expect_equal(o$sample_size$result$power_by_effect, list(Genotype = 0.86))
  expect_true(is.null(o$sample_size$result$data))
  expect_equal(o$predictors[[1]]$uuid, "p1")
  expect_equal(vapply(o$outcomes, `[[`, "", "uuid"), c("o1", "o2"))
  expect_equal(o$controls[[1]][c("uuid", "predictor", "level")], list(uuid = "c1", predictor = "Genotype", level = "Cd44 ft/ft"))
  expect_equal(o$message, "Random effect Cage is not supported yet and ignored")

  json <- jsonlite::fromJSON(OpenDOE:::eln_sample_size_json(out), simplifyVector = FALSE)
  expect_equal(json$Output$predictors[[1]]$levels, list("Cd44 ft/ft", "Cd44 ko"))
}
test_eln_sample_size_output_mc_anova()

test_eln_sample_size_output_pairwise <- function() {
  ess <- OpenDOE:::parse_eln_animal_planning(animal_planning_mock())
  params <- list(
    means = c(wt = 1, ko = 2), sds = c(wt = 1, ko = 1), power_target = 0.8, alpha = 0.05,
    mcc = "holm", family = "any", nsim = 100, n_min = 2, n_max = 50, seed = 1
  )
  result <- methods::new("sampleSizeResult", n = 10)
  out <- OpenDOE:::eln_sample_size_output(
    ess, result, list(type = "mc_wilcoxon", params = params),
    list(Genotype = c("wt", "ko")), software = list()
  )
  o <- out$Output
  expect_equal(o$display$model, "pairwise_wilcoxon_test")
  expect_false(o$display$assumptions$normally_distributed)
  expect_equal(o$display$result, list(n_per_group = 10, n_total = 20))
  expect_equal(o$sample_size$settings$means, list(wt = 1, ko = 2))
  expect_true(is.null(o$sample_size$settings$variance_model))
  expect_true(is.na(o$display$key_settings$target_effect))
  plan <- o$analysis_plan
  expect_equal(vapply(plan, `[[`, "", "type"), c("Version", "CreateFormula", "PairwiseComparison"))
  expect_equal(plan[[3]][c("P-value", "Adjusted p value method", "Method")], list(`P-value` = 0.05, `Adjusted p value method` = "holm", Method = "Wilcox"))
  expect_error(OpenDOE:::eln_sample_size_output(ess, result, list(type = "nope", params = list()), list()), "Unknown")
}
test_eln_sample_size_output_pairwise()

test_eln_send_sample_size_controls <- function() {
  options(OpenDOE.background = FALSE)
  app <- OpenDOE:::app()
  checks <- logical(0)
  shiny::testServer(app$server, {
    checks[1] <<- is.null(output$`ELN-send_sample_size_controls`)
    OpenDOE:::import_eln_sample_size(State, animal_planning_mock())
    session$flushReact()
    checks[2] <<- is.null(output$`ELN-send_sample_size_controls`)
    params <- list(predictors = State$predictors, primary_factor = "Genotype", cohens_d = 0.8, sig_level = 0.05, desired_power = 0.8)
    OpenDOE:::add_result(State, "ttest", "Power analysis: t-test", params, methods::new("sampleSizeResult", n = 26))
    session$flushReact()
    checks[3] <<- grepl("ELN-send_sample_size", output$`ELN-send_sample_size_controls`$html, fixed = TRUE)
    out <- OpenDOE:::eln_sample_size_output(State$eln_sample_size, State$results[["1"]], State$history[["1"]], State$predictors, software = list())
    checks[4] <<- identical(out$Output$display$result$n_total, 52)
    checks[5] <<- identical(out$Output$analysis_plan[[5]]$type, "TTest")
  })
  expect_true(all(checks))
}
test_eln_send_sample_size_controls()
