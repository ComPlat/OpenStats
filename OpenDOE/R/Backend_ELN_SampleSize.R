# method = "SampleSizeCalculation": url points to the export of the LabIMotion element
# "Animal Study Protocol" (layers Predictors, Outcome Measures, Controls).
ELN_SAMPLE_SIZE_METHOD <- "SampleSizeCalculation"

# Select keys of the template (template.json, select_options). The ELN may send key or label,
# and they differ for some options, so both are listed.
ELN_PREDICTOR_ROLES <- c(
  "Primary Factor" = "primary_factor", "Secondary Factor" = "secondary_factor",
  "Covariate" = "covariate", "Confounder" = "confounder", "Blocking variable" = "blocking"
)
ELN_VARIABLE_TYPES <- c(Categorial = "factor", Categorical = "factor", Ordinal = "ordinal", Continuous = "numeric")
ELN_MODEL_TERMS <- c("Fixed Effect" = "fixed", "Random Effect" = "random", unknown = NA, none = NA)
ELN_OUTCOME_ROLES <- c("Primary Outcome" = "primary", "Secondary Outcome" = "secondary", "Exploratory Outcome" = "exploratory")
ELN_CONTROL_TYPES <- c(
  "Positive control" = "positive", "Positive Control" = "positive",
  "Negativ control" = "negative", "Negative control" = "negative",
  Sham = "sham", "Genetic Control" = "genetic"
)

eln_cell <- function(row, name) {
  for (cell in row$cells) {
    if (identical(cell$name, name)) {
      value <- cell$value
      if (is.null(value) || length(value) == 0L) return(NA_character_)
      value <- trimws(as.character(value[[1L]]))
      if (!nzchar(value)) return(NA_character_)
      return(value)
    }
  }
  NA_character_
}

# Unknown select values become NA instead of an error: the integration is still work in progress.
eln_select <- function(value, map) {
  if (is.na(value) || !(value %in% names(map))) return(NA_character_)
  unname(map[[value]])
}

eln_table_rows <- function(x, key) {
  rows <- x[[key]]$data
  if (is.null(rows)) list() else rows
}

eln_split_levels <- function(value) {
  if (is.na(value)) return(character(0))
  levels <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
  levels[nzchar(levels)]
}

eln_row_uuid <- function(row) if (is.null(row$uuid)) NA_character_ else as.character(row$uuid)

eln_parse_predictor <- function(row) {
  list(
    uuid = eln_row_uuid(row),
    name = eln_cell(row, "Variable"),
    role = eln_select(eln_cell(row, "Predictor Role"), ELN_PREDICTOR_ROLES),
    type = eln_select(eln_cell(row, "Predictor Type"), ELN_VARIABLE_TYPES),
    levels = eln_split_levels(eln_cell(row, "Value / Range")),
    unit = eln_cell(row, "Unit"),
    model_term = eln_select(eln_cell(row, "Model Term"), ELN_MODEL_TERMS),
    comment = eln_cell(row, "Comment")
  )
}

eln_parse_outcome <- function(row) {
  list(
    uuid = eln_row_uuid(row),
    name = eln_cell(row, "Outcome Measure"),
    role = eln_select(eln_cell(row, "Outcome Role"), ELN_OUTCOME_ROLES),
    type = eln_select(eln_cell(row, "Outcome Type"), ELN_VARIABLE_TYPES),
    acquisition_method = eln_cell(row, "Acquisition Method"),
    device = eln_cell(row, "Device"),
    unit = eln_cell(row, "Unit"),
    timepoints = eln_cell(row, "Timepoint(s)"),
    relevance = eln_cell(row, "Relevance for the study objectives")
  )
}

# The controls table has no link to a predictor; the ELN users list them in predictor order,
# so the i-th control is preset to the i-th predictor and can be reassigned in OpenDOE.
eln_parse_control <- function(row, predictor) {
  list(
    uuid = eln_row_uuid(row),
    name = eln_cell(row, "Name"),
    type = eln_select(eln_cell(row, "Type"), ELN_CONTROL_TYPES),
    description = eln_cell(row, "Description"),
    in_case_of_failure = eln_cell(row, "In case of Failure"),
    predictor = predictor,
    level = NA_character_
  )
}

eln_sample_size_notes <- function(predictors, outcomes) {
  notes <- character(0)
  for (p in predictors) {
    label <- if (is.na(p$name)) "unnamed predictor" else p$name
    if (identical(p$model_term, "random")) notes <- c(notes, paste0("Random effect ", label, " is not supported yet and ignored"))
    if (p$role %in% c("covariate", "blocking")) notes <- c(notes, paste0("Predictor ", label, " (", p$role, ") is not supported yet"))
    if (is.na(p$type)) notes <- c(notes, paste0("Predictor ", label, " has no known type"))
    if (identical(p$type, "numeric")) notes <- c(notes, paste0("Continuous predictor ", label, " is not supported yet and ignored"))
  }
  primary <- Filter(function(o) identical(o$role, "primary"), outcomes)
  if (length(primary) == 0L) notes <- c(notes, "No primary outcome defined")
  if (length(primary) > 1L) notes <- c(notes, "Several primary outcomes defined; the first one is used")
  if (length(primary) > 0L && !identical(primary[[1L]]$type, "numeric")) {
    notes <- c(notes, "Only continuous primary outcomes are supported yet")
  }
  notes
}

# Converts the Animal planning export (parsed with simplifyVector = FALSE).
parse_eln_animal_planning <- function(x) {
  predictors <- lapply(eln_table_rows(x, "Predictors"), eln_parse_predictor)
  outcomes <- lapply(eln_table_rows(x, "Outcome_Measures_Table"), eln_parse_outcome)
  control_rows <- eln_table_rows(x, "Selection_Controls")
  predictor_names <- vapply(predictors, function(p) p$name, character(1))
  controls <- lapply(seq_along(control_rows), function(i) {
    eln_parse_control(control_rows[[i]], if (i <= length(predictor_names)) predictor_names[[i]] else NA_character_)
  })

  roles <- vapply(outcomes, function(o) o$role, character(1))
  primary <- which(roles %in% "primary")
  response <- if (length(primary) > 0L) outcomes[[primary[[1L]]]]$name else NA_character_

  element <- x$element_info
  list(
    eln = list(
      id = x$id, request_id = x$request_id,
      element_info = element, klass_uuid = element$klass_uuid
    ),
    response = response,
    predictors = predictors,
    outcomes = outcomes,
    controls = controls,
    notes = eln_sample_size_notes(predictors, outcomes)
  )
}

eln_sample_size_importable <- function(p) {
  !is.na(p$name) && p$type %in% c("factor", "ordinal") && !identical(p$model_term, "random") &&
    !(p$role %in% c("covariate", "blocking")) && length(p$levels) > 0L
}

# Predictors with a single level are imported anyway: the missing levels (often the control)
# are added by the user in the Predictors tab.
eln_sample_size_predictors <- function(parsed) {
  keep <- Filter(eln_sample_size_importable, parsed$predictors)
  names <- vapply(keep, function(p) p$name, character(1))
  keep <- keep[!duplicated(names)]
  stats::setNames(lapply(keep, function(p) unique(p$levels)), unique(names))
}

eln_preselect_control_levels <- function(controls, predictors) {
  lapply(controls, function(control) {
    levels <- if (!is.na(control$predictor)) predictors[[control$predictor]] else NULL
    if (is.na(control$level) && !is.na(control$name) && control$name %in% levels) control$level <- control$name
    control
  })
}

eln_assign_control <- function(controls, i, field, value) {
  if (is.null(value) || i > length(controls)) return(controls)
  if (!nzchar(value)) value <- NA_character_
  if (identical(controls[[i]][[field]], value)) return(controls)
  controls[[i]][[field]] <- value
  if (field == "predictor") controls[[i]]$level <- NA_character_
  controls
}

eln_incomplete_predictors <- function(predictors) {
  names(predictors)[vapply(predictors, length, integer(1)) < 2L]
}

# Kept apart from State$eln: that one belongs to the reaction DoE flow and enables its send-back.
import_eln_sample_size <- function(State, x) {
  parsed <- parse_eln_animal_planning(x)
  predictors <- eln_sample_size_predictors(parsed)
  parsed$controls <- eln_preselect_control_levels(parsed$controls, predictors)
  State$predictors <- predictors
  State$predictor_types <- stats::setNames(rep("categorical", length(predictors)), names(predictors))
  State$eln_sample_size <- parsed
  parsed
}

# Output for the ELN (format: development/eln_example_n_way_anova.json)
# ======================================================================
ELN_SAMPLE_SIZE_SCHEMA <- "0.1"

# History type of a sampleSizeResult -> model description
ELN_SAMPLE_SIZE_MODELS <- list(
  ttest = list(model = "two_sample_ttest", method = "closed_form", normal = TRUE, homogeneous = TRUE),
  anova = list(model = "one_way_anova", method = "closed_form", normal = TRUE, homogeneous = TRUE),
  mc = list(model = "pairwise_welch_t_test", method = "monte_carlo", normal = TRUE, homogeneous = FALSE),
  mc_student_t = list(model = "pairwise_t_test", method = "monte_carlo", normal = TRUE, homogeneous = TRUE),
  mc_wilcoxon = list(model = "pairwise_wilcoxon_test", method = "monte_carlo", normal = FALSE, homogeneous = FALSE),
  mc_anova = list(model = "n_way_anova", method = "monte_carlo", normal = TRUE, homogeneous = TRUE),
  mc_kruskal = list(model = "kruskal_wallis", method = "monte_carlo", normal = FALSE, homogeneous = FALSE),
  mc_welch_anova = list(model = "welch_anova", method = "monte_carlo", normal = TRUE, homogeneous = FALSE)
)

eln_or <- function(x, y) if (is.null(x)) y else x

# Predictors the calculation actually used; the pairwise and one-way tests only keep group names.
eln_sample_size_used_predictors <- function(type, params, current) {
  switch(type,
    ttest = , anova = params$predictors,
    mc_anova = params$levels,
    current
  )
}

eln_sample_size_n_groups <- function(type, params, predictors) {
  switch(type,
    mc = , mc_student_t = , mc_wilcoxon = length(params$means),
    mc_kruskal = , mc_welch_anova = length(params[["level"]]),
    nrow(expand.grid(predictors))
  )
}

eln_sample_size_alpha <- function(params) {
  if (!is.null(params$sig_level)) return(params$sig_level)
  if (!is.null(params[["alpha"]])) return(params[["alpha"]])
  if (!is.null(params$alphas)) return(unname(params$alphas[[params$target_effect]]))
  NA_real_
}

eln_sample_size_settings <- function(type, params, response) {
  settings <- params
  settings$predictors <- NULL
  if (!is.null(params$formula)) {
    settings$formula <- paste(eln_quote_name(response), "~", paste(attr(stats::terms(params$formula), "term.labels"), collapse = " + "))
  }
  if (type == "mc_anova") {
    cells <- expand.grid(params$levels, stringsAsFactors = FALSE)
    cells$mean <- params$mu
    settings$cell_means <- cells
    settings$levels <- NULL
    settings$mu <- NULL
  }
  if (type %in% c("mc_kruskal", "mc_welch_anova")) {
    settings$level_means <- data.frame(level = params[["level"]], mean = params$mu)
    settings$level <- NULL
    settings$mu <- NULL
  }
  # [[ ]] instead of $: params$sd would partially match sds of the pairwise tests
  if (!is.null(params[["cv"]]) || !is.null(params[["sd"]])) settings$variance_model <- if (is.null(params[["sd"]])) "cv" else "sd"
  settings <- Filter(Negate(is.null), settings)
  lapply(settings, function(v) if (is.atomic(v) && !is.null(names(v))) as.list(v) else v)
}

# ELN names like "Tumor volume" are no syntactic R names; OpenStats parses the formula with as.formula.
eln_quote_name <- function(name) if (identical(make.names(name), name)) name else paste0("`", name, "`")

eln_formula <- function(response, predictor_names) {
  paste(eln_quote_name(response), "~", paste(vapply(predictor_names, eln_quote_name, character(1)), collapse = " + "))
}

# OpenStats history steps (Backend_History.R, version 1_2) that analyse the planned experiment.
# OpenStats has no pairwise Welch test: "mc" falls back to pairwise.t.test (pooled SD).
eln_analysis_plan <- function(type, params, response, predictor_names) {
  formula <- eln_formula(response, predictor_names)
  shapiro <- list(type = "ShapiroOnResiduals", formula = formula)
  levene <- list(type = "LeveneTest", formula = formula, "Data center" = "median")
  pairwise <- function(method) {
    list(
      type = "PairwiseComparison", formula = formula, "P-value" = params$alpha,
      "alternative hypothesis" = "two.sided", "Adjusted p value method" = params$mcc, Method = method
    )
  }
  steps <- switch(type,
    ttest = list(shapiro, levene, list(
      type = "TTest", formula = formula, "Confidence level of the interval" = 1 - params$sig_level,
      "alternative hypothesis" = "two.sided", "The two variances are" = "eq"
    )),
    anova = , mc_anova = list(shapiro, levene, list(type = "ANOVA")),
    mc_student_t = list(shapiro, levene, pairwise("T-Test")),
    mc = list(shapiro, pairwise("T-Test")),
    mc_wilcoxon = list(pairwise("Wilcox")),
    mc_kruskal = list(list(type = "Kruskal-Wallis Test", formula = formula)),
    mc_welch_anova = list(shapiro, list(type = "WELCH_ANOVA", formula = formula))
  )
  c(
    list(list(type = "Version", Nr = "1_2")),
    list(list(type = "CreateFormula", formula = formula, "Model Type" = "Linear", details = "")),
    steps
  )
}

eln_output_predictors <- function(ess, predictors) {
  lapply(names(predictors), function(name) {
    eln <- Find(function(p) identical(p$name, name), ess$predictors)
    list(
      name = name, uuid = eln_or(eln$uuid, NA_character_), role = eln_or(eln$role, NA_character_),
      type = eln_or(eln$type, "factor"), levels = I(as.character(predictors[[name]]))
    )
  })
}

eln_software <- function() {
  list(
    R = paste(R.version$major, R.version$minor, sep = "."),
    OpenDOE = as.character(utils::packageVersion("OpenDOE")),
    Randomization = as.character(utils::packageVersion("Randomization")),
    RNGkind = I(RNGkind())
  )
}

# ess: State$eln_sample_size; result + entry: a sampleSizeResult and its history entry.
eln_sample_size_output <- function(ess, result, entry, current_predictors, software = eln_software()) {
  model <- ELN_SAMPLE_SIZE_MODELS[[entry$type]]
  if (is.null(model)) stop("Unknown sample size calculation: ", entry$type)
  params <- entry$params
  response <- if (is.na(ess$response)) "values" else ess$response
  predictors <- eln_sample_size_used_predictors(entry$type, params, current_predictors)
  n_groups <- eln_sample_size_n_groups(entry$type, params, predictors)
  analysed <- names(predictors)
  if (entry$type %in% c("mc_kruskal", "mc_welch_anova")) analysed <- analysed[1L]
  assumptions <- list(normally_distributed = model$normal, homogeneous_variances = model$homogeneous)
  target_effect <- eln_or(params$target_effect, eln_or(params$primary_factor, NA_character_))

  res <- c(list(n_per_group = result@n, n_total = result@n * n_groups), result@power)
  key_settings <- list(
    alpha = eln_sample_size_alpha(params),
    power_target = eln_or(params$desired_power, params$power_target),
    target_effect = target_effect
  )

  list(
    id = ess$eln$id,
    request_id = ess$eln$request_id,
    element_info = ess$eln$element_info,
    Output = list(
      schema_version = ELN_SAMPLE_SIZE_SCHEMA,
      status = "success",
      message = paste(ess$notes, collapse = "; "),
      display = list(
        model = model$model, method = model$method, assumptions = assumptions,
        key_settings = key_settings,
        result = res[intersect(c("n_per_group", "n_total", "power"), names(res))]
      ),
      software = software,
      response = response,
      outcomes = lapply(ess$outcomes, function(o) o[c("uuid", "name", "role", "type", "unit")]),
      predictors = eln_output_predictors(ess, predictors),
      controls = lapply(ess$controls, function(ctrl) ctrl[c("uuid", "name", "type", "predictor", "level")]),
      design = list(type = "completely_randomized", structure = "full_factorial", balanced = TRUE, n_cells = n_groups),
      sample_size = list(
        model = model$model, method = model$method, assumptions = assumptions,
        settings = eln_sample_size_settings(entry$type, params, response),
        result = res
      ),
      analysis_plan = eln_analysis_plan(entry$type, params, response, analysed)
    )
  )
}

eln_sample_size_json <- function(output) {
  jsonlite::toJSON(output, auto_unbox = TRUE, null = "null", na = "null", digits = NA, pretty = TRUE)
}

send_eln_sample_size <- function(session, output) {
  if (!requireNamespace("COMELN", quietly = TRUE)) stop("Sending to the ELN requires the 'COMELN' package")
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  writeLines(eln_sample_size_json(output), path)
  uploader <- getExportedValue("COMELN", "upload")
  uploader(session, path, "opendoe_sample_size.json")
}
