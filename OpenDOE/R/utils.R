log_env <- new.env(parent = emptyenv())
log_env$entries <- NULL

init_message_log <- function() {
  log_env$entries <- shiny::reactiveVal(list())
}

get_log_entries <- function() log_env$entries

append_log_entry <- function(type, message) {
  if (is.null(log_env$entries)) return(invisible(NULL))
  current <- shiny::isolate(log_env$entries())
  log_env$entries(c(current, list(list(
    type = type,
    message = message,
    time = format(Sys.time(), "%H:%M:%S")
  ))))
}

clear_log <- function() {
  if (!is.null(log_env$entries)) log_env$entries(list())
}

print_warn <- function(message) {
  append_log_entry("warning", message)
  shiny::showNotification(message, type = "warning")
}

print_err <- function(message) {
  append_log_entry("error", message)
  shiny::showNotification(message, type = "error")
}

print_req <- function(expr, message) {
  if (!expr) {
    shiny::showNotification(message, type = "message")
  }
  shiny::req(expr)
}

print_noti <- function(message) {
  shiny::showNotification(message, type = "message")
}

print_success <- function(message) {
  shiny::showNotification(message)
}

PREDICTOR_TYPES <- c("continuous_grid", "continuous", "discrete", "categorical")
CONTINUOUS_TYPES <- c("continuous_grid", "continuous")

infer_predictor_type <- function(levels) {
  if (length(levels) > 0L && !anyNA(suppressWarnings(as.numeric(levels)))) {
    "discrete"
  } else {
    "categorical"
  }
}

validate_predictor <- function(levels, type) {
  if (!(type %in% PREDICTOR_TYPES)) return(paste0("Unknown predictor type: ", type))
  if (type == "categorical") return(NULL)
  values <- suppressWarnings(as.numeric(levels))
  if (anyNA(values)) return(paste0("A ", type, " predictor needs numeric levels"))
  if (type == "continuous") {
    if (length(values) != 2L) return("A continuous predictor needs exactly two values: min, max")
    if (values[1L] >= values[2L]) return("For a continuous predictor min must be smaller than max")
  }
  if (type == "continuous_grid") {
    if (length(values) != 3L) return("A continuous_grid predictor needs exactly three values: start, end, step")
    start <- values[1L]
    end <- values[2L]
    step <- values[3L]
    if (start >= end) return("For a continuous_grid predictor start must be smaller than end")
    if (step <= 0) return("For a continuous_grid predictor step must be positive")
    n_steps <- (end - start) / step
    # end has to lie on the grid, so that start/end/step can be recovered from the levels alone
    if (abs(n_steps - round(n_steps)) > 1e-8) return("For a continuous_grid predictor (end - start) must be a multiple of step")
  }
  NULL
}

expand_predictor_levels <- function(levels, type) {
  if (type != "continuous_grid") return(levels)
  values <- as.numeric(levels)
  n_steps <- round((values[2L] - values[1L]) / values[3L])
  as.character(values[1L] + seq.int(0L, n_steps) * values[3L])
}

# continuous predictors have no levels, so they can't be crossed into a design grid
has_gridless_predictors <- function(predictor_types) {
  any(predictor_types == "continuous")
}

infer_bo_predictor_type <- function(col) {
  if (is.numeric(col)) "continuous" else "categorical"
}

predictors_from_data <- function(data, predictor_types) {
  missing_cols <- setdiff(names(predictor_types), names(data))
  if (length(missing_cols) > 0L) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
  predictors <- lapply(names(predictor_types), function(name) {
    type <- predictor_types[[name]]
    col <- data[[name]]
    col <- col[!is.na(col)]
    if (type == "categorical") return(sort(unique(as.character(col))))
    if (!is.numeric(col)) stop("A ", type, " predictor needs a numeric column: ", name)
    if (type == "discrete") return(as.character(sort(unique(col))))
    if (type == "continuous") {
      if (length(col) == 0L || min(col) >= max(col)) {
        stop("A continuous predictor needs at least two distinct values: ", name)
      }
      return(as.character(c(min(col), max(col))))
    }
    stop("Unsupported predictor type for conversion: ", type)
  })
  names(predictors) <- names(predictor_types)
  list(predictors = predictors, predictor_types = predictor_types)
}

bo_predictors <- function(data, predictor_types, bounds = NULL) {
  fixed <- intersect(names(predictor_types), names(bounds))
  conv <- predictors_from_data(data, predictor_types[setdiff(names(predictor_types), fixed)])
  predictors <- c(conv$predictors, bounds[fixed])
  list(predictors = predictors[names(predictor_types)], predictor_types = predictor_types)
}

resolve_predictor_type <- function(type, levels) {
  if (is.null(type) || type == "") infer_predictor_type(levels) else type
}

parse_levels <- function(levels_raw) {
  if (grepl("\"", levels_raw) || grepl("\'", levels_raw)) {
    print_warn("Found quotes which will be removed")
  }
  levels_raw <- gsub("\"|\'", "", levels_raw)
  levels <- trimws(strsplit(levels_raw, ",")[[1]])
  levels[levels != ""]
}

any_duplicates <- function(levels) {
  any(duplicated(levels))
}

parse_ratios <- function(ratios_raw) {
  parts <- trimws(strsplit(ratios_raw, ",")[[1]])
  parts <- parts[parts != ""]
  ratios <- suppressWarnings(as.numeric(parts))
  if (anyNA(ratios)) {
    print_warn("Ratios must be numeric")
    return(numeric(0))
  }
  ratios
}

step_defs <- function() {
  c("Predictors", "Sample size", "Design of experiment", "Random assignment", "Finite group assignment")
}

step_unlocked <- function(State, step) {
  has_design <- any(vapply(State$results, function(v) inherits(v, c("designResult", "assignmentResult")), logical(1)))
  switch(step,
    "Predictors" = TRUE,
    "Sample size" = length(State$predictors) >= 1L && !has_gridless_predictors(State$predictor_types),
    "Design of experiment" = length(State$predictors) >= 1L,
    "Random assignment" = has_design,
    "Finite group assignment" = has_design,
    TRUE
  )
}

step_display_labels <- function() {
  c(
    "Predictors" = "Define groups",
    "Sample size" = "Sample Size calculation",
    "Design of experiment" = "Design of Experiments",
    "Random assignment" = "Random assignment",
    "Finite group assignment" = "Finite assignment"
  )
}

step_explanations <- function() {
  list(
    "Predictors" =
      "You have to define the parameters of interest.<br>
       For example you want to investigate the effect of three different drugs compared to a control.<br>
       Then you can define here the group 'drug' with four different group-levels: drug-A, drug-B, drug-C and Control.",
    "Sample size" =
      "Within this section you can calculate how often a group has to be repeated (= sample size).<br>
       Considering the example with the drugs you retrieve the number of repetitions for drug-A, drug-B, drug-C and Control.",
    "Design of experiment" =
      "Here you create a list of experiments which is called design-matrix.<br>
       Each row in the matrix is one experiment for one of your group-levels (drug-A, drug-B, drug-C, and Control).",
    "Random assignment" =
      "Randomly assigns an infinite resource (e.g. 3 experimental days) to each row of your design-matrix.<br>
       This spreads every group-level (drug-A, drug-B, drug-C, Control) evenly across days.",
    "Finite group assignment" =
      "Assigns a finite resource (e.g. 50 animals with known weights) to each row of your design-matrix,<br>
       so weights are evenly balanced across group-levels (drug-A, drug-B, drug-C, Control)."
  )
}

df_result_choices <- function(State, classes = df_result_classes) {
  ids <- names(State$results)
  matching <- ids[vapply(State$results, function(v) inherits(v, classes), logical(1))]
  if (length(matching) == 0L) return(character(0))
  labels <- vapply(matching, function(id) attr(State$results[[id]], "label"), character(1))
  stats::setNames(matching, labels)
}

build_predictor_df <- function(predictors, predictor_types) {
  if (length(predictors) == 0L) return(data.frame())
  max_len <- max(vapply(predictors, length, integer(1)))
  padded <- lapply(predictors, function(lv) {
    length(lv) <- max_len
    lv[is.na(lv)] <- ""
    lv
  })
  df <- as.data.frame(padded, stringsAsFactors = FALSE, check.names = FALSE)
  colnames(df) <- paste0(names(predictors), " (", predictor_types, ")")
  df
}

add_result <- function(State, type, label, params, value) {
  State$counter <- State$counter + 1L
  id <- as.character(State$counter)
  attr(value, "label") <- paste0(id, " ", label)
  results <- State$results
  results[[id]] <- value
  State$results <- results

  history <- State$history
  history[[id]] <- list(
    type = type, label = label, params = params,
    time = format(Sys.time(), "%H:%M:%S")
  )
  State$history <- history
}
