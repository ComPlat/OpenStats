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

infer_predictor_type <- function(levels) {
  if (length(levels) > 0L && !anyNA(suppressWarnings(as.numeric(levels)))) {
    "numeric"
  } else {
    "character"
  }
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
    "Sample size" = length(State$predictors) >= 1L,
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
