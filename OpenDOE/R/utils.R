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
