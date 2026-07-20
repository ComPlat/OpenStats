# jsonlite drops names from a *named atomic vector* (it serialises as a bare
# JSON array), but keeps them from a named *list* (serialises as a JSON
# object) -- so every named vector in params must become a named list before
# toJSON(), or e.g. means = c(A = 0, B = 1) round-trips back with no names.
to_json_safe <- function(x) {
  if (is.list(x)) return(lapply(x, to_json_safe))
  if (!is.null(names(x))) return(as.list(x))
  x
}

# formula/call objects (mc_anova's `formula` and `interactions[[i]]$mask`)
# aren't JSON-representable; stash them as their deparsed text instead.
serialize_params <- function(type, params) {
  if (identical(type, "mc_anova")) {
    params$formula <- format(params$formula)
    params$interactions <- lapply(params$interactions, function(it) {
      it$mask <- format(it$mask)
      it
    })
  }
  to_json_safe(params)
}

format_param_value <- function(v) {
  if (is.null(v)) return("NULL")
  if (is.list(v)) {
    if (is.null(names(v))) return(paste(vapply(v, format_param_value, character(1)), collapse = "; "))
    return(paste(vapply(names(v), function(n) paste0(n, "=[", format_param_value(v[[n]]), "]"), character(1)), collapse = ", "))
  }
  paste(format(v), collapse = ", ")
}

build_history_row <- function(id, entry) {
  row_id <- paste0("HISTORY-history-row-", id)
  params_text <- paste(
    vapply(names(entry$params), function(k) paste0(k, " = ", format_param_value(entry$params[[k]])), character(1)),
    collapse = " | "
  )
  htmltools::div(
    id = row_id,
    class = "doe-box",
    htmltools::strong(paste0(id, " ", entry$label)),
    htmltools::tags$p(paste("Time:", entry$time)),
    htmltools::tags$p(params_text)
  )
}

# Builds the history-entry list shape used by the Excel export's trailing
# JSON block and the "History" tab display.
history_entries_list <- function(State) {
  stats::setNames(
    lapply(State$history, function(entry) {
      list(type = entry$type, label = entry$label, time = entry$time,
           params = serialize_params(entry$type, entry$params))
    }),
    names(State$history)
  )
}

historyServer <- function(id, State) {
  shiny::moduleServer(id, function(input, output, session) {

    output$header <- shiny::renderUI({
      if (length(State$history) == 0L) return(shiny::tags$p("No history yet."))
      shiny::tags$h4("History")
    })

    # Insert a row only for entries not yet added, instead of rebuilding the
    # whole list on every new entry.
    shiny::observe({
      if (length(State$history) == 0L) return()
      all_ids <- names(State$history)
      already <- State$registered_history
      to_add  <- setdiff(all_ids, already)
      if (!length(to_add)) return()

      lapply(to_add, function(hist_id) {
        entry <- State$history[[hist_id]]
        shiny::insertUI(
          selector = "#HISTORY-history-container",
          where = "afterBegin",
          ui = build_history_row(hist_id, entry)
        )
      })

      State$registered_history <- union(already, to_add)
    })

    # Save: one Excel sheet, results in chronological order (each with its
    # label as a title), history JSON appended at the end -- mirrors
    # OpenStats' Server_export_results.R / create_excel_file().
    output$save_history <- shiny::downloadHandler(
      filename = function() paste0("opendoe_session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
      content = function(file) {
        l <- State$results
        names(l) <- vapply(State$results, function(v) attr(v, "label"), character(1))

        history_json <- jsonlite::toJSON(history_entries_list(State), pretty = TRUE, auto_unbox = TRUE, null = "null")
        l <- c(l, stats::setNames(list(as.character(history_json)), "History (JSON)"))

        fn <- create_excel_file(l)
        if (is.null(fn)) return(invisible())
        file.copy(fn, file, overwrite = TRUE)
        unlink(fn)
      }
    )
  })
}
