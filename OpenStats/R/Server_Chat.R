llm_system_prompt_V1_2 <- paste(
  "You are the statistics assistant inside OpenStats, a statistics app.",
  "The users are lab scientists without statistics training.",
  "The user loads the data in the app.",
  "Translate the scientific question into a model with create_formula",
  "before running tests, unless get_state shows a suitable formula.",
  "Build the model from the variables in the user's question.",
  "If other columns could matter (e.g. design or blocking factors),",
  "mention them and ask before adding them.",
  "You can only act through the provided tools; never invent results.",
  "Never announce an analysis without calling its tool.",
  "After get_state, call the analysis tool in the same turn.",
  "After create_formula, always call diagnostic_plots to check the assumptions.",
  "Interpret models with the effects (estimated means, differences, slopes),",
  "not with the raw coefficients.",
  "A p-value is the probability of a result at least this extreme if there were no effect;",
  "it is not the probability that the result is due to chance.",
  "A non-significant result does not show that there is no effect.",
  "Explain the analysis, its assumptions and the results in plain words.",
  "Answer in the language of the user."
)

llm_teach_system_prompt_V1_2 <- paste(
  "You are the statistics tutor inside OpenStats, a statistics app.",
  "The users are lab scientists without statistics training",
  "who want to learn to do their analysis themselves.",
  "Never do the analysis for the user; the user runs every step in the app.",
  "You can only read the app: get_state shows the data and the formula,",
  "get_results shows the results the user created. Never invent results.",
  "Call get_state before your first answer and use only the column names it shows;",
  "never invent column names.",
  "First ask about the statistics background of the user",
  "(beginner, some experience, advanced) and adapt the depth of your explanations.",
  "Guide with a ladder of hints:",
  "1. a question that makes the user think about the next step,",
  "2. a hint that points in the right direction,",
  "3. the concrete step.",
  "Go to the next level only when the user asks again or gets it wrong.",
  "Explain why a step is done, not only what to do.",
  "Tell the user where to click in the app, using the exact names from the app guide below.",
  "When a click path is available for the step, also call highlight so the app shows the user where to click.",
  "When the user created a result, read it with get_results and give feedback:",
  "what is right, what to reconsider and how to interpret it.",
  "A p-value is the probability of a result at least this extreme if there were no effect;",
  "it is not the probability that the result is due to chance.",
  "A non-significant result does not show that there is no effect.",
  "Answer in the language of the user."
)

# Same file as the user documentation, so the click paths stay in one place.
# Read at runtime: system.file() is not valid while the package is built
llm_app_guide_V1_2 <- function() {
  html <- paste(readLines(system.file("www/workflow.html", package = "OpenStats"), warn = FALSE), collapse = "\n")
  html <- gsub("</?code>", "'", gsub("<li>", "- ", html))
  text <- gsub("<[^>]+>", "", html)
  lines <- trimws(strsplit(text, "\n", fixed = TRUE)[[1]])
  paste(lines[nzchar(lines)], collapse = "\n")
}

llm_set_mode_V1_2 <- function(client, get_mode, DataModelState, ResultsState) {
  mode <- get_mode()
  tools <- llm_tools_V1_2(DataModelState, ResultsState, get_mode)
  if (mode == "teach") {
    client$set_system_prompt(paste0(
      llm_teach_system_prompt_V1_2, "\n\n",
      "App guide:\n", llm_app_guide_V1_2()
    ))
    tools <- tools[llm_teach_tools_V1_2]
  } else {
    client$set_system_prompt(llm_system_prompt_V1_2)
    tools <- tools[!names(tools) %in% llm_teach_only_tools_V1_2]
  }
  client$set_tools(unname(tools))
}

llm_content_text_V1_2 <- function(x) {
  if (inherits(x, "ellmer::ContentToolRequest")) {
    return(paste0("[tool call] ", x@name, " ", jsonlite::toJSON(x@arguments, auto_unbox = TRUE)))
  }
  if (inherits(x, "ellmer::ContentToolResult")) {
    if (!is.null(x@error)) {
      return(paste0("[tool error] ", conditionMessage(x@error)))
    }
    return(paste0("[tool result]\n", paste(as.character(x@value), collapse = "\n")))
  }
  if (inherits(x, "ellmer::ContentText")) {
    return(x@text)
  }
  NULL
}

llm_turns_text_V1_2 <- function(turns, mode) {
  parts <- vapply(turns, function(turn) {
    texts <- unlist(lapply(turn@contents, llm_content_text_V1_2))
    paste0("## ", turn@role, "\n", paste(texts, collapse = "\n\n"))
  }, character(1))
  paste(c(paste0("Mode: ", mode), parts), collapse = "\n\n")
}

llm_is_broken_turn_V1_2 <- function(turn) {
  if (turn@role != "assistant") {
    return(FALSE)
  }
  useful <- Filter(
    function(x) !inherits(x, "ellmer::ContentThinking"),
    turn@contents
  )
  length(useful) == 0L
}

# A cancelled response leaves an assistant turn without content
# (or a tool request without result) and every later request is rejected
llm_clean_turns_V1_2 <- function(turns) {
  turns <- Filter(Negate(llm_is_broken_turn_V1_2), turns)
  n <- length(turns)
  if (n > 0L && turns[[n]]@role == "assistant") {
    has_request <- vapply(
      turns[[n]]@contents,
      function(x) inherits(x, "ellmer::ContentToolRequest"),
      logical(1)
    )
    if (any(has_request)) {
      turns <- turns[-n]
    }
  }
  turns
}

ChatServer <- function(id, DataModelState, ResultsState) {
  llm <- getOption("OpenStats.llm")
  if (is.null(llm)) {
    return()
  }
  session <- shiny::getDefaultReactiveDomain()
  client <- ellmer::chat(llm)
  # Tools run in promise callbacks, outside of any reactive context
  get_mode <- function() {
    mode <- shiny::isolate(session$input[[paste0(id, "-mode")]])
    if (is.null(mode)) "do" else mode
  }
  llm_set_mode_V1_2(client, get_mode, DataModelState, ResultsState)

  # The tool itself has no access to the session
  client$on_tool_result(function(result) {
    if (result@request@name == "highlight" && is.null(result@error)) {
      steps <- llm_click_paths_V1_2[[result@request@arguments$path]]
      session$sendCustomMessage("highlight_path", list(steps = steps))
    }
  })

  # Same client object instead of a new one: chat_server keeps the client it got
  shiny::observeEvent(session$input[[paste0(id, "-mode")]], {
    session$sendCustomMessage("highlight_path", list(steps = list()))
    llm_set_mode_V1_2(client, get_mode, DataModelState, ResultsState)
    client$set_turns(list())
    shinychat::chat_clear(id)
  }, ignoreInit = TRUE)

  # Higher priority than the observer of shinychat sending the message
  shiny::observeEvent(session$input[[paste0(id, "_user_input")]], {
    client$set_turns(llm_clean_turns_V1_2(client$get_turns()))
  }, priority = 1)

  shiny::observeEvent(session$input[[paste0(id, "-copy")]], {
    text <- llm_turns_text_V1_2(client$get_turns(), get_mode())
    shinyjs::runjs(sprintf("navigator.clipboard.writeText(%s);", jsonlite::toJSON(text, auto_unbox = TRUE)))
    print_noti("Chat copied to the clipboard")
  })

  shiny::observeEvent(session$input[[paste0(id, "-clear")]], {
    session$sendCustomMessage("highlight_path", list(steps = list()))
    client$set_turns(list())
    shinychat::chat_clear(id)
  })

  shinychat::chat_server(id, client, history = FALSE)
}
