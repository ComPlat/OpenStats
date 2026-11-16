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
  client <- ellmer::chat(llm, system_prompt = llm_system_prompt_V1_2)
  client$register_tools(unname(llm_tools_V1_2(DataModelState, ResultsState)))

  # Higher priority than the observer of shinychat sending the message
  shiny::observeEvent(session$input[[paste0(id, "_user_input")]], {
    client$set_turns(llm_clean_turns_V1_2(client$get_turns()))
  }, priority = 1)

  shiny::observeEvent(session$input[[paste0(id, "-clear")]], {
    client$set_turns(list())
    shinychat::chat_clear(id)
  })

  shinychat::chat_server(id, client, history = FALSE)
}
