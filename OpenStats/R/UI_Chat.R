# nocov start ui-scaffold
ChatUI <- function(id) {
  if (is.null(getOption("OpenStats.llm"))) {
    return(shiny::helpText(
      "No LLM configured. Set e.g.",
      "options(OpenStats.llm = \"ollama/llama3.2\")",
      "or options(OpenStats.llm = \"anthropic/claude-opus-5-5\")",
      "and the matching API key as environment variable."
    ))
  }
  htmltools::tagList(
    shiny::actionButton(paste0(id, "-clear"), "Clear chat", icon = shiny::icon("trash")),
    shinychat::chat_ui(id, show_history = FALSE)
  )
}
# nocov end ui-scaffold
