# nocov start ui-scaffold
ChatUI <- function(id) {
  if (is.null(getOption("OpenStats.llm"))) {
    return(shiny::helpText(
      "No LLM configured. Set e.g.",
      "options(OpenStats.llm = \"ollama/llama3.2\")",
      "or options(OpenStats.llm = \"anthropic/claude-opus-5-5\")",
      "and the matching API key as environment variable,",
      "or a function returning an ellmer chat, e.g.",
      "options(OpenStats.llm = function() ellmer::chat_openai_compatible(...))."
    ))
  }
  htmltools::tagList(
    htmltools::div(
      style = "display: flex; align-items: center; gap: 16px;",
      shiny::actionButton(paste0(id, "-clear"), "Clear chat", icon = shiny::icon("trash")),
      shiny::actionButton(paste0(id, "-copy"), "Copy chat", icon = shiny::icon("copy"))
    ),
    shinychat::chat_ui(id, show_history = FALSE)
  )
}
# nocov end ui-scaffold
