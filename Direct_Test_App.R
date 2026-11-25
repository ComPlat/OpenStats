models <- list(
  "kit.mistral-small-4-119b-a8b",
  "kit.deepseek-v4-flash",
  "kit.glm-5.3-flash"
)
kit_key <- function() {
  system2(
    "pass", c("show", "kit_tool_box"),
    stdout = TRUE
  )[1]
}
options(OpenStats.llm = function() {
  ellmer::chat_openai_compatible(
    base_url = "https://ki-toolbox.scc.kit.edu/api",
    model = models[[3L]],
    credentials = kit_key
  )
})
# options(OpenStats.llm = "ollama/qwen3:4b-instruct")

Sys.setenv(RUN_MODE = "LOCAL")
if (!grepl("R", getwd())) setwd("OpenStats/R")
options(shiny.launch.browser = TRUE)
files <- list.files(".")
trash <- lapply(files, source)
app <- app()
shiny::shinyApp(app$ui, app$server)
