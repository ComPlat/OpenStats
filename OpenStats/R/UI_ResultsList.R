
# nocov start ui-scaffold
resultsUI <- function(id) {
  htmltools::tagList(
    shiny::uiOutput("RESULTS-Results"),
    htmltools::div(id = "RESULTS-results-container")
  )
}
# nocov end ui-scaffold
