sampleSizeSidebarUI <- function(id) {
  shiny::uiOutput("SAMPLESIZE-sidebar_ui")
}

sampleSizeMainUI <- function(id) {
  shiny::tagList(
    shiny::uiOutput("SAMPLESIZE-wizard_ui"),
    shiny::tags$div(
      class = "doe-section",
      shiny::uiOutput("SAMPLESIZE-tabs_ui")
    ),
    shiny::uiOutput("SAMPLESIZE-result_box")
  )
}
