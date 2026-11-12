sampleSizeSidebarUI <- function(id) {
  shiny::tagList(
    shiny::uiOutput("SAMPLESIZE-sidebar_ui"),
    shiny::uiOutput("ELN-send_sample_size_controls")
  )
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
