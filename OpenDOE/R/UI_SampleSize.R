sampleSizeSidebarUI <- function(id) {
  shiny::uiOutput("SAMPLESIZE-sidebar_ui")
}

sampleSizeMainUI <- function(id) {
  shiny::tagList(
    shiny::tags$div(
      class = "doe-section",
      shiny::tabsetPanel(
        shiny::tabPanel("Power analysis", shiny::tags$p("Configure inputs in the sidebar.")),
        shiny::tabPanel("Monte Carlo: multiple comparison", shiny::tags$p("Configure inputs in the sidebar.")),
        shiny::tabPanel("Monte Carlo: anova", shiny::tags$p("Configure inputs in the sidebar.")),
        id = "SAMPLESIZE-conditionedPanels"
      )
    ),
    shiny::uiOutput("SAMPLESIZE-result_box")
  )
}
