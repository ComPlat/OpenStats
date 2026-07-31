main_ui <- function() {
  shiny::fluidPage(
    shiny::tags$head(
      shiny::includeCSS(system.file("www/styles.css", package = "OpenDOE")),
      shiny::tags$script(shiny::HTML(
        "function doeShowDoc(el) {
           var box = document.getElementById('diagram-docs');
           if (box) box.innerHTML = el.getAttribute('data-doc');
         }"
      ))
    ),
    shiny::titlePanel("OpenDOE"),
    htmltools::div(
      shiny::downloadButton("HISTORY-save_history", "Save results & history"),
      shiny::actionLink("goto_data", "Import data", style = "margin-left: 15px;"),
      shiny::actionLink("goto_history", "History", style = "margin-left: 15px;")
    ),

    htmltools::div(
      class = "doe-diagram-wrapper",
      shiny::uiOutput("diagram"),
      htmltools::div(
        id = "diagram-docs", class = "doe-diagram-docs",
        "Hover over a step to see what it does."
      )
    ),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::conditionalPanel(
          condition = "input.conditionedPanels == 'Data'",
          dataSidebarUI("DATA")
        ),
        shiny::conditionalPanel(
          condition = "input.conditionedPanels == 'Predictors'",
          predictorsSidebarUI("PREDICTORS")
        ),
        shiny::conditionalPanel(
          condition = "input.conditionedPanels == 'Sample size'",
          sampleSizeSidebarUI("SAMPLESIZE")
        ),
        shiny::conditionalPanel(
          condition = "input.conditionedPanels == 'Design of experiment'",
          designSidebarUI("DESIGN")
        ),
        shiny::conditionalPanel(
          condition = "input.conditionedPanels == 'Random assignment'",
          randomAssignSidebarUI("RANDOMASSIGN")
        ),
        shiny::conditionalPanel(
          condition = "input.conditionedPanels == 'Finite group assignment'",
          finiteAssignSidebarUI("FINITEASSIGN")
        )
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Predictors",
            predictorsMainUI("PREDICTORS")
          ),
          shiny::tabPanel(
            "Sample size",
            sampleSizeMainUI("SAMPLESIZE")
          ),
          shiny::tabPanel(
            "Design of experiment",
            designMainUI("DESIGN")
          ),
          shiny::tabPanel(
            "Random assignment",
            randomAssignMainUI("RANDOMASSIGN")
          ),
          shiny::tabPanel(
            "Finite group assignment",
            finiteAssignMainUI("FINITEASSIGN")
          ),
          shiny::tabPanel(
            "Data",
            dataMainUI("DATA")
          ),
          shiny::tabPanel(
            "History",
            historyUI("HISTORY")
          ),
          id = "conditionedPanels"
        ),
        resultsListUI("RESULTS")
      )
    )
  )
}
