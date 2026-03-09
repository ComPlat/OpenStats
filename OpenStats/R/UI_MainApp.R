# nocov start ui-scaffold
main_app_ui <- function() {
  upload_ui_field <- function() {
    if (Sys.getenv("RUN_MODE") != "SERVER") {
      res <- shiny::conditionalPanel(
        condition = "input.conditionedPanels == 'Data'",
        shiny::fileInput("file", "Choose CSV File",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv",
            ".xlsx"
          )
        )
      )
      return(res)
    }
  }
  uploadUIField <- upload_ui_field()
  shiny::fluidPage(
    useShinyjs(),
    htmltools::tagList(
      shiny::includeScript(system.file("www/FileSaver.min.js", package = "OpenStats")),
      shiny::includeScript(system.file("www/html2canvas.min.js", package = "OpenStats")),
      shiny::includeScript(system.file("www/jszip.min.js", package = "OpenStats")),
      shiny::includeScript(system.file("www/download.js", package = "OpenStats"))
    ),
    shiny::tags$head(
      shiny::includeCSS(system.file("www/styles.css", package = "OpenStats"))
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        htmltools::div(
          style = "display: flex; align-items: center; gap: 6px;",
          shiny::actionButton(
            "docu",
            label = NULL,
            icon = shiny::icon("question-circle")
          ),
          shiny::uiOutput("running_status") # TODO: when an error ocurred it does not vanish
        ),
        shiny::uiOutput("open_formula_editor_main"),
        shiny::uiOutput("formulaUI"),
        htmltools::br(),
        shiny::uiOutput("open_split_by_groupUI"),
        shiny::uiOutput("data_splitted"),
        shiny::verbatimTextOutput("applied_filter"),
        htmltools::br(),
        shiny::uiOutput("active_df"),
        htmltools::br(),
        htmltools::div(
          shiny::conditionalPanel(
            condition = "input.conditionedPanels == 'Data'",
            uploadUIField,
            shiny::tags$hr()
          ),
          shiny::conditionalPanel(
            condition = "input.conditionedPanels == 'DataWrangling'",
            OperatorEditorSidebar("OP")
          ),
          shiny::conditionalPanel(
            condition = "input.conditionedPanels == 'Visualisation'",
            visSidebarUI("VIS")
          ),
          shiny::conditionalPanel(
            condition = "input.conditionedPanels == 'Assumption'",
            assSidebarUI("ASS")
          ),
          shiny::conditionalPanel(
            condition = "input.conditionedPanels == 'Correlation'",
            corrSidebarUI("CORR")
          ),
          shiny::conditionalPanel(
            condition = "input.conditionedPanels == 'Tests'",
            testsSidebarUI("TESTS")
          ),
          shiny::conditionalPanel(
            condition = "input.conditionedPanels == 'Dose Response analysis'",
            DoseResponseSidebarUI("DOSERESPONSE")
          ),
          shiny::conditionalPanel(
            condition = "input.conditionedPanels == 'History'",
            HistorySidebarUI("HISTORY")
          )
        )
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Data",
            DT::DTOutput("df")
          ),
          shiny::tabPanel(
            "DataWrangling",
            OperatorEditorUI("OP")
          ),
          shiny::tabPanel(
            "Visualisation",
            visUI("VIS")
          ),
          shiny::tabPanel(
            "Assumption",
            assUI("ASS")
          ),
          shiny::tabPanel(
            "Correlation",
            corrUI("CORR")
          ),
          shiny::tabPanel(
            "Tests",
            testsUI("TESTS")
          ),
          shiny::tabPanel(
            "Dose Response analysis",
            DoseResponseUI("DOSERESPONSE")
          ),
          shiny::tabPanel(
            "History",
            HistoryEditorUI("HISTORY")
          ),
          id = "conditionedPanels"
        ),
        shiny::uiOutput("Results")
      )
    )
  )
}
# nocov end ui-scaffold
