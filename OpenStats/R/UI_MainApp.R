# nocov start ui-scaffold
main_app_ui <- function() {
  upload_ui_field <- function() {
    if (Sys.getenv("RUN_MODE") != "SERVER") {
      res <- conditionalPanel(
        condition = "input.conditionedPanels == 'Data'",
        fileInput("file", "Choose CSV File",
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
  fluidPage(
    useShinyjs(),
    tagList(
      includeScript(system.file("www/FileSaver.min.js", package = "OpenStats")),
      includeScript(system.file("www/html2canvas.min.js", package = "OpenStats")),
      includeScript(system.file("www/jszip.min.js", package = "OpenStats")),
      includeScript(system.file("www/download.js", package = "OpenStats"))
    ),
    tags$head(
      includeCSS(system.file("www/styles.css", package = "OpenStats"))
    ),
    sidebarLayout(
      sidebarPanel(
        div(
          style = "display: flex; align-items: center; gap: 6px;",
          actionButton(
            "docu",
            label = NULL,
            icon = icon("question-circle")
          ),
          uiOutput("running_status") # TODO: when an error ocurred it does not vanish
        ),
        uiOutput("open_formula_editor_main"),
        uiOutput("formulaUI"),
        br(),
        uiOutput("open_split_by_groupUI"),
        uiOutput("data_splitted"),
        verbatimTextOutput("applied_filter"),
        br(),
        uiOutput("active_df"),
        br(),
        div(
          conditionalPanel(
            condition = "input.conditionedPanels == 'Data'",
            uploadUIField,
            tags$hr()
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'DataWrangling'",
            OperatorEditorSidebar("OP")
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'Visualisation'",
            visSidebarUI("VIS")
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'Assumption'",
            assSidebarUI("ASS")
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'Correlation'",
            corrSidebarUI("CORR")
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'Tests'",
            testsSidebarUI("TESTS")
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'Dose Response analysis'",
            DoseResponseSidebarUI("DOSERESPONSE")
          ),
          conditionalPanel(
            condition = "input.conditionedPanels == 'History'",
            HistorySidebarUI("HISTORY")
          )
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Data",
            DTOutput("df")
          ),
          tabPanel(
            "DataWrangling",
            OperatorEditorUI("OP")
          ),
          tabPanel(
            "Visualisation",
            visUI("VIS")
          ),
          tabPanel(
            "Assumption",
            assUI("ASS")
          ),
          tabPanel(
            "Correlation",
            corrUI("CORR")
          ),
          tabPanel(
            "Tests",
            testsUI("TESTS")
          ),
          tabPanel(
            "Dose Response analysis",
            DoseResponseUI("DOSERESPONSE")
          ),
          tabPanel(
            "History",
            HistoryEditorUI("HISTORY")
          ),
          id = "conditionedPanels"
        ),
        uiOutput("Results")
      )
    )
  )
}
# nocov end ui-scaffold
