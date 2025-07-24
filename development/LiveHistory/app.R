if (!grepl("LiveHistory", getwd())) setwd("./development/LiveHistory")
library(shiny)
library(jsonlite)

ui <- fluidPage(
  tagList(
    includeScript("www/vis-network.min.js"),
    includeScript("www/custom.js")
  ),
  tags$div(id = "graph", style = "height:500px;"),
  verbatimTextOutput("clicked_node"),
  uiOutput("PageSetterHistory")
)

server <- function(input, output, session) {

  # Fake history
  history <- list(
    list(
      type = "CreateFormula",
      formula = "uptake ~ conc",
      "Model Type" = "Linear",
      details = ""
    ),
    list(
      type = "ShapiroOnData",
      formula = "uptake ~ formula",
      "Result name" = "2 ShapiroOnData"
    ),
    list(
      type = "DiagnosticPlots",
      formula = "uptake ~ formula",
      "Result name" = "3 DiagnosticPlots"
    ),
    list(
      type = "TTest",
      formula = "uptake ~ conc",
      "Confidence level of the interval" = 0.05,
      "alternative hypothesis" = "Less",
      "The two variances are" = TRUE,
      "Result name" = "4 T-Test"
    ),
    list(
      type = "CreateFormula",
      formula = "uptake ~ conc",
      "Model Type" = "Linear",
      details = ""
    ),
    list(
      type = "ShapiroOnData",
      formula = "uptake ~ formula",
      "Result name" = "2 ShapiroOnData"
    ),
    list(
      type = "DiagnosticPlots",
      formula = "uptake ~ formula",
      "Result name" = "3 DiagnosticPlots"
    ),
    list(
      type = "TTest",
      formula = "uptake ~ conc",
      "Confidence level of the interval" = 0.05,
      "alternative hypothesis" = "Less",
      "The two variances are" = TRUE,
      "Result name" = "4 T-Test"
    ),
    list(
      type = "CreateFormula",
      formula = "uptake ~ conc",
      "Model Type" = "Linear",
      details = ""
    ),
    list(
      type = "ShapiroOnData",
      formula = "uptake ~ formula",
      "Result name" = "2 ShapiroOnData"
    ),
    list(
      type = "DiagnosticPlots",
      formula = "uptake ~ formula",
      "Result name" = "3 DiagnosticPlots"
    ),
    list(
      type = "TTest",
      formula = "uptake ~ conc",
      "Confidence level of the interval" = 0.05,
      "alternative hypothesis" = "Less",
      "The two variances are" = TRUE,
      "Result name" = "4 T-Test"
    ),
    list(
      type = "CreateFormula",
      formula = "uptake ~ conc",
      "Model Type" = "Linear",
      details = ""
    ),
    list(
      type = "ShapiroOnData",
      formula = "uptake ~ formula",
      "Result name" = "2 ShapiroOnData"
    ),
    list(
      type = "DiagnosticPlots",
      formula = "uptake ~ formula",
      "Result name" = "3 DiagnosticPlots"
    ),
    list(
      type = "TTest",
      formula = "uptake ~ conc",
      "Confidence level of the interval" = 0.05,
      "alternative hypothesis" = "Less",
      "The two variances are" = TRUE,
      "Result name" = "4 T-Test"
    )
  )

  DataModelState <- reactiveValues(current_history_page = 1, number_of_history_entries = 5)

  output[["PageSetterHistory"]] <- renderUI({
    div(
      actionButton("previousHistoryPage", "Previous history entries"),
      actionButton("nextHistoryPage", "Next history entries")
    )
  })
  observeEvent(input$previousHistoryPage, {
    req(length(history) > DataModelState$number_of_history_entries)
    req(DataModelState$current_history_page >= 2)
    DataModelState$current_history_page <- DataModelState$current_history_page - 1
  })
  observeEvent(input$nextHistoryPage, {
    req(length(history) > DataModelState$number_of_history_entries)
    number_of_pages <- ceiling(length(history) / DataModelState$number_of_history_entries)
    req(DataModelState$current_history_page < number_of_pages)
    DataModelState$current_history_page <- DataModelState$current_history_page + 1
  })

  create_history_page <- function(history) {
    req(length(history) >= 1)
    req(DataModelState$current_history_page)
    current_history_page <- DataModelState$current_history_page
    start <- (current_history_page - 1) *  DataModelState$number_of_history_entries + 1
    end <- min(length(history), start + DataModelState$number_of_history_entries - 1)
    history[start:end]
  }
  create_edges <- function(history) {
    n <- length(history)
    res <- list()
    for (i in seq_len(n - 1)) {
      res[[i]] <- list(from = i, to = i + 1)
    }
    res
  }
  create_history_id <- function(history) {
    lapply(seq_along(history), function(i) {
      node <- history[[i]]
      type <- node$type
      info <- node[names(node) != "type"]
      label <- paste0(
        type, "\n",
        paste(sprintf("%s: %s", names(info), as.character(unlist(info))), collapse = "\n")
      )
      list(
        id = i,
        label = label
      )
    })
  }

  observe({
    req(length(history) >= 1)
    history_page <- create_history_page(history)
    history_with_id <- create_history_id(history_page)
    edges <- create_edges(history_page)
    session$sendCustomMessage(
      type = "drawGraph",
      message = list(
        nodes = history_with_id,
        edges = edges
      )
    )
  })

  output$clicked_node <- renderPrint({
    req(input$node_clicked)
    input$node_clicked
  })

  observeEvent(input$node_clicked, {
    cat("User clicked node:", input$node_clicked, "\n")
    # Restore state, branch, etc.
  })

  output[["PageSetterHistory"]] <- renderUI({
    number_of_pages <- ceiling(length(history) / DataModelState$number_of_history_entries)
    div(
      actionButton("previousHistoryPage", "Previous history entries"),
      span(style = "margin: 0 10px;", paste("Page", DataModelState$current_history_page, "of", number_of_pages)),
      actionButton("nextHistoryPage", "Next history entries")
    )
  })
}
shinyApp <- shiny::shinyApp(ui, server)
shiny::runApp(shinyApp, host = "0.0.0.0", port = 3838)
