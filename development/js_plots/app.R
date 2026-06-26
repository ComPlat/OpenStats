# setwd("./development/js_plots")
library(shiny)
library(jsonlite)

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/vega@5"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/vega-lite@5"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/vega-embed@6"),
    includeScript("plots.js")
  ),
  titlePanel("Vega-Lite prototype"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV"),
      uiOutput("xVarUI"),
      uiOutput("yVarUI"),
      uiOutput("colorVarUI"),
      uiOutput("facetVarUI"),
      selectInput("plotType", "Plot type",
        choices = c(
          "Scatter"   = "scatter",
          "Boxplot"   = "boxplot",
          "Line"      = "line",
          "Histogram" = "histogram"
        )
      ),
      conditionalPanel(
        condition = "input.plotType == 'boxplot'",
        checkboxInput("overlay", "Overlay points", value = FALSE)
      ),
      checkboxInput("zoom", "Enable zoom / pan", value = TRUE),
      actionButton("plot", "Plot")
    ),
    mainPanel(
      div(id = "vega-plot")
    )
  )
)

server <- function(input, output, session) {
  df <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })

  col_types <- reactive({
    req(df())
    sapply(df(), function(col) {
      if (is.numeric(col)) "quantitative"
      else if (inherits(col, c("Date", "POSIXct", "POSIXlt"))) "temporal"
      else "nominal"
    })
  })

  var_ui <- function(inputId, label, include_none = FALSE) {
    renderUI({
      req(df())
      choices <- if (include_none) c("(none)" = "", names(df())) else names(df())
      selectInput(inputId, label, choices = choices)
    })
  }

  output$xVarUI     <- var_ui("xVar",     "X variable")
  output$yVarUI     <- var_ui("yVar",     "Y variable",      include_none = TRUE)
  output$colorVarUI <- var_ui("colorVar", "Colour variable",  include_none = TRUE)
  output$facetVarUI <- var_ui("facetVar", "Facet variable",   include_none = TRUE)

  observeEvent(input$plot, {
    req(df(), input$xVar)
    types    <- col_types()
    yVar     <- if (nzchar(input$yVar))     input$yVar     else NULL
    colorVar <- if (nzchar(input$colorVar)) input$colorVar else NULL
    facetVar <- if (nzchar(input$facetVar)) input$facetVar else NULL
    session$sendCustomMessage("renderPlot", list(
      data      = jsonlite::toJSON(df(), dataframe = "rows", auto_unbox = TRUE),
      x         = input$xVar,
      y         = yVar,
      colorVar  = colorVar,
      facetVar  = facetVar,
      xType     = unname(types[input$xVar]),
      yType     = if (!is.null(yVar))     unname(types[yVar])     else NULL,
      colorType = if (!is.null(colorVar)) unname(types[colorVar]) else NULL,
      type      = input$plotType,
      overlay   = isTRUE(input$overlay),
      zoom      = isTRUE(input$zoom)
    ))
  })
}

shinyApp(ui, server)
