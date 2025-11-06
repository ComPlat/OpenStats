# nocov start ui-scaffold
visSidebarUI <- function(id) {
  tabPanel(
    "Visualisation",
    br(),
    div(
      class = "boxed-output",
      uiOutput(NS(id, "yVarUI")),
      uiOutput(NS(id, "xVarUI")),
      textInput(NS(id, "xaxisText"), "X axis label", value = "x label"),
      textInput(NS(id, "yaxisText"), "Y axis label", value = "y label")
    ),
    div(
      class = "boxed-output",
      conditionalPanel(
        condition = "input.VisConditionedPanels == 'Boxplot'",
        uiOutput(NS(id, "fillUI")),
        textInput(NS(id, "legendTitleFill"), "Legend title for fill", value = "Title fill"),
        selectInput(NS(id, "themeFill"), "Choose a 'fill' theme",
          c(
            "BuGn" = "BuGn",
            "PuRd" = "PuRd",
            "YlOrBr" = "YlOrBr",
            "Greens" = "Greens",
            "GnBu" = "GnBu",
            "Reds" = "Reds",
            "Oranges" = "Oranges",
            "Greys" = "Greys"
          ),
          selectize = FALSE
        )
      )
    ),
    div(
      class = "boxed-output",
      uiOutput(NS(id, "colUI")),
      textInput(NS(id, "legendTitleCol"), "Legend title for colour", value = "Title colour"),
      selectInput(NS(id, "theme"), "Choose a 'colour' theme",
        c(
          "Accent" = "Accent",
          "Dark2" = "Dark2",
          "Paired" = "Paired",
          "Pastel1" = "Pastel1",
          "Pastel2" = "Pastel2",
          "Set1" = "Set1",
          "Set2" = "Set2",
          "Set3" = "Set3"
        ),
        selectize = FALSE
      )
    ),
    div(
      class = "boxed-output",
      uiOutput(NS(id, "facetByUI")),
      uiOutput(NS(id, "facetScalesUI"))
    ),
    div(
      class = "boxed-output",
      radioButtons(NS(id, "xType"), "Type of x",
        choices = c(
          factor = "factor",
          numeric = "numeric"
        ),
        selected = "factor"
      ),
      uiOutput(NS(id, "XRangeUI")),
      uiOutput(NS(id, "YRangeUI"))
    )
  )
}

visUI <- function(id) {
  fluidRow(
    br(),
    tabsetPanel(
      tabPanel(
        "Boxplot",
        br(),
        actionButton(NS(id, "CreatePlotBox"), "Create plot"),
        uiOutput(NS(id, "CreateModelBoxUI"))
      ),
      tabPanel(
        "Scatterplot",
        br(),
        actionButton(NS(id, "CreatePlotScatter"), "Create plot"),
        uiOutput(NS(id, "CreateModelScatterUI"))
      ),
      tabPanel(
        "Lineplot",
        br(),
        actionButton(NS(id, "CreatePlotLine"), "Create plot"),
        uiOutput(NS(id, "CreateModelLineUI"))
      ),
      id = "VisConditionedPanels"
    ),
    fluidRow(
      column(
        4,
        numericInput(NS(id, "widthPlot"), "Width of plot [cm]", value = 10)
      ),
      column(
        4,
        numericInput(NS(id, "heightPlot"), "Height of plot [cm]", value = 10)
      ),
      column(
        4,
        numericInput(NS(id, "resPlot"), "Resolution of plot", value = 300)
      ),
    )
  )
}
# nocov end ui-scaffold
