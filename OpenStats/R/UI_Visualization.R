# nocov start ui-scaffold
visSidebarUI <- function(id) {
  shiny::tabPanel(
    "Visualisation",
    htmltools::br(),
    htmltools::div(
      class = "boxed-output",
      shiny::uiOutput(shiny::NS(id, "yVarUI")),
      shiny::uiOutput(shiny::NS(id, "xVarUI")),
      shiny::textInput(shiny::NS(id, "xaxisText"), "X axis label", value = "x label"),
      shiny::textInput(shiny::NS(id, "yaxisText"), "Y axis label", value = "y label")
    ),
    htmltools::div(
      class = "boxed-output",
      shiny::conditionalPanel(
        condition = "input.VisConditionedPanels == 'Boxplot'",
        shiny::uiOutput(shiny::NS(id, "fillUI")),
        shiny::textInput(shiny::NS(id, "legendTitleFill"), "Legend title for fill", value = "Title fill"),
        shiny::selectInput(shiny::NS(id, "themeFill"), "Choose a 'fill' theme",
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
    htmltools::div(
      class = "boxed-output",
      shiny::uiOutput(shiny::NS(id, "colUI")),
      shiny::textInput(shiny::NS(id, "legendTitleCol"), "Legend title for colour", value = "Title colour"),
      shiny::selectInput(shiny::NS(id, "theme"), "Choose a 'colour' theme",
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
    htmltools::div(
      class = "boxed-output",
      shiny::uiOutput(shiny::NS(id, "facetByUI")),
      shiny::uiOutput(shiny::NS(id, "facetScalesUI"))
    ),
    htmltools::div(
      class = "boxed-output",
      shiny::radioButtons(shiny::NS(id, "xType"), "Type of x",
        choices = c(
          factor = "factor",
          numeric = "numeric"
        ),
        selected = "factor"
      ),
      shiny::uiOutput(shiny::NS(id, "XRangeUI")),
      shiny::uiOutput(shiny::NS(id, "YRangeUI"))
    )
  )
}

visUI <- function(id) {
  shiny::fluidRow(
    htmltools::br(),
    shiny::tabsetPanel(
      shiny::tabPanel(
        "Boxplot",
        htmltools::br(),
        shiny::uiOutput(shiny::NS(id, "CreatePlotBoxUI")),
        shiny::uiOutput(shiny::NS(id, "CreateModelBoxUI"))
      ),
      shiny::tabPanel(
        "Scatterplot",
        htmltools::br(),
        shiny::uiOutput(shiny::NS(id, "CreatePlotScatterUI")),
        shiny::uiOutput(shiny::NS(id, "CreateModelScatterUI"))
      ),
      shiny::tabPanel(
        "Lineplot",
        htmltools::br(),
        shiny::uiOutput(shiny::NS(id, "CreatePlotLineUI")),
        shiny::uiOutput(shiny::NS(id, "CreateModelLineUI"))
      ),
      id = "VisConditionedPanels"
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::numericInput(shiny::NS(id, "widthPlot"), "Width of plot [cm]", value = 10)
      ),
      shiny::column(
        4,
        shiny::numericInput(shiny::NS(id, "heightPlot"), "Height of plot [cm]", value = 10)
      ),
      shiny::column(
        4,
        shiny::numericInput(shiny::NS(id, "resPlot"), "Resolution of plot", value = 300)
      ),
    )
  )
}
# nocov end ui-scaffold
