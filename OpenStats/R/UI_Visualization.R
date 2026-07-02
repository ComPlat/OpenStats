# nocov start ui-scaffold
visSidebarUI <- function(id) {
  shiny::tabPanel(
    "Visualisation",
    htmltools::br(),
    shiny::uiOutput(shiny::NS(id, "VariableUI")),
    shiny::uiOutput(shiny::NS(id, "fillUI")),
    shiny::uiOutput(shiny::NS(id, "histUI")),
    shiny::uiOutput(shiny::NS(id, "colUI")),
    shiny::uiOutput(shiny::NS(id, "facetUI")),
    shiny::uiOutput(shiny::NS(id, "ConstraintsUI"))
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
      shiny::tabPanel(
        "Histograms",
        htmltools::br(),
        shiny::uiOutput(shiny::NS(id, "CreatePlotHistUI"))
      ),
      id = shiny::NS(id, "VisConditionedPanels")
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
