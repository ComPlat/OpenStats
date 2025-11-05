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

visServer <- function(id, DataModelState, ResultsState) {
  moduleServer(id, function(input, output, session) {
    # Render model plots
    output[["CreateModelBoxUI"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      if (inherits(DataModelState$formula, "LinearFormula") || inherits(DataModelState$formula, "GeneralisedLinearFormula")) {
        message <- check_visualization2(DataModelState)
        if (!is.null(message)) return()
        actionButton("VIS-CreateModelBox", "Plot model")
      }
    })
    output[["CreateModelScatterUI"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      if (inherits(DataModelState$formula, "LinearFormula") || inherits(DataModelState$formula, "GeneralisedLinearFormula")) {
        message <- check_visualization2(DataModelState)
        if (!is.null(message)) return()
        actionButton("VIS-CreateModelScatter", "Plot model")
      }
    })
    output[["CreateModelLineUI"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      if (inherits(DataModelState$formula, "LinearFormula") || inherits(DataModelState$formula, "GeneralisedLinearFormula")) {
        message <- check_visualization2(DataModelState)
        if (!is.null(message)) return()
        actionButton("VIS-CreateModelLine", "Plot model")
      }
    })
    # Render axis limits
    output[["XRangeUI"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      req(input$xVar)
      x <- input$xVar
      df <- DataModelState$df
      if (is.numeric(df[, x])) {
        x_min <- min(df[[x]], na.rm = TRUE)
        padded_min <- x_min * 0.5 # Needed for boxplots
        x_max <- max(df[[x]], na.rm = TRUE)
        padded_max <- x_max * 1.25
        return(
          sliderInput(
            "VIS-XRange",
            "Select range for x axis:",
            min = padded_min,
            max = padded_max,
            value = c(padded_min, padded_max)
          )
        )
      } else {
        choices <- unique(df[[x]])
        return(
          shinyWidgets::sliderTextInput(
            "VIS-XRange",
            "Select range for x axis:",
            selected = c(choices[1], choices[length(choices)]),
            choices = choices
          )
        )
      }
    })
    output[["YRangeUI"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      req(input$yVar)
      y <- input$yVar
      df <- DataModelState$df
      if (is.numeric(df[, y])) {
        y_min <- min(df[[y]], na.rm = TRUE)
        padded_min <- y_min * 0.95
        y_max <- max(df[[y]], na.rm = TRUE)
        padded_max <- y_max * 1.05
        return(
          sliderInput(
            "VIS-YRange",
            "Select range for y axis:",
            min = padded_min,
            max = padded_max,
            value = c(padded_min, padded_max)
          )
        )
      } else {
        choices <- unique(df[[y]])
        return(
          shinyWidgets::sliderTextInput(
            "VIS-YRange",
            "Select range for x axis:",
            selected = c(choices[1], choices[length(choices)]),
            choices = choices
          )
        )
      }
    })
    # Render x and y selectInput
    output[["yVarUI"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      colnames <- names(DataModelState$df)
      tooltip <- "Select the value of the Y variable"
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("VIS-yVar"),
          label = "Y Variable",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })
    output[["xVarUI"]] <- renderUI({
      message <- check_visualization1(DataModelState)
      if (!is.null(message)) {
        return(
          info_div(message)
        )
      }
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      colnames <- names(DataModelState$df)
      tooltip <- "Select the value of the X variable"
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("VIS-xVar"),
          label = "X Variable",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })
    output[["colUI"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      colnames <- c("", names(DataModelState$df))
      tooltip <- "Select a variable  for the colour variable. By chosing this groups are formed based on the unique entries in this column. Thereby, each entry gets its own colour to distinguish the groups. Dependent on the plot type either the lines, dots or the frame of the boxes are labelled"
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("VIS-col"),
          label = "Colour Variable",
          choices = colnames[1:length(colnames)],
          selected = character(0)
        )
      )
    })
    output[["fillUI"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      colnames <- c("", names(DataModelState$df))
      tooltip <- "Select a variable  for the fill variable. By chosing this groups are formed based on the unique entries in this column. Thereby, each entry gets its own colour to distinguish the groups."
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("VIS-fill"),
          label = "Fill Variable",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })
    output[["facetByUI"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      colnames <- c("", names(DataModelState$df))
      tooltip <- "Split plot in panels based on which variable"
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("VIS-facetBy"),
          label = "Split Variable",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })
    # TODO: Why is this defined as renderUI?
    output[["facetScalesUI"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      colnames <- c("", names(DataModelState$df))
      tooltip <- "Do you want to scale the Y axis"
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        radioButtons(
          "VIS-facetScales",
          "Scaling of y axis",
          choices = c(
            free = "free",
            fixed = "fixed"
          ),
          selected = "free"
        )
      )
    })

    # Plot stuff
    plotFct <- function(method) {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      vis <- visualisation_V1_2$new(
        df = DataModelState$df, x = input$xVar, y = input$yVar,
        method = method,
        xlabel = input$xaxisText, type_of_x = input$xType,
        ylabel = input$yaxisText,
        colour_var = input$col, colour_legend_title = input$legendTitleCol,
        colour_theme = input$theme,
        fill_var = input$fill, fill_legend_title = input$legendTitleFill,
        fill_theme = input$themeFill,
        facet_var = input$facetBy, facet_y_scaling = input$facetScales,
        xrange = input$XRange, yrange = input$YRange,
        width = input$widthPlot, height = input$heightPlot, resolution = input$resPlot
      )

      p <- try({
        vis$validate()
        pl <- vis$eval(ResultsState)
      })
    }

    observeEvent(input$CreatePlotBox, {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      plotFct("box")
    })

    observeEvent(input$CreatePlotScatter, {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      plotFct("dot")
    })

    observeEvent(input$CreatePlotLine, {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      plotFct("line")
    })

    # Plot model
    # TODO: add tests for history and backend
    plot_model_fct <- function(method) {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      print_form(DataModelState$formula)
      vis <- visualisation_model_V1_2$new(
        df = DataModelState$df, formula = DataModelState$formula,
        layer = method
      )

      p <- try({
        vis$validate()
        pl <- vis$eval(ResultsState)
        exportTestValues(
          plot = pl
        )
      })
      if (inherits(p, "try-error")) {
        return()
      }
    }

    observeEvent(input$CreateModelBox, {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      plot_model_fct("box")
    })

    observeEvent(input$CreateModelScatter, {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      plot_model_fct("dot")
    })

    observeEvent(input$CreateModelLine, {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      plot_model_fct("line")
    })

  })
}
