DoseResponseSidebarUI <- function(id) {
  tabPanel(
    "Dose Response analysis",
    uiOutput(NS(id, "substanceNamesUI")),
    uiOutput(NS(id, "DoseResponseUI"))
  )
}

DoseResponseUI <- function(id) {
  fluidRow(
    tabsetPanel(
      id = NS(id, "results_tabs"),
      tabPanel(
        "Results Table",
        tableOutput(NS(id, "dr_result"))
      ),
      tabPanel(
        "Overview Plot",
        plotOutput(NS(id, "dr_overview_plot"), height = 700),
        actionButton(NS(id, "previousPageOverview"), "Previous page"),
        actionButton(NS(id, "nextPageOverview"), "Next page")
      ),
      tabPanel(
        "Results Plot",
        uiOutput(NS(id, "dropdown_plots")),
        plotOutput(
          NS(id, "dr_result_plot"),
          click = NS(id, "plot_click")
        ),
        actionButton(NS(id, "previousPage"), "Previous plot"),
        actionButton(NS(id, "nextPage"), "Next plot"),
        p("Click on the plot to mark outliers")
      )
    )
  )
}

DoseResponseServer <- function(id, DataModelState, ResultsState) {
  moduleServer(id, function(input, output, session) {
    DoseResponseState <- reactiveValues(
      plots = NULL,
      names = NULL, # For dropdown_plots
      overview_plots = NULL,
      currentPage = 1,
      currentPageOverview = 1,
      outliers = NULL,
      df_dr = NULL,
      promise_result_name = NULL
    )

    # Render sidebar
    output[["DoseResponseUI"]] <- renderUI({
      message <- check_dose_response(DataModelState)
      if (!is.null(message)) {
        return(
          info_div(message)
        )
      }
      div(
        style = "position: relative;",
        br(),
        checkboxInput(
          "DOSERESPONSE-xTransform",
          label = "Log transform x-axis",
          value = FALSE
        ),
        checkboxInput(
          "DOSERESPONSE-yTransform",
          label = "Log transform y-axis",
          value = FALSE
        ),
        actionButton("DOSERESPONSE-ic50", "Conduct analysis")
      )
    })
    # Render names of substances
    output[["substanceNamesUI"]] <- renderUI({
      req(!is.null(DataModelState$df))
      req(is.data.frame(DataModelState$df))
      req(inherits(DataModelState$formula, "LinearFormula"))
      colnames <- names(DataModelState$df)
      tooltip <- "Select the column which contains the names of the different substances"
      div(
        tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        selectInput(
          inputId = paste0("DOSERESPONSE-substanceNames"),
          label = "Column containing the names",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })

    reset_dr <- function() {
      DoseResponseState$plots <- NULL
      DoseResponseState$names <- NULL
      DoseResponseState$currentPage <- 1
      DoseResponseState$overview_plots <- NULL
      DoseResponseState$currentPageOverview <- 1
      DoseResponseState$outliers <- NULL
      DoseResponseState$df_dr <- NULL
      DoseResponseState$promise_result_name <- NULL
    }

    check_dr <- function() {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      req(input$substanceNames)
      print_form(DataModelState$formula)
      req(!is.null(DataModelState$formula))
    }

    run_dr <- function(df, outliers, new_name) {
      dr <- dose_response_V1_2$new(
        df, outliers, input$xTransform, input$yTransform,
        input$substanceNames, DataModelState$formula
      )
      dr$eval(ResultsState, new_name)
    }

    dr_complete <- function() {
      check_dr()
      df <- DataModelState$df
      reset_dr()
      resDF <- NULL
      resP <- NULL
      new_name <- paste0(ResultsState$counter + 1, " DoseResponse")
      e <- try(run_dr(df, NULL, new_name))
      if (!inherits(e, "try-error")) {
        DoseResponseState$promise_result_name <- new_name
      }
    }
    observe({
      if (!is.null(DoseResponseState$promise_result_name)) {
        invalidateLater(500)
        # TODO: requires check whether process is alive or died
        # Or even better when process throws an error the promise result name in DoseResponseState
        # should be set to NULL
        if (!is.null(ResultsState$all_data[[DoseResponseState$promise_result_name]])) {
          res <- ResultsState$all_data[[DoseResponseState$promise_result_name]]
          if (is.null(DoseResponseState$outliers)) {
            DoseResponseState$plots <- res@p
            DoseResponseState$names <- res@df$name
            DoseResponseState$df_dr <- res@df
            overviewPlots <- create_plot_pages(res@p)
            DoseResponseState$overview_plots <- overviewPlots
            DoseResponseState$promise_result_name <- NULL
            output$dr_result <- renderTable(res@df, digits = 6)
          } else {
            names <- DoseResponseState$names
            name <- DoseResponseState$names[DoseResponseState$currentPage]
            idx <- which(name == names)
            resDF <- res@df
            resP <- res@p
            old_plots <- DoseResponseState$plots
            old_df_dr <- DoseResponseState$df_dr
            old_plots[[idx]] <- resP[[1]]
            old_df_dr[idx, ] <- resDF
            DoseResponseState$plots <- old_plots
            DoseResponseState$df_dr <- old_df_dr
            overviewPlots <- create_plot_pages(DoseResponseState$plots)
            DoseResponseState$overview_plots <- overviewPlots
            DoseResponseState$promise_result_name <- NULL
            output$dr_result <- renderTable(DoseResponseState$df_dr, digits = 6)
          }
        }
      }
    })

    observeEvent(input$ic50, {
      dr_complete()
    })

    # Display plots
    observe({
      req(!is.null(DoseResponseState$plots))
      req(is.list(DoseResponseState$plots))
      output$dr_result_plot <- renderPlot(DoseResponseState$plots[[DoseResponseState$currentPage]])
    })

    output[["dropdown_plots"]] <- renderUI({
      req(!is.null(DoseResponseState$plots))
      req(is.list(DoseResponseState$plots))
      req(length(DoseResponseState$plots) > 1)
      req(!is.null(DoseResponseState$names))
      req(length(DoseResponseState$names) > 1)
      selectInput(
        inputId = "DOSERESPONSE-dropdown_plots",
        label = "Select plot",
        choices = DoseResponseState$names,
        selected = DoseResponseState$names[1]
      )
    })

    observeEvent(input$dropdown_plots, {
      req(!is.null(DoseResponseState$plots))
      req(is.list(DoseResponseState$plots))
      req(length(DoseResponseState$plots) > 1)
      req(!is.null(DoseResponseState$names))
      req(length(DoseResponseState$names) > 1)
      DoseResponseState$currentPage <- which(input$dropdown_plots == DoseResponseState$names)
    })

    observeEvent(input$nextPage, {
      req(!is.null(DoseResponseState$plots))
      req(is.list(DoseResponseState$plots))
      req(length(DoseResponseState$plots) > 1)
      if (DoseResponseState$currentPage < length(DoseResponseState$plots)) {
        DoseResponseState$currentPage <- DoseResponseState$currentPage + 1
      }
    })

    observeEvent(input$previousPage, {
      req(!is.null(DoseResponseState$plots))
      req(is.list(DoseResponseState$plots))
      req(length(DoseResponseState$plots) > 1)
      if (DoseResponseState$currentPage > 1) {
        DoseResponseState$currentPage <- DoseResponseState$currentPage - 1
      }
    })

    dr_partial <- function(df, name) {
      check_dr()
      resDF <- NULL
      resP <- NULL
      outliers <- NULL
      e <- try(
        {
          new_name <- paste0(ResultsState$counter + 1, " DoseResponse")
          outliers <- DoseResponseState$outliers[name]
          res <- run_dr(df, outliers, new_name)
          if (inherits(res, "try-error")) {
            m <- conditionMessage(attr(res, "condition"))
            stop(m)
          }
        },
        silent = TRUE
      )
      if (!inherits(e, "try-error")) {
        DoseResponseState$promise_result_name <- new_name
      }
    }

    observeEvent(input$plot_click, {
      req(!is.null(input$plot_click))
      req(is.data.frame(DataModelState$df))
      print_req(!is.null(DataModelState$formula), "You have to set a formula")
      try({
        click <- input$plot_click
        name_col <- input$substanceNames
        df <- DataModelState$df
        sub_df <- df[df[, name_col] == DoseResponseState$names[DoseResponseState$currentPage], ]
        f <- as.character(DataModelState$formula@formula)
        formula <- DataModelState$formula@formula
        check_formula(formula)
        dep <- f[2]
        indep <- f[3]
        x <- as.numeric(sub_df[, indep])
        y <- as.numeric(sub_df[, dep])
        x <- x[!is.na(x)]
        y <- y[!is.na(y)]
        x_range <- range(x, na.rm = TRUE)
        y_range <- range(y, na.rm = TRUE)
        x_normalized <- (x - x_range[1]) / (x_range[2] - x_range[1])
        y_normalized <- (y - y_range[1]) / (y_range[2] - y_range[1])
        click_x_normalized <- (click$x - x_range[1]) / (x_range[2] - x_range[1])
        click_y_normalized <- (click$y - y_range[1]) / (y_range[2] - y_range[1])
        distances <- sqrt((x_normalized - click_x_normalized)^2 + (y_normalized - click_y_normalized)^2)

        nearest <- which.min(distances)
        if (distances[nearest] < 0.1) {
          name <- DoseResponseState$names[[DoseResponseState$currentPage]] |> as.character()
          old_outliers <- DoseResponseState$outliers[[name]]
          if (is.null(old_outliers)) {
            DoseResponseState$outliers[[name]] <- nearest
          } else {
            if (nearest %in% old_outliers) {
              old_outliers <- old_outliers[old_outliers != nearest]
            } else {
              old_outliers <- c(old_outliers, nearest)
            }
            DoseResponseState$outliers[[name]] <- old_outliers
          }
          old_current_page <- DoseResponseState$currentPage
          dr_partial(sub_df, name)
          DoseResponseState$currentPage <- old_current_page
        }
      })
    })

    # Display overview plots
    observe({
      req(!is.null(DoseResponseState$overview_plots))
      req(is.list(DoseResponseState$overview_plots))
      output$dr_overview_plot <- renderPlot(
        DoseResponseState$overview_plots[[DoseResponseState$currentPageOverview]]
      )
    })

    observeEvent(input$nextPageOverview, {
      req(!is.null(DoseResponseState$overview_plots))
      req(is.list(DoseResponseState$overview_plots))
      req(length(DoseResponseState$overview_plots) > 1)
      if (DoseResponseState$currentPageOverview < length(DoseResponseState$overview_plots)) {
        DoseResponseState$currentPageOverview <- DoseResponseState$currentPageOverview + 1
      }
    })

    observeEvent(input$previousPageOverview, {
      req(!is.null(DoseResponseState$overview_plots))
      req(is.list(DoseResponseState$overview_plots))
      req(length(DoseResponseState$overview_plots) > 1)
      if (DoseResponseState$currentPageOverview > 1) {
        DoseResponseState$currentPageOverview <- DoseResponseState$currentPageOverview - 1
      }
    })
  })

  return(ResultsState)
}
