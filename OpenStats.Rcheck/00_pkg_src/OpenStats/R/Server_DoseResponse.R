DoseResponseServer <- function(id, DataModelState, ResultsState) {
  shiny::moduleServer(id, function(input, output, session) {

    # Render sidebar
    output[["DoseResponseUI"]] <- shiny::renderUI({
      message <- check_dose_response(DataModelState)
      if (!is.null(message)) {
        return(
          info_div(message)
        )
      }
      htmltools::div(
        style = "position: relative;",
        htmltools::br(),
        shiny::checkboxInput(
          "DOSERESPONSE-xTransform",
          label = "Log transform x-axis",
          value = FALSE
        ),
        shiny::checkboxInput(
          "DOSERESPONSE-yTransform",
          label = "Log transform y-axis",
          value = FALSE
        ),
        shiny::actionButton("DOSERESPONSE-ic50", "Conduct analysis")
      )
    })
    # Render names of substances
    output[["substanceNamesUI"]] <- shiny::renderUI({
      shiny::req(!is.null(DataModelState$df))
      shiny::req(is.data.frame(DataModelState$df))
      shiny::req(inherits(DataModelState$formula, "LinearFormula"))
      colnames <- names(DataModelState$df)
      tooltip <- "Select the column which contains the names of the different substances"
      htmltools::div(
        shiny::tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        shiny::selectInput(
          inputId = paste0("DOSERESPONSE-substanceNames"),
          label = "Column containing the names",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    })

    # Render unit column
    render_unit_col <- function(id_name) {
      colnames <- names(DataModelState$df)
      tooltip <- "Select the column which contains the units of the different substances"
      htmltools::div(
        shiny::tags$label(
          "Dependent Variable",
          class = "tooltip",
          title = tooltip,
          `data-toggle` = "tooltip"
        ),
        shiny::selectInput(
          inputId = paste0("DOSERESPONSE-", id_name),
          label = "Column containing the units",
          choices = colnames[1:length(colnames)],
          selected = NULL
        )
      )
    }
    output[["unitNamesUI"]] <- shiny::renderUI({
      shiny::req(!is.null(DataModelState$df))
      shiny::req(is.data.frame(DataModelState$df))
      shiny::req(inherits(DataModelState$formula, "LinearFormula"))
      render_unit_col("unitNames")
    })

    # Render control group
    output[["primaryAssayUI"]] <- shiny::renderUI({
      shiny::req(!is.null(DataModelState$df))
      shiny::req(is.data.frame(DataModelState$df))
      shiny::req(inherits(DataModelState$formula, "LinearFormula"))

      message <- try(check_primary_assay(DataModelState), silent = TRUE)
      if (!is.null(message)) {
        return(info_div(message))
      }

      indep <- try({
        f <- as.character(DataModelState$formula@formula)
        f[[3L]]
      }, silent = TRUE)
      if (inherits(indep, "try-error")) return()

      choices <- unique(DataModelState$df[[indep]])
      htmltools::div(
        htmltools::h4("Primary Assay"),

        shiny::selectizeInput(
          inputId = paste0("DOSERESPONSE-neg_control_name"),
          label = "Name of the negative control",
          selected = choices[1],
          choices = choices,
          options = list(
            placeholder = 'Type to search...',
            maxOptions = 1000
          )
        ),

        shiny::selectizeInput(
          inputId = paste0("DOSERESPONSE-pos_control_name"),
          label = "Name of the positive control",
          selected = choices[1],
          choices = choices,
          options = list(
            placeholder = 'Type to search...',
            maxOptions = 1000
          )
        ),

        shiny::selectInput("DOSERESPONSE-PValAdjMethod", "Choose an adjustment method",
          choices = c(
            "holm" = "holm",
            "hochberg" = "hochberg",
            "hommel" = "hommel",
            "bonferroni" = "bonferroni",
            "sidak" = "sidak"
          )
        ),

        shiny::actionButton("DOSERESPONSE-primary_assay", "Run primary assay"),
        message,
        class = "boxed-output"
      )
    })

    check_dr <- function() {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      shiny::req(input$substanceNames)
      print_form(DataModelState$formula)
      shiny::req(!is.null(DataModelState$formula))
    }

    run_dr <- function(df, new_name) {
      dr <- get_dose_response()$new(
        df, input$ic_percentage, input$xTransform, input$yTransform,
        input$substanceNames, input$unitNames, DataModelState$formula
      )
      dr$eval(ResultsState, new_name)
    }

    shiny::observeEvent(input$ic50, {
      check_dr()
      df <- DataModelState$df
      new_name <- paste0(ResultsState$counter + 1, " DoseResponse")
      e <- try(run_dr(df, new_name))
    })

    shiny::observeEvent(input$primary_assay, {
      try({
        pa <- get_primary_assay()$new(
          DataModelState$df, DataModelState$formula,
          input$neg_control_name, input$pos_control_name,
          input$PValAdjMethod
        )
        pa$eval(ResultsState)
      })
    })

  })

}
