DoseResponseServer <- function(id, DataModelState, ResultsState, MethodState) {
  shiny::moduleServer(id, function(input, output, session) {

    # Dose response UI
    # ----------------------------------------------------------------------------------------
    # Render sidebar
    DOSE_RESPONSE_KINDS <- c(
      "dose-response-continuous",
      "dose-response-binomial",
      "dose-response-both"
    )
    show_dose_response <- shiny::reactive({
      shiny::req(MethodState$method)
      if (MethodState$method != "Default") {
        shiny::req(MethodState$storage_class)
        eii <- MethodState$storage_class@element_info$info
        if (!(eii$kind %in% DOSE_RESPONSE_KINDS)) {
          return(FALSE)
        } else {
          return(TRUE)
        }
      }
      return(TRUE)
    })
    output[["Percentage"]] <- shiny::renderUI({
      if (!show_dose_response()) return()
      create_slider <- function(p) {
        p <- as.numeric(p)
        if (is.na(p)) {
          shiny::sliderInput(shiny::NS(id, "ic_percentage"), "Percentage if IC",
            min = 1, max = 99, value = 50
          )
        } else {
          htmltools::tagList(
            htmltools::tags$label(
              sprintf("Percentage: %s%%", p),
              class = "control-label"
            ),
            shinyjs::hidden(
              shiny::numericInput(shiny::NS(id, "ic_percentage"), label = NULL, value = p)
            )
          )
        }
      }
      if (MethodState$method == "Default") {
        create_slider(NA)
      } else {
        eii <- MethodState$storage_class@element_info$info
        if (eii$kind %in% DOSE_RESPONSE_KINDS) {
          create_slider(eii$parameter)
        }
      }
    })
    output[["typeSelector"]] <- shiny::renderUI({
      if (!show_dose_response()) return()
      free_dropdown <- shiny::selectInput(
        "DOSERESPONSE-type", "Response type",
        choices = c(
          "continuous" = "continuous",
          "binomial" = "binomial",
          "Poisson" = "Poisson"
        ),
        selectize = FALSE
      )
      if (MethodState$method == "Default") {
        return(free_dropdown)
      }
      eii <- MethodState$storage_class@element_info$info
      type_value <- switch(
        eii$kind,
        "dose-response-continuous" = "continuous",
        "dose-response-binomial" = "binomial",
        "dose-response-both" = "",
        NULL
      )
      if (is.null(type_value)) return()
      if (identical(type_value, "")) return(free_dropdown)
      htmltools::tagList(
        htmltools::tags$label(
          sprintf("Response type: %s", type_value),
          class = "control-label"
        ),
        shinyjs::hidden(
          shiny::textInput(shiny::NS(id, "type"), label = NULL, value = type_value)
        )
      )
    })
    output[["DoseResponseUI"]] <- shiny::renderUI({
      if (show_dose_response()) {
        message <- check_dose_response(DataModelState)
        if (!is.null(message)) {
          return(
            info_div(message)
          )
        }
      }
      if (!show_dose_response()) return()
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
      if (!show_dose_response()) return()
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
      if (!show_dose_response()) return()
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
      if (!show_dose_response()) return()
      shiny::req(!is.null(DataModelState$df))
      shiny::req(is.data.frame(DataModelState$df))
      shiny::req(inherits(DataModelState$formula, "LinearFormula"))
      render_unit_col("unitNames")
    })

    # Primary Assay UI
    # ----------------------------------------------------------------------------------------
    show_primary_assay <- shiny::reactive({
      shiny::req(MethodState$method)
      if (MethodState$method != "Default") {
        shiny::req(MethodState$storage_class)
        eii <- MethodState$storage_class@element_info$info
        if (!(eii$kind %in% c("primary-assay-percentage-continuous", "primary-assay-fold-change", "primary-assay-percentage-binomial"))) {
          return(FALSE)
        } else {
          return(TRUE)
        }
      }
      return(TRUE)
    })
    # Render control group
    output[["primaryAssayUI"]] <- shiny::renderUI({
      if (show_primary_assay()) {
        message <- try(check_primary_assay(DataModelState), silent = TRUE)
        if (!is.null(message) && !inherits(message, "try-error")) {
          return(info_div(message))
        }
      }
      if (!show_primary_assay()) return()
      shiny::req(!is.null(DataModelState$df))
      shiny::req(is.data.frame(DataModelState$df))
      shiny::req(inherits(DataModelState$formula, "LinearFormula") ||
        inherits(DataModelState$formula, "GeneralisedLinearFormula"))

      indep <- try({
        f <- as.character(DataModelState$formula@formula)
        f[[3L]]
      }, silent = TRUE)
      if (inherits(indep, "try-error")) return()

      choices <- unique(DataModelState$df[[indep]])
      l <- list()
      l[[1L]] <- shiny::selectizeInput(
        inputId = paste0("DOSERESPONSE-neg_control_name"),
        label = "Name of the negative control",
        selected = choices[1],
        choices = choices,
        options = list(
          placeholder = 'Type to search...',
          maxOptions = 1000
        )
      )
      l[[2L]] <- shiny::selectizeInput(
        inputId = paste0("DOSERESPONSE-pos_control_name"),
        label = "Name of the positive control",
        selected = choices[1],
        choices = choices,
        options = list(
          placeholder = 'Type to search...',
          maxOptions = 1000
        )
      )
      l[[3L]] <- shiny::radioButtons(
        "DOSERESPONSE-FoldOrPercentage",
        "Fold change or Percentage",
        choices = c(
          "Fold change" = "Fold change",
          "Percentage" = "Percentage"
        ),
        inline = TRUE
      )

      shiny::req(MethodState$method)
      if (MethodState$method != "Default") {
        shiny::req(MethodState$storage_class)
        eii <- MethodState$storage_class@element_info$info
        if (eii$kind == "primary-assay-percentage-continuous") {
          if (!inherits(DataModelState$formula, "LinearFormula")) {
            return(info_div("A linear formula is required"))
          }
          l[[3L]] <- htmltools::tagList(
            htmltools::tags$label(
              sprintf("Modus: %s", "Percentage"),
              class = "control-label"
            ),
            shinyjs::hidden(
              shiny::textInput(shiny::NS(id, "FoldOrPercentage"), label = NULL, value = "Percentage")
            )
          )
        } else if (eii$kind == "primary-assay-fold-change") {
          if (!inherits(DataModelState$formula, "LinearFormula")) {
            return(info_div("A linear formula is required"))
          }
          l[[3L]] <- htmltools::tagList(
            htmltools::tags$label(
              sprintf("Modus: %s", "Fold change"),
              class = "control-label"
            ),
            shinyjs::hidden(
              shiny::textInput(shiny::NS(id, "FoldOrPercentage"), label = NULL, value = "Fold change")
            )
          )
        } else if (eii$kind == "primary-assay-percentage-binomial") {
          if (!inherits(DataModelState$formula, "GeneralisedLinearFormula")) {
            return(info_div("A generalised linear formula is required"))
          }
          l[[3L]] <- htmltools::tagList(
            htmltools::tags$label(
              sprintf("Modus: %s", "None"),
              class = "control-label"
            ),
            shinyjs::hidden(
              shiny::textInput(shiny::NS(id, "FoldOrPercentage"), label = NULL, value = "None")
            )
          )
          l[[2L]] <- shinyjs::hidden(
            shiny::textInput("DOSERESPONSE-pos_control_name", label = "Mock", value = "")
          )
        }
      }

      l[[4L]] <- shiny::selectInput("DOSERESPONSE-PValAdjMethod", "Choose an adjustment method",
        choices = c(
          "holm" = "holm",
          "hochberg" = "hochberg",
          "hommel" = "hommel",
          "bonferroni" = "bonferroni",
          "sidak" = "sidak"
        )
      )

      l[[5L]] <- shiny::actionButton("DOSERESPONSE-primary_assay", "Run primary assay")

      do.call(htmltools::tagList, l)
    })

    # Dose Response Server
    # ----------------------------------------------------------------------------------------
    check_dr <- function() {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      shiny::req(input$substanceNames)
      shiny::req(input$type)
      print_form(DataModelState$formula)
      shiny::req(!is.null(DataModelState$formula))
    }

    run_dr <- function(df, new_name) {
      dr <- get_dose_response()$new(
        df, input$ic_percentage, input$xTransform, input$yTransform,
        input$substanceNames, input$unitNames, DataModelState$formula,
        input$type
      )
      dr$eval(ResultsState, new_name)
    }

    shiny::observeEvent(input$ic50, {
      check_dr()
      df <- DataModelState$df
      new_name <- paste0(ResultsState$counter + 1, " DoseResponse")
      e <- try(run_dr(df, new_name))
    })

    # Primary Assay percentage continous Server
    # ----------------------------------------------------------------------------------------
    shiny::observeEvent(input$primary_assay, {
      try({
        pa <- get_primary_assay()$new(
          DataModelState$df, DataModelState$formula,
          input$neg_control_name, input$pos_control_name,
          input$PValAdjMethod, input$FoldOrPercentage
        )
        pa$eval(ResultsState)
      })
    })

  })

}
