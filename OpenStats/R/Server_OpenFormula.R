OpenFormulaServer <- function(id, DataModelState, ResultsState, MethodState, conditional_panels) {
  shiny::moduleServer(id, function(input, output, session) {
    output$open_formula_editor_main <- shiny::renderUI({
      if (conditional_panels() == "DataWrangling") {
        return()
      }
      htmltools::div(
        class = "boxed-output",
        shiny::actionButton("OPENFORMULA-open_formula_editor",
          "Open formula editor",
          title = "Open the formula editor to create or modify a formula"
        )
      )
    })
    shiny::observeEvent(input[["open_formula_editor"]], {
      print_req(is.data.frame(DataModelState$df), "The dataset is missing")
      shiny::showModal(shiny::modalDialog(
        title = htmltools::div(style = "display: flex; align-items: center; justify-content: space-between;",
          htmltools::span("FormulaEditor"),
          shiny::actionButton("FO-formula_docu", label = NULL, icon = shiny::icon("question-circle"))
        ),
        FormulaEditorUI("FO"),
        easyClose = TRUE,
        size = "l",
        footer = htmltools::tagList(
          shiny::modalButton("Close")
        )
      ))
    })

    output$formulaUI <- shiny::renderUI({
      if (conditional_panels() == "DataWrangling") {
        return()
      } else {
        shiny::renderUI({
          if (inherits(DataModelState$formula, "LinearFormula")) {
            htmltools::div(
              class = "var-box-output",
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  htmltools::p("Linear model"),
                  deparse(DataModelState$formula@formula),
                )
                # shiny::column(
                #   width = 6,
                #   shiny::actionButton(
                #     "open_predictor_editor",
                #     "Open the prediction editor",
                #     title = "Open the prediction editor to apply a model to (new) data"
                #   )
                # )
              )
            )
          } else if (inherits(DataModelState$formula, "GeneralisedLinearFormula")) {
            htmltools::div(
              class = "var-box-output",
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  htmltools::p("Generalised Linear Model"),
                  deparse(DataModelState$formula@formula),
                  htmltools::br(),
                  paste0("Family: ", deparse(DataModelState$formula@family)),
                  htmltools::br(),
                  paste0("Link fct.: ", deparse(DataModelState$formula@link_fct))
                )
                # shiny::column(
                #   width = 6,
                #   shiny::actionButton(
                #     "open_predictor_editor",
                #     "Open the prediction editor",
                #     title = "Open the prediction editor to apply a model to (new) data"
                #   )
                # )
              )
            )
          } else if (inherits(DataModelState$formula, "OptimFormula")) {
            htmltools::div(
              class = "var-box-output",
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  htmltools::p("Optimization Model"),
                  deparse(DataModelState$formula@formula),
                  htmltools::br(),
                  paste0("Optimization method: ", DataModelState$formula@method),
                  htmltools::br(),
                  paste0("Lower boundary: ", deparse(DataModelState$formula@lower)),
                  paste0("Upper boundary: ", deparse(DataModelState$formula@upper)),
                  htmltools::br(),
                  paste0("Seed: ", deparse(DataModelState$formula@seed))
                )
                # shiny::column(
                #   width = 6,
                #   shiny::actionButton(
                #     "open_predictor_editor",
                #     "Open the prediction editor",
                #     title = "Open the prediction editor to apply a model to (new) data"
                #   )
                # )
              )
            )
          } else if (inherits(DataModelState$formula, "LinearMixedFormula")) {
            htmltools::div(
              class = "var-box-output",
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  htmltools::p("Linear Mixed model"),
                  deparse(DataModelState$formula@formula),
                )
              )
            )
          } else {
            ""
          }
        })
      }
    })
  })
}
