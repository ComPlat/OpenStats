# nocov start ui-scaffold
FormulaEditorUI <- function(id) {
  ui <- shiny::fluidPage(
    shiny::fluidRow(
      shiny::uiOutput(shiny::NS(id, "model")),
      shiny::uiOutput(shiny::NS(id, "predefined_modelsUI")),
      shiny::column(
        width = 6,
        shiny::uiOutput(shiny::NS(id, "colnames_dropdown")),
        htmltools::div(
          shiny::selectInput(shiny::NS(id, "model_type"), "Change the model type (optional)",
            c(
              "Linear" = "Linear",
              "Generalised Linear Model" = "Generalised Linear Model",
              "Linear Mixed Model" = "Linear Mixed Model",
              "Optimization Model" = "Optimization Model"
            ),
            selectize = TRUE
          ),
          shiny::uiOutput(shiny::NS(id, "glm_family_dropdown")),
          shiny::uiOutput(shiny::NS(id, "glm_link_fct_dropdown")),
          shiny::uiOutput(shiny::NS(id, "optim_predefined_equations")),
          shiny::uiOutput(shiny::NS(id, "optim_boundaries_and_method")),
          class = "boxed-output"
        )
      ),
      shiny::column(
        width = 6,
        htmltools::div(
          shiny::uiOutput(shiny::NS(id, "buttons")),
          shiny::actionButton(shiny::NS(id, "create_formula"), "Create statistical model", class = "create_button"),
          class = "boxed-output"
        )
      )
    )
  )
}

# Static modal instead of showModal(): showModal rebuilds the DOM on every
# open, which drops everything the user entered when the modal is closed
FormulaEditorModalUI <- function(id) {
  htmltools::div(
    id = "formula_modal", class = "modal fade", tabindex = "-1", role = "dialog",
    htmltools::div(
      class = "modal-dialog modal-lg", role = "document",
      htmltools::div(
        class = "modal-content",
        htmltools::div(
          class = "modal-header",
          htmltools::h4(
            class = "modal-title",
            htmltools::div(
              style = "display: flex; align-items: center; justify-content: space-between;",
              htmltools::span("FormulaEditor"),
              shiny::actionButton(shiny::NS(id, "formula_docu"), label = NULL, icon = shiny::icon("question-circle"))
            )
          )
        ),
        htmltools::div(class = "modal-body", FormulaEditorUI(id)),
        htmltools::div(
          class = "modal-footer",
          htmltools::tags$button(
            type = "button", class = "btn btn-default",
            `data-dismiss` = "modal", `data-bs-dismiss` = "modal",
            "Close"
          )
        )
      )
    )
  )
}
# nocov end ui-scaffold
