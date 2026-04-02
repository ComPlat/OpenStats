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
              "Optimization Model" = "Optimization Model"
            ),
            selectize = FALSE
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
          shiny::uiOutput(shiny::NS(id, "colnames_list")),
          shiny::uiOutput(shiny::NS(id, "buttons")),
          shiny::uiOutput(shiny::NS(id, "rhs")),
          shiny::actionButton(shiny::NS(id, "create_formula"), "Create statistical model", class = "create_button"),
          class = "boxed-output"
        )
      )
    )
  )
}
# nocov end ui-scaffold
