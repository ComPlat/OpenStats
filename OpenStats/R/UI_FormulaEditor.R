# nocov start ui-scaffold
FormulaEditorUI <- function(id) {
  ui <- fluidPage(
    shiny::fluidRow(
      uiOutput(NS(id, "model")),
      uiOutput(NS(id, "predefined_modelsUI")),
      shiny::column(
        width = 6,
        uiOutput(NS(id, "colnames_dropdown")),
        htmltools::div(
          selectInput(NS(id, "model_type"), "Change the model type (optional)",
            c(
              "Linear" = "Linear",
              "Generalised Linear Model" = "Generalised Linear Model",
              "Optimization Model" = "Optimization Model"
            ),
            selectize = FALSE
          ),
          uiOutput(NS(id, "glm_family_dropdown")),
          uiOutput(NS(id, "glm_link_fct_dropdown")),
          uiOutput(NS(id, "optim_predefined_equations")),
          uiOutput(NS(id, "optim_boundaries_and_method")),
          class = "boxed-output"
        )
      ),
      shiny::column(
        width = 6,
        htmltools::div(
          uiOutput(NS(id, "colnames_list")),
          uiOutput(NS(id, "buttons")),
          uiOutput(NS(id, "rhs")),
          shiny::actionButton(NS(id, "create_formula"), "Create statistical model", class = "create_button"),
          class = "boxed-output"
        )
      )
    )
  )
}
# nocov end ui-scaffold
