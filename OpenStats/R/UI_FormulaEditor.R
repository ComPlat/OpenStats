# nocov start ui-scaffold
FormulaEditorUI <- function(id) {
  ui <- fluidPage(
    fluidRow(
      uiOutput(NS(id, "model")),
      uiOutput(NS(id, "predefined_modelsUI")),
      column(
        width = 6,
        uiOutput(NS(id, "colnames_dropdown")),
        div(
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
          uiOutput(NS(id, "optim_boundaries")),
          class = "boxed-output"
        )
      ),
      column(
        width = 6,
        div(
          uiOutput(NS(id, "colnames_list")),
          uiOutput(NS(id, "buttons")),
          uiOutput(NS(id, "rhs")),
          actionButton(NS(id, "create_formula"), "Create statistical model", class = "create_button"),
          class = "boxed-output"
        )
      )
    )
  )
}
# nocov end ui-scaffold
