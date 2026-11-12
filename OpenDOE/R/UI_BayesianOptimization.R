boSidebarUI <- function(id) {
  shiny::tagList(
    shiny::uiOutput("BO-bo_controls"),
    shiny::uiOutput("BO-type_controls"),
    shiny::uiOutput("BO-objective_controls"),
    shiny::uiOutput("BO-direction_controls"),
    shiny::uiOutput("BO-run_controls")
  )
}

boMainUI <- function(id) {
  shiny::tagList(
    shiny::tags$p(
      "Suggests the next experiment based on all measured experiments so far.
       The suggestion is appended as a new row with an empty measured result.
       Rows with an empty measured result are treated as planned experiments:
       they are not used as measurements, but the next suggestions avoid them."
    ),
    shiny::tags$p(
      "Bounds of continuous predictors are taken from the min/max of the dataset,
       so the search never leaves the range measured so far.
       For variations from the ELN the bounds of the design definition are used instead."
    ),
    shiny::tags$p(
      "With several measured results there is usually no single best experiment.
       The column pareto_optimal marks the measured experiments which are not beaten
       by any other experiment in all results at once (the best trade-offs)."
    ),
    shiny::uiOutput("BO-status_box")
  )
}
