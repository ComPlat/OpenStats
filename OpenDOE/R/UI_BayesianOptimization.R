boSidebarUI <- function(id) {
  shiny::tagList(
    shiny::uiOutput("BO-bo_controls"),
    shiny::uiOutput("BO-type_controls"),
    shiny::uiOutput("BO-run_controls")
  )
}

boMainUI <- function(id) {
  shiny::tagList(
    shiny::tags$p(
      "Suggests the next experiment based on all measured experiments so far.
       The suggestion is appended as a new row with an empty objective.
       Rows with an empty objective are treated as planned and ignored."
    ),
    shiny::tags$p(
      "Bounds of continuous predictors are taken from the min/max of the dataset,
       so the search never leaves the range measured so far."
    ),
    shiny::uiOutput("BO-status_box")
  )
}
