randomAssignSidebarUI <- function(id) {
  shiny::uiOutput("RANDOMASSIGN-assign_controls")
}

randomAssignMainUI <- function(id) {
  shiny::tagList(
    shiny::h4("Dataset (preview)"),
    shiny::uiOutput("RANDOMASSIGN-df_box")
  )
}
