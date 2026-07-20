finiteAssignSidebarUI <- function(id) {
  shiny::uiOutput("FINITEASSIGN-assign_controls")
}

finiteAssignMainUI <- function(id) {
  shiny::tagList(
    shiny::tags$p("Configure inputs in the sidebar."),
    shiny::uiOutput("FINITEASSIGN-status_box")
  )
}
