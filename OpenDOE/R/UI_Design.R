designSidebarUI <- function(id) {
  shiny::tagList(
    shiny::h4("Design of experiment"),
    shiny::uiOutput("DESIGN-design_controls")
  )
}

designMainUI <- function(id) {
  shiny::tags$p("Configure inputs in the sidebar.")
}
