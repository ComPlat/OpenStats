dataSidebarUI <- function(id) {
  shiny::tagList(
    shiny::h4("Import data"),
    shiny::fileInput("DATA-upload", "Upload CSV", accept = ".csv")
  )
}

dataMainUI <- function(id) {
  shiny::tags$p("Uploaded files appear in the Results list below and can be picked wherever a dataset is needed.")
}
