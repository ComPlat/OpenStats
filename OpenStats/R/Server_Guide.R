GuideServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$path, {
      shiny::req(input$path %in% names(click_paths_V1_2))
      session$sendCustomMessage("highlight_path", list(steps = click_paths_V1_2[[input$path]]))
      # Back to the placeholder so the same workflow can be started again
      shiny::updateSelectInput(session, "path", selected = "")
    })
  })
}
