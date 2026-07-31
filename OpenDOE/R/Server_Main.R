app <- function() {
  ui <- main_ui()

  server <- function(input, output, session) {

    State <- shiny::reactiveValues(
      predictors = list(),
      predictor_types = character(0),
      n_per_level = NULL,
      mc_running = FALSE,
      results = list(),
      counter = 0L,
      history = list(),
      finite_assign_running = FALSE,
      replay_queue = list(),
      history_replay_running = FALSE
    )
    bgp <- bg_process$new()
    State$bgp <- bgp
    bgp$init()

    dataServer("DATA", State)
    predictorsServer("PREDICTORS", State)
    sampleSizeServer("SAMPLESIZE", State)
    designServer("DESIGN", State)
    randomAssignServer("RANDOMASSIGN", State)
    finiteAssignServer("FINITEASSIGN", State)
    resultsListServer("RESULTS", State)
    historyServer("HISTORY", State)

    output$diagram <- shiny::renderUI({
      steps <- step_defs()
      display_labels <- step_display_labels()
      explanations <- step_explanations()
      current <- input$conditionedPanels

      boxes <- lapply(seq_along(steps), function(i) {
        step <- steps[[i]]
        unlocked <- step_unlocked(State, step)
        cls <- "doe-diagram-box"
        if (identical(current, step)) cls <- paste(cls, "doe-diagram-box-active")
        box <- if (unlocked) {
          shiny::actionButton(
            paste0("diagram_open_", i), label = display_labels[[step]],
            class = cls, onmouseenter = "doeShowDoc(this)",
            `data-doc` = explanations[[step]]
          )
        } else {
          htmltools::div(
            display_labels[[step]], htmltools::tags$small(" \U0001F512"),
            class = paste(cls, "doe-diagram-box-locked"),
            onmouseenter = "doeShowDoc(this)", `data-doc` = explanations[[step]]
          )
        }
        if (i == length(steps)) return(box)
        htmltools::tagList(box, htmltools::span(shiny::HTML("&rArr;"), class = "doe-arrow"))
      })
      htmltools::div(class = "doe-diagram", boxes)
    })

    lapply(seq_along(step_defs()), function(i) {
      steps <- step_defs()
      shiny::observeEvent(input[[paste0("diagram_open_", i)]], {
        step <- steps[[i]]
        if (step_unlocked(State, step)) {
          shiny::updateTabsetPanel(session, "conditionedPanels", selected = step)
        }
      }, ignoreInit = TRUE)
    })
    shiny::observeEvent(input$goto_data, {
      shiny::updateTabsetPanel(session, "conditionedPanels", selected = "Data")
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$goto_history, {
      shiny::updateTabsetPanel(session, "conditionedPanels", selected = "History")
    }, ignoreInit = TRUE)

  }

  list(ui = ui, server = server)
}
