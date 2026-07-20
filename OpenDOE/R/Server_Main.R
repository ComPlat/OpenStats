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

  }

  list(ui = ui, server = server)
}
