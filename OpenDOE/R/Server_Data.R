dataServer <- function(id, State) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$upload, {
      file <- input$upload
      df <- try(utils::read.csv(file$datapath, stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE)
      if (inherits(df, "try-error")) {
        print_err("Could not read CSV file")
        return(invisible())
      }
      if (nrow(df) == 0L) {
        print_err("Uploaded file has no rows")
        return(invisible())
      }
      add_result(State, "import_csv", file$name, list(), methods::new("importedData", df = df))
    })
  })
}
