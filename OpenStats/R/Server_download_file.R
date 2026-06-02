downloadServer <- function(id, DataModelState, ResultsState, MethodState) {
  shiny::moduleServer(id, function(input, output, session) {
    download_file <- shiny::reactive({
      out_dir <- file.path(tempdir(), "openstats-results")
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

      if (!requireNamespace("COMELN", quietly = TRUE)) {
        shiny::validate(shiny::need(FALSE,
          paste(
            "Feature requires the 'COMELN' package.",
            "Install it to enable downloads.",
            'For GitHub: remotes::install_github("ComPlat/OpenStats", subdir = "comeln")'
          )
        ))
        return(NULL)
      }
      downloader <- getExportedValue("COMELN", "download")

      file <- downloader(session, out_dir) #"/home/shiny/results"

      if (MethodState$method == "Default") {
        env_import$read_data(file, DataModelState, ResultsState)
      } else if (MethodState$method == "DoseResponse") {
        MethodState$storage_class <- new("MethodDoseResponse", id = "", request_id = "", element_info = list())
        env_import_dose_response$import_dose_response_json(file, DataModelState, ResultsState, MethodState)
      } else if (MethodState$method == "VariationStatistics") {
        env_import_export_variations_V1_2$read_variations(file, DataModelState, ResultsState)
      }
      print_req(
        is.data.frame(DataModelState$df),
        "File can not be used. Upload into R failed!"
      )
      tryCatch(
        {
          unlink(file)
        },
        warning = function(warn) {
          print_warn(paste("A warning occurred: ", conditionMessage(warn)))
        },
        error = function(err) {
          print_err(paste("An error occurred: ", conditionMessage(err)))
        }
      )
      shiny::req(is.data.frame(DataModelState$df))
    })

    get_method <- shiny::reactive({
      if (!requireNamespace("COMELN", quietly = TRUE)) {
        shiny::validate(shiny::need(FALSE,
          paste(
            "Feature requires the 'COMELN' package.",
            "Install it to enable downloads.",
            'For GitHub: remotes::install_github("ComPlat/OpenStats", subdir = "comeln")'
          )
        ))
        return(NULL)
      }
      get_method <- getExportedValue("COMELN", "getMethod")
      m <- get_method()
      if (m == "DoseResponse") {
        MethodState$method <- "DoseResponse"
        MethodState$storage_class <- new(
          "MethodDoseResponse",
          id = "", request_id = "", element_info = list()
        )
      } else if (m == "VariationStatistics") {
        MethodState$method <- "VariationStatistics"
      }
    })

    if (Sys.getenv("RUN_MODE") == "SERVER") {
      shiny::observe({
        shiny::req(is.null(DataModelState$df))
        e <- try(get_method(), silent = TRUE)
        if (inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
          print_err(err)
        }
        e <- try(download_file(), silent = TRUE)
        if (inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
          print_err(err)
        }
      })
    } else {
      shiny::observeEvent(input$file, {
        e <- try(env_import$read_data(input$file$datapath, DataModelState, ResultsState))
        if (inherits(e, "try-error")) {
          err <- conditionMessage(attr(e, "condition"))
          print_err(err)
        }
      })
    }

  })
}
