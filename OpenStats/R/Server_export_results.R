ExportResultsServer <- function(id, DataModelState, ResultsState, MethodState) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$download, {

      e <- try({

        if (MethodState$method == "Default") {
          if (!env_utils$is_valid_filename(input$user_filename)) {
            shinyjs::runjs("document.getElementById('user_filename').focus();")
            print_noti(
              env_utils$why_filename_invalid(input$user_filename)
            )
          }
          print_req(
            env_utils$is_valid_filename(input$user_filename),
            "Defined filename is not valid"
          )
        }

        print_req(length(ResultsState$all_data) > 0, "No results to save")
        l <- ResultsState$all_data
        history_json <- jsonlite::toJSON(ResultsState$history, pretty = TRUE, auto_unbox = TRUE)
        history_table <- history_to_table(ResultsState$history)
        l_history <- c("HistoryTable" = history_table)
        l <- c(l_history, l)
        l <- c(l, "HistoryJSON" = history_json)
        versions <- env_utils_V1_2$get_packages_w_versions()
        l <- c(l, list(Versions = versions))

        # Sent data to ChemotionELN
        # -----------------------------------------------------------------------
        if (Sys.getenv("RUN_MODE") == "SERVER") {
          if (!requireNamespace("COMELN", quietly = TRUE)) {
            shiny::validate(shiny::need(FALSE,
              paste(
                "Feature requires the 'COMELN' package.",
                "Install it to enable uploads",
                'For GitHub: remotes::install_github("ComPlat/OpenStats", subdir = "comeln")'
              )
            ))
            return(NULL)
          }
          uploader <- getExportedValue("COMELN", "upload")

          if (MethodState$method == "Default") {
            print_req(
              env_utils$check_filename_for_server(input$user_filename),
              "Defined filename does not have xlsx as extension"
            )
            excelFile <- env_utils$create_excel_file(l)
            uploader(session, excelFile, new_name = input$user_filename)
            unlink(excelFile)
          }
          else if (MethodState$method == "DoseResponse") {
            jsonFile <- try(env_import_dose_response$dose_response_to_json(MethodState, ResultsState$all_data))
            excelFile <- try(env_utils$create_excel_file(l))
            if (!is.character(jsonFile) || length(jsonFile) != 1L || !file.exists(jsonFile)) {
              print_err("Cannot convert results to json")
            }
            if (!is.character(excelFile) || length(excelFile) != 1L || !file.exists(excelFile)) {
              print_err("Cannot store the results in an excelFile")
            }
            fn <- tempfile(fileext = ".zip")
            utils::zip(fn, c(jsonFile, excelFile))
            if (!file.exists(fn) || file.info(fn)$size <= 0) {
              print_err("Could not create the zip archive storing the final results")
            }
            uploader(session, fn, new_name = "result.zip")
            unlink(fn)
            unlink(excelFile)
            unlink(jsonFile)
          }
          else if (MethodState$method == "VariationStatistics") {
           jsonFile <- try(env_import_export_variations_V1_2$summary_data_frame_to_json(MethodState, ResultsState$all_data), silent = TRUE)
            excelFile <- try(env_utils$create_excel_file(l))
            if (!is.character(jsonFile) || length(jsonFile) != 1L || !file.exists(jsonFile)) {
              print_err("Cannot convert results to json")
            }
            if (!is.character(excelFile) || length(excelFile) != 1L || !file.exists(excelFile)) {
              print_err("Cannot store the results in an excelFile")
            }
            fn <- tempfile(fileext = ".zip")
            utils::zip(fn, c(jsonFile, excelFile))
            if (!file.exists(fn) || file.info(fn)$size <= 0) {
              print_err("Could not create the zip archive storing the final results")
            }
            uploader(session, fn, new_name = "result.zip")
            unlink(fn)
            unlink(excelFile)
            unlink(jsonFile)
          }
        }

        # Running OpenStats locally
        # -----------------------------------------------------------------------
        else if (Sys.getenv("RUN_MODE") == "LOCAL") {
          print_req(
            env_utils$check_filename_for_server(input$user_filename) || env_utils$check_filename_for_serverless(input$user_filename),
            "Defined filename does not have xlsx or zip as extension"
          )
          ex <- env_utils$extract_extension(input$user_filename)
          if (ex == "xlsx") {
            excelFile <- env_utils$create_excel_file(l)
            file_content <- readBin(excelFile, "raw", file.info(excelFile)$size)
            file_content_base64 <- jsonlite::base64_enc(file_content)
            session$sendCustomMessage(
              type = "downloadExcel",
              list(
                fileContent = file_content_base64,
                filename = input$user_filename
              )
            )
            unlink(excelFile)
          } else {
            string_and_names <- env_utils$create_js_string(l)
            session$sendCustomMessage(
              type = "downloadZip",
              list(
                numberOfResults = length(string_and_names[[1]]),
                FileContent = string_and_names[[1]],
                Filename = input$user_filename,
                ResultNames = string_and_names[[2]]
              )
            )
          }

        } else {
          print_req(
            env_utils$check_filename_for_serverless(input$user_filename),
            "Defined filename does not have zip as extension"
          )
          string_and_names <- env_utils$create_js_string(l)
          session$sendCustomMessage(
            type = "downloadZip",
            list(
              numberOfResults = length(string_and_names[[1]]),
              FileContent = string_and_names[[1]],
              Filename = input$user_filename,
              ResultNames = string_and_names[[2]]
            )
          )
        }

      }, silent = TRUE)

      if (inherits(e, "try-error")) {
        print_err(e)
      }

    })
  })
}
