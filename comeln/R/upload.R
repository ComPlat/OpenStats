upload <- function(session, filepath, new_name) {
  ipaddress <- getURL(session)
  p <- session$clientData
  if(is.null(p)) {
    shiny::showNotification("Slot clientData is not found. Is it a session object?", duration = 0)
    Sys.sleep(30)
  }
  p <- tc(shiny::reactiveValuesToList(p), "Could not apply as.list to input")
  p <- p$url_search
  if(is.null(p)) {
    shiny::showNotification("Slot url_search is not found. Is it a session object?", duration = 0)
    Sys.sleep(30)
  } else if(p == "") {
    shiny::showNotification("Slot url_search is empty. Cannot download the file?", duration = 0)
    Sys.sleep(30)
  }

  query <- getQueryString()
  url <- paste0(ipaddress)

  url <- sub("0.0.0.0", "172.17.0.1", url) # Only for testing when running on local host
  ext <- tools::file_ext(filepath)
  mime <- if (tolower(ext) == "zip") "application/zip" else NULL

  file_extension <- tools::file_ext(filepath)
  request <- POST(
    url,
    encode = "multipart",
    body = list(
      file = httr::upload_file(filepath, type = mime),
      attachmentName = new_name,
      fileType = ext
    )
  )
  txt <- httr::content(request, as = "text", encoding = "UTF-8")

  if (httr::http_error(request)) {
    shiny::showNotification(
      paste0("Upload failed (", httr::status_code(request), "): ", txt),
      duration = 0
    )
    return(invisible(FALSE))
  }

  shiny::showNotification(
    paste0("Upload OK (", httr::status_code(request), "): ", txt),
    duration = 5
  )
  invisible(TRUE)
}
