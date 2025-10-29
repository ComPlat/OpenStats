upload <- function(session, filepath, new_name) {
  ipaddress <- getURL(session)
  p <- session$clientData
  if(is.null(p)) {
    showNotification("Slot clientData is not found. Is it a session object?", duration = 0)
    Sys.sleep(30)
  }
  p <- tc(reactiveValuesToList(p), "Could not apply as.list to input")
  p <- p$url_search
  if(is.null(p)) {
    showNotification("Slot url_search is not found. Is it a session object?", duration = 0)
    Sys.sleep(30)
  } else if(p == "") {
    showNotification("Slot url_search is empty. Cannot download the file?", duration = 0)
    Sys.sleep(30)
  }

  query <- getQueryString()
  url <- paste0(ipaddress)

  # url <- sub("0.0.0.0", "172.17.0.1", url) # TODO: remove; Only for testing when running on local host

  file_extension <- tools::file_ext(filepath)
  request <- POST(
    url,
    body = list(file = upload_file(filepath), attachmentName = new_name,
                  fileType = file_extension) )
  response <- content(request)
  showNotification(response, duration = 0)
}
