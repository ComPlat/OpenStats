download <- function(session, folder) {
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
  } else if(p[[1]] == "") {
    shiny::showNotification("Slot url_search is empty. Cannot download the file?", duration = 0)
    Sys.sleep(30)
  }

  query <- getQueryString()
  url <- paste0(ipaddress)

  url <- sub("0.0.0.0", "172.17.0.1", url) # Only for testing when running on local host

  res <- tryCatch(
    {
      httr::GET(url)
    },
    error = function(e) {
      shiny::showNotification(paste("Test GET failed:", e$message), duration = 0)
      NULL
    }
  )

  response <- GET(url)

  if(status_code(response)[[1]] != 200) {
    shiny::showNotification("File could not be downloaded from ELN", duration = 0)
    Sys.sleep(30)
  }
  content <- content(response, as = "text")
  tempfile <- tempfile(tmpdir = folder)

  fileConn<-file(tempfile)
  writeLines(content, fileConn)
  close(fileConn)

  return(tempfile)
}
