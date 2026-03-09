getURL <- function(session) {
  p <- session$clientData
  if (is.null(p)) {
    shiny::showNotification("Slot clientData is not found. Is it a session object?", duration = 0)
    Sys.sleep(30)
  }
  p <- tc(shiny::reactiveValuesToList(p), "Could not apply as.list to input")
  p <- p$url_search
  if (is.null(p)) {
    shiny::showNotification("Slot url_search is not found. Is it a session object?", duration = 0)
    Sys.sleep(30)
  } else if (p[[1]] == "") {
    shiny::showNotification("Slot url_search is empty. Cannot download the file?", duration = 0)
    Sys.sleep(30)
  }

  query <- getQueryString()
  url <- paste0(query$url)
  return(url)
}

getMethod <- function(session) {
  query <- getQueryString()
  m <- query[["method"]]
  if (is.null(m) || m == "") {
    return("Default")
  } else {
    return(m)
  }
}
