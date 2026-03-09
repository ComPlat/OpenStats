tc <- function(expr, error_message) {
  ret <- NULL
  tryCatch(
    expr = {
      ret <- eval(expr)
    },
    error = function(e) {
      shiny::showNotification(error_message, duration = 0)
      Sys.sleep(30)
    }
  )
  return(ret)
}