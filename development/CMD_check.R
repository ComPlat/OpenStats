makeActiveBinding("Index", function() stop("Should never be used"), parent.frame())

f <- function() {
  return(Index)
}
f()
