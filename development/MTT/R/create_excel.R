#' Saves data
#' @export
#' @import openxlsx
#' @param ic_50List the list result of a call to ic50.
#' @param file_name the name of the excel file where the results should be saved.
#' @examples
#' path <- system.file("data", package = "MTT")
#' df <- read.csv(paste0(path, "/ExampleData.txt"))
#' res <- ic50(df, "abs", "conc", "names", "neg", "pos")
#' save(res, "results.xlsx")
save <- function(ic_50List, file_name) {

  f1 <- function(ic_50List) {
    pl <- lapply(ic_50List, function(x) {
      if (is(x, "errorClass")) {
        return(NULL)
      } else {
        return(x[[2]])
      }
    })
    pl <- Filter(Negate(is.null), pl)
    start_row <- 1
    for(i in seq_along(pl)) {
      if(is.null(pl[[i]])) {
        next
      }
      print(pl[i])
      insertPlot(wb, 1, "Plots", xy = c("A", start_row), width = 16,
                 height = 10, fileType = "png", units = "cm")
      start_row <- start_row + 25
    }
  }

  f2 <- function(ic_50List) {
    ar <- lapply(ic_50List, function(x) {
      if (is(x, "errorClass")) {
        return(NULL)
      } else {
        return(x[[1]])
      }
    })
    ar <- Filter(Negate(is.null), ar)
    start_row <- 1
    for(i in seq_along(ar)) {
      if(is.null(ar[[i]])) {
        next
      }
      writeData(wb, "ic50fit",
                ar[[i]],
                startCol = "A",
                startRow = start_row + 1, colNames = TRUE, rowNames = TRUE)
      start_row <- start_row + 10
    }
  }

  wb <- createWorkbook()
  addWorksheet(wb, "Plots")
  addWorksheet(wb, "ic50fit")
  f1(ic_50List)
  f2(ic_50List)
  saveWorkbook(wb, file_name, overwrite = TRUE)
}






