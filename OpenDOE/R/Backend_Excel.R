create_excel_file <- function(l) {
  if (length(l) == 0) {
    print_warn("Nothing to save")
    return(NULL)
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Results")

  curr_row <- 1
  line_style <- openxlsx::createStyle(border = "bottom", borderStyle = "thick")

  for (i in seq_along(l)) {
    openxlsx::writeData(wb, "Results", names(l)[i], startRow = curr_row)
    curr_row <- curr_row + 2

    value <- l[[i]]
    if (inherits(value, setdiff(df_result_classes, "finiteAssignmentResult"))) {
      openxlsx::writeData(wb, "Results", value@df, startRow = curr_row)
      curr_row <- curr_row + nrow(value@df) + 2
    } else if (inherits(value, "finiteAssignmentResult")) {
      openxlsx::writeData(wb, "Results", paste("Loss:", value@loss), startRow = curr_row)
      curr_row <- curr_row + 2
      openxlsx::writeData(wb, "Results", value@df, startRow = curr_row)
      curr_row <- curr_row + nrow(value@df) + 2
    } else if (inherits(value, "sampleSizeResult")) {
      openxlsx::writeData(wb, "Results", value@n, startRow = curr_row)
      curr_row <- curr_row + 2
    } else if (is.character(value)) {
      openxlsx::writeData(wb, "Results", value, startRow = curr_row)
      curr_row <- curr_row + length(value) + 2
    }

    openxlsx::addStyle(
      wb, sheet = "Results", style = line_style, rows = curr_row,
      cols = 1:20, gridExpand = TRUE
    )
    curr_row <- curr_row + 3
  }

  fn <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, fn)
  fn
}
