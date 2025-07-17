df <- read.csv("./test_data/DoseResponse.csv")
df <- df[df$names == "S1", ]
outliers <- list(S1 = 10)
str(outliers)
OpenStats:::ic50(df, "abs", "conc", "names", outliers, FALSE, FALSE)

create_outlier_info <- function(l) {
  if (is.null(l)) return("")
  res <- sapply(
    seq_len(length(l)), function(idx) {
      n <- names(l)[idx]
      points <- paste0(l[[idx]], collapse = ", ")
      paste0(n, ": ", points)
    }
  )
  res
}
create_outlier_info(list(S1 = c(1, 2)))

parse_outlier_history <- function(history_outliers) {
  splitted_history <- strsplit(history_outliers, ":")[[1]]
  name <- splitted_history[[1]]
  indices <- splitted_history[-1]
  indices <- strsplit(indices, ",")[[1]]
  indices <- as.numeric(indices)
  l <- list(indices)
  names(l) <- name
  l
}
parse_outlier_history("S1: 1, 20")
