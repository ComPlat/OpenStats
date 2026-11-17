#' Conversion of xml to data.frame
#'
#' @export
#' @param string is a xml file as string
#' @param names is a data.frame which defines the names of the wells. This argument is optional. The default value can be checked using names_default()
#' @param conc is a data.frame which defines the concentrations within the wells. This argument is optional. The default value can be checked using conc_default()
#' @return data.frame containg the imported data with the corresponding names and conc. 
XML2DF <- function(string, names, conc) {
  pg <- xml2::as_xml_document(string)
  test <- xml2::xml_child(pg, search = 4)
  test <- xml2::as_list(test)
  df <- NULL
  for(i in test[[1]]) {
    d <- c()
    if(length(i) == 11) { # required that data has always the same layout!!!
      for(j in 1:11) {
        d <- c(d, i[[j]]$Data[[1]])
      }
      df <- rbind(df, d)
    }
  }
  df <- as.data.frame(df)
  names(df) <- c("conc",
                 vapply(1:10, FUN.VALUE = character(1), function(x) {
                   return(as.character(paste0("V", x)))
                 })
  )
  row.names(df) <- NULL
  df <- df[, -1]
  names <- stack(names)
  conc <- stack(conc)
  df <- stack(df)
  df <- data.frame(abs = df$values, names = names$values, conc = conc$values)
  df <- df[!is.na(df[, 2]), ]
  df
}


reader <- function(path) {
  pg <- xml2::read_xml(path)
  test <- xml2::xml_child(pg, search = 4)
  test <- xml2::as_list(test)
  
  df <- NULL
  for(i in test[[1]]) {
    d <- c()
    if(length(i) == 11) { # required that data has always the same layout!!!
      for(j in 1:11) {
        d <- c(d, i[[j]]$Data[[1]])
      }
      df <- rbind(df, d)
    }
  }
  
  df <- as.data.frame(df)
  
  names(df) <- c("conc",
                 vapply(1:10, FUN.VALUE = character(1), function(x) {
                    return(as.character(paste0("V", x)))
                   })
  )
  
  row.names(df) <- NULL
  df <- df[, -1]

  df
}

#' Returns default design for substances
#'
#' @return The default data.frame used for naming
names_default <- function() {
  n <- data.frame(
    V1 = c("S1", "S2", "S3", "S4", "S5", "S6"),
    V2 = c("S1", "S2", "S3", "S4", "S5", "S6"),
    V3 = c("S1", "S2", "S3", "S4", "S5", "S6"),
    V4 = c("S1", "S2", "S3", "S4", "S5", "S6"),
    V5 = c("S1", "S2", "S3", "S4", "S5", "S6"),
    V6 = c("S1", "S2", "S3", "S4", "S5", "S6"),
    V7 = c("S1", "S2", "S3", "S4", "S5", "S6"),
    V8 = c("S1", "S2", "S3", "S4", "S5", "S6"),
    V9 = c("S1", "S2", "S3", "S4", "S5", "S6"),
    V10 = c("neg", "neg", "neg", "pos", "pos", "pos")
  )
  n
}

#' Returns default design for concentrations
#'
#' @return The default data.frame used for concentration
conc_default <- function() {
  c <- data.frame(
    V1 = rep(50, 6),
    V2 = rep(50, 6),
    V3 = rep(50, 6),
    V4 = rep(5, 6),
    V5 = rep(5, 6),
    V6 = rep(5, 6),
    V7 = rep(0.5, 6),
    V8 = rep(0.5, 6),
    V9 = rep(0.5, 6),
    V10 = c("neg", "neg", "neg", "pos", "pos", "pos")
  )
  c
}


#' Import data
#'
#' @export
#' @param path defines the path to the file which should be imported
#' @param names is a data.frame which defines the names of the wells. This argument is optional. The default value can be checked using names_default()
#' @param conc is a data.frame which defines the concentrations within the wells. This argument is optional. The default value can be checked using conc_default()
#' @return data.frame containg the imported data with the corresponding names and conc. 
#' @examples
#' df <- import("./data/testfile1.xml")
#' df
import <- function(path, names = names_default(), conc = conc_default()) {
  df <- reader(path)
  names <- stack(names)
  conc <- stack(conc)
  df <- stack(df)
  
  df <- data.frame(abs = df$values, names = names$values, conc = conc$values)
 
  df <- df[!is.na(df[, 2]), ]
  
  df
}



#' remove outliers
#'
#' @export
#' @param df a data.frame from which the outliers should be removed
#' @param l a list containing column indices which defines the different groups
#' @param dep the column index which defines the dependent variable
removeOutlier <- function(df, l, dep) {
  ps <- function(a, b) {
    paste0(a, "_", b)
  }
  int <- Reduce(ps, as.list(df[, l]))
  intUnique <- unique(int)
  
  dfor <- lapply(intUnique, function(x) {
   temp <- df[int == x, ]
   bs <- boxplot.stats(temp$abs)
   if (length(bs$out) == 0) return(temp)
   temp[temp$abs %in% bs$out, "abs"] <- NA
   return(temp)
  })
  df <- do.call(rbind, dfor)
  df[!is.na(df$abs), ]
}
