# FIX: the names abs, conc, names are mandatory fix this
errorClass <- R6::R6Class("errorClass",
  public = list(
    error_message = NULL,
    object = NULL,
    initialize = function(error_message = NULL) {
      self$error_message <- error_message
    },
    isNull = function() {
      if (is.null(self$error_message)) {
        return(TRUE)
      }
      return(FALSE)
    }
  )
)

shapenumber <- function(my.number) {
  if (is.finite(my.number)) {
    my.result <- signif(my.number, 3)
  } else {
    my.result <- NA
  }
  return(my.result)
}

# calculates the robust 68th percentile of the residuals
# adapted from Motulsky HJ, Brown RE, BMC Bioinformatics 2006, 7:123
robust_68_percentile <- function(residuals) {
  res <- abs(residuals)
  res_sorted <- sort(res)
  res_percentiles <- (seq(1:length(res_sorted)) / length(res_sorted)) * 100
  index <- min(which(res_percentiles > 68.25))
  x <- c(res_percentiles[index - 1], res_percentiles[index])
  y <- c(res_sorted[index - 1], res_sorted[index])
  m <- lm(y ~ x)
  x <- c(68.25)
  y <- predict(m, as.data.frame(x))
  return(y)
}

# calculates the robust standard deviation of the residuals (RSDR) with correction for degrees of freedom
# adapted from Motulsky HJ, Brown RE, BMC Bioinformatics 2006, 7:123
robust_standard_deviation_residuals <- function(residuals, number_of_coefficients_fitted) {
  my_residuals <- as.numeric(residuals)
  my_residuals <- na.omit(residuals)
  N <- length(my_residuals) # the number of data points fitted
  K <- number_of_coefficients_fitted # for ic50, 4 coefficients are fitted
  result <- robust_68_percentile(residuals) * N / (N - K)
  return(result)
}

# false discovery rate (FDR) approach, returns a T/F vector for selection of valid data points
# adapted from Motulsky HJ, Brown RE, BMC Bioinformatics 2006, 7:123
false_discovery_rate <- function(res) {
  N <- length(res)
  Q <- 0.01 # Q=1%
  K <- 4 # number of coefficients in the fitted LL.4 model
  R <- robust_standard_deviation_residuals(res, K) # the robust standard deviation of the residuals
  id <- seq(1:length(res))
  df <- data.frame(id, res)
  df$res_abs <- abs(df$res)
  df <- df[order(df$res_abs), ]
  df$i <- seq(1:N)
  df$i_fraction <- df$i / N
  df$alpha <- Q * (N - (df$i - 1)) / N
  df$t <- df$res_abs / R
  df$P <- dt(df$t, N - K)
  df$include <- ifelse(df$P < df$alpha & df$i_fraction >= 0.7, FALSE, TRUE)
  df2 <- df[order(df$id), ]
  return(df2$include)
}

check_fit <- function(model, min_conc, max_conc, min_abs, max_abs, substance_name) {
  if (model$fit$convergence != TRUE) {
    return(errorClass$new(paste(
      substance_name,
      "Model did not converge"
    )))
  }
  b <- coefficients(model)[1] # Hill coefficient
  c <- coefficients(model)[2] # asymptote 1
  d <- coefficients(model)[3] # asymptote 2
  e <- coefficients(model)[4] # IC50
  RSE <- summary(model)$rseMat[1] # residual standard error estimated
  Response_lowestdose_predicted <- predict(model, data.frame(concentration = min_conc), se.fit = FALSE)[1]
  Response_highestdose_predicted <- predict(model, data.frame(concentration = max_conc), se.fit = FALSE)[1]
  Response_difference <- 100 * abs(Response_lowestdose_predicted - Response_highestdose_predicted)
  HillCoefficient <- b
  IC50_relative <- e
  pIC50 <- -log10(e / 1000000)
  Problems <- ""
  if (Response_difference < 25) {
    Problems <- paste(Problems, "Response Difference lower than 25%", collapse = " , ")
  } else if (IC50_relative > max_conc) {
    Problems <- paste(Problems, "IC50 larger than highest measured concentration", collapse = " , ")
  } else if (IC50_relative < min_conc) {
    Problems <- paste(Problems, "IC50 lower than lowest measured concentration", collapse = " , ")
  }

  confidence_interval <- confint(model, parm = c("e"), level = 0.95)
  IC50_relative_lower <- confidence_interval[1]
  IC50_relative_higher <- confidence_interval[2]
  p_value <- noEffect(model)[3]
  Response_lowestdose_predicted <- shapenumber(Response_lowestdose_predicted)
  Response_highestdose_predicted <- shapenumber(Response_highestdose_predicted)
  HillCoefficient <- shapenumber(HillCoefficient)
  IC50_relative <- shapenumber(IC50_relative)
  IC50_relative_lower <- shapenumber(IC50_relative_lower)
  IC50_relative_higher <- shapenumber(IC50_relative_higher)
  pIC50 <- shapenumber(-log10(IC50_relative / 1000000))
  p_value <- shapenumber(p_value)
  ylim_low <- 0
  ylim_high <- 125
  if (min_abs < ylim_low) ylim_low <- min_abs
  if (max_abs > ylim_high) ylim_high <- max_abs
  outvar <- data.frame(
    name = substance_name,
    Response_lowestdose_predicted = Response_lowestdose_predicted,
    Response_highestdose_predicted = Response_highestdose_predicted,
    HillCoefficient = HillCoefficient,
    asymptote_one = c, asymptote_two = d,
    IC50_relative = IC50_relative, IC50_relative_lower = IC50_relative_lower,
    IC50_relative_higher = IC50_relative_higher, pIC50 = pIC50,
    RSE = RSE, p_value = p_value, Problems = Problems
  )
  return(outvar)
}

drawplot <- function(df, abs_col, conc_col, model, valid_points, title,
                     IC50_relative, IC50_relative_lower, IC50_relative_higher) {
  min_conc <- min(df[, conc_col])
  max_conc <- max(df[, conc_col])
  grid <- seq(min_conc, max_conc, 0.1)
  plotFct <- (model$curve)[[1]]
  res <- plotFct(grid)
  data <- data.frame(
    abs = res,
    conc = grid
  )
  data_measured <- data.frame(conc = df[, conc_col], abs = df[, abs_col])
  p <- ggplot() +
    geom_boxplot(data = data_measured, aes(x = conc, y = abs * 100, group = conc)) +
    geom_line(data = data, aes(x = conc, y = abs * 100)) +
    xlab("Concentration [µM]") +
    ylab("Viability [%]") +
    ggtitle(title)

  max_conc <- max(df[, conc_col]) + 10
  min_conc <- -10
  xmin <- IC50_relative - IC50_relative_lower
  xmax <- IC50_relative + IC50_relative_higher
  if (!is.na(xmin) & !is.na(xmax)) {
    ymin <- min(df[, abs_col]) * 100
    ymax <- max(df[, abs_col]) * 100
    yrange <- ymax - ymin
    butt_height <- yrange * 0.1
    ymedian <- median(df[, abs_col]) * 100
    if (xmin > min_conc && xmax < max_conc) {
      p <- p + geom_errorbarh(
        aes(
          xmin = xmin,
          xmax = xmax, y = ymedian
        ),
        colour = "darkred", end = "butt", height = butt_height
      )
    } else {
      p <- p + labs(caption = "Confidence intervall not in conc. range") +
        theme(plot.caption = element_text(color = "darkred", face = "italic", size = 7))
    }
  } else {
    p <- p + labs(caption = "Confidence intervall could not be calculated") +
      theme(plot.caption = element_text(color = "darkred", face = "italic", size = 7))
  }

  return(p)
}

ic50_internal <- function(df, abs, conc, title) {
  model <- drm(abs ~ conc, data = df, fct = LL.4(), robust = "median")
  valid_points <- false_discovery_rate(residuals(model))
  model <- drm(abs ~ conc, data = df, subset = valid_points, start = model$coefficients, fct = LL.4(), robust = "mean")
  res <- check_fit(model, min(df[, conc]), max(df[, conc]), min(df[, abs]), max(df[, abs]), title)
  p <- drawplot(
    df, abs, conc, model, valid_points, title, res$IC50_relative,
    res$IC50_relative_lower, res$IC50_relative_higher
  )
  return(list(res, p))
}

drawplotOnlyRawData <- function(df, abs_col, conc_col, title) {
  min_conc <- min(df[, conc_col])
  max_conc <- max(df[, conc_col])
  data_measured <- data.frame(conc = df[, conc_col], abs = df[, abs_col])
  p <- ggplot() +
    geom_boxplot(data = data_measured, aes(x = conc, y = abs * 100, group = conc)) +
    xlab("Concentration [µM]") +
    ylab("Viability [%]") +
    ggtitle(title)
  return(p)
}

#' Calculates the ic50 values
#' @export
#' @import drc
#' @import ggplot2
#' @param df a data.frame which contains all the data
#' @param abs_col the name of the column in df which contains the dependent variable
#' @param conc_col the name of the column in df which contains the different concentrations
#' @param substance_name_col the name of the column in df which contains the different names of the compounds
#' @param negative_identifier a character defining the name to identify the negative control within conc_col
#' @param positive_identifier a character defining the name to identify the positive control within conc_col
#' @return a list is returned containing the ic50 value the fitted plots and other parameters
#' @examples
#' path <- system.file("data", package = "MTT")
#' df <- read.csv(paste0(path, "/ExampleData.txt"))
#' ic50(df, "abs", "conc", "names", "neg", "pos")
ic50 <- function(df, abs_col, conc_col, substance_name_col, negative_identifier, positive_identifier) {
  substances <- unique(df$names)

  if (!(negative_identifier %in% substances)) {
    return(errorClass$new("the string for the negative control was not found!"))
  }
  if (!(positive_identifier %in% substances)) {
    return(errorClass$new("the string for the positive control was not found!"))
  }
  substances <- substances[substances != negative_identifier]
  substances <- substances[substances != positive_identifier]
  if (length(substances) < 1) {
    return(errorClass$new("The data for compounds seems to be missing"))
  }
  if (!is.numeric(df[, abs_col])) {
    return(errorClass$new("The absorbance data is not numerical"))
  }
  temp_conc <- df[, conc_col]
  temp_conc[temp_conc == negative_identifier] <- -1
  temp_conc[temp_conc == positive_identifier] <- -2
  temp_conc <- as.numeric(temp_conc)
  if (any(is.na(temp_conc))) {
    return(errorClass$new("The concentration data cannot be converted to numerical"))
  }
  df[, conc_col] <- temp_conc
  if (!is.numeric(df[, conc_col])) {
    return(errorClass$new("The concentration data is not numerical"))
  }
  neg_mean <- mean(df[df[, substance_name_col] == negative_identifier, abs_col])
  pos_mean <- mean(df[df[, substance_name_col] == positive_identifier, abs_col])
  df[, abs_col] <- (df[, abs_col] - pos_mean) / neg_mean
  res <- list()
  for (i in seq_along(substances)) {
    df_temp <- df[df$names == substances[i], ]
    m <- tryCatch(
      {
        m <- ic50_internal(df_temp, abs_col, conc_col, substances[i])
      },
      error = function(err) {
        retval <- errorClass$new(paste("A warning occurred: ", conditionMessage(err)))
        retval$object <- drawplotOnlyRawData(df_temp, abs_col, conc_col, substances[i])
        return(retval)
      }
    )
    res[[i]] <- m
  }

  return(res)
}
