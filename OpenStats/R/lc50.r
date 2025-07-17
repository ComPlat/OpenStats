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

shapenumber <- function(num) {
  if (is.finite(num)) {
    res <- signif(num)
  } else {
    res <- NA
  }
  return(res)
}

# calculates the robust 68th percentile of the residuals
# adapted from Motulsky HJ, Brown RE, BMC Bioinformatics 2006, 7:123
robust_68_percentile <- function(residuals) {
  res <- abs(residuals)
  res_sorted <- sort(res)
  res_percentiles <- (seq_len(length(res_sorted)) / length(res_sorted))
  index <- min(which(res_percentiles > 0.6825))
  x <- c(res_percentiles[index - 1], res_percentiles[index])
  y <- c(res_sorted[index - 1], res_sorted[index])
  m <- lm(y ~ x)
  x <- 0.6825
  y <- predict(m, as.data.frame(x))
  return(y)
}

# calculates the robust standard deviation of the residuals (RSDR)
# with correction for degrees of freedom
# adapted from Motulsky HJ, Brown RE, BMC Bioinformatics 2006, 7:123
# robust_standard_deviation_residuals = rsdr
rsdr <- function(residuals, number_of_coefficients_fitted) {
  resids <- as.numeric(residuals)
  resids <- na.omit(residuals)
  N <- length(resids)
  robust_68_percentile(residuals) *
    N / (N - number_of_coefficients_fitted)
}

# false discovery rate (FDR) approach,
# returns a T/F vector for selection of valid data points
# adapted from Motulsky HJ, Brown RE, BMC Bioinformatics 2006, 7:123
false_discovery_rate <- function(res) {
  N <- length(res)
  # Q=1%
  Q <- 0.01
  # number of coefficients in the fitted LL.4 model
  K <- 4
  R <- rsdr(res, K)
  id <- seq_len(length(res))
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

check_fit <- function(model, min_conc, max_conc,
                      min_abs, max_abs, substance_name) {
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
  Response_lowestdose_predicted <- predict(
    model, data.frame(concentration = min_conc),
    se.fit = FALSE
  )[1]
  Response_highestdose_predicted <- predict(
    model, data.frame(concentration = max_conc),
    se.fit = FALSE
  )[1]
  Response_difference <- abs(
    Response_lowestdose_predicted - Response_highestdose_predicted
  )
  HillCoefficient <- b
  IC50_relative <- e
  Problems <- ""
  if (Response_difference < 0.25) {
    Problems <- paste(Problems,
      "Response Difference lower than 25%",
      collapse = " , "
    )
  } else if (IC50_relative > max_conc) {
    Problems <- paste(Problems,
      "IC50 larger than highest measured concentration",
      collapse = " , "
    )
  } else if (IC50_relative < min_conc) {
    Problems <- paste(Problems,
      "IC50 lower than lowest measured concentration",
      collapse = " , "
    )
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
  pIC50 <- shapenumber(-log10(IC50_relative))
  p_value <- shapenumber(p_value)
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

drawplotOnlyRawData <- function(df, abs_col, conc_col, title) {
  data_measured <- data.frame(conc = df[, conc_col], abs = df[, abs_col])
  p <- ggplot() +
    geom_boxplot(
      data = data_measured,
      aes(x = conc, y = abs, group = conc)
    ) +
    geom_point(
      data = data_measured,
      aes(x = conc, y = abs)
    ) +
    xlab("Concentration") + # Concentration in µM
    ylab("Viability") + # Viaibility [%]
    ggtitle(title)
  return(p)
}

addOutlierLayer <- function(df_outlier, abs_col, conc_col, p) {
  if (nrow(df_outlier) == 0) {
    return(p)
  }
  p <- p +
    geom_point(
      data = df_outlier,
      aes(
        x = df_outlier[, conc_col],
        y = df_outlier[, abs_col], color = "Outlier"
      )
    ) +
    scale_color_manual(
      name = "",
      values = "darkorange"
    )
}

drawplot <- function(df, abs_col, conc_col, model, valid_points, title,
                     IC50_relative, IC50_relative_lower, IC50_relative_higher,
                     df_outlier, islog_x, islog_y) {
  min_conc <- min(df[, conc_col])
  max_conc <- max(df[, conc_col])
  grid <- seq(min_conc, max_conc, 0.1)
  plotFct <- (model$curve)[[1]]
  res <- plotFct(grid)
  data <- data.frame(
    abs = res,
    conc = grid
  )
  p <- drawplotOnlyRawData(df, abs_col, conc_col, title) +
    geom_line(data = data, aes(x = conc, y = abs))
  p <- addOutlierLayer(df_outlier, abs_col, conc_col, p)
  max_conc <- max(df[, conc_col]) +
    0.1 * (max(df[, conc_col]) - min(df[, conc_col]))
  min_conc <- min(df[, conc_col]) - 0.1 * min(df[, conc_col])
  xmin <- IC50_relative - IC50_relative_lower
  xmax <- IC50_relative + IC50_relative_higher
  if (!is.na(xmin) && !is.na(xmax)) {
    ymin <- min(df[, abs_col])
    ymax <- max(df[, abs_col])
    yrange <- ymax - ymin
    butt_height <- yrange * 0.1
    ymedian <- median(df[, abs_col])
    if (xmin > min_conc && xmax < max_conc) {
      p <- p + geom_errorbarh(
        aes(
          xmin = xmin,
          xmax = xmax, y = ymedian
        ),
        colour = "darkred", height = butt_height
      )
    } else {
      p <- p + labs(caption = "Confidence intervall not in conc. range") +
        theme(
          plot.caption =
            element_text(color = "darkred", face = "italic", size = 8)
        )
    }
  } else {
    p <- p + labs(caption = "Confidence intervall could not be calculated") +
      theme(
        plot.caption =
          element_text(color = "darkred", face = "italic", size = 8)
      )
  }
  if (islog_x) {
    p <- p + scale_x_log10()
  }
  if (islog_y) {
    p <- p + scale_y_log10()
  }
  return(p)
}

ic50_internal <- function(df, abs, conc,
                          title, df_outlier, islog_x, islog_y) {
  model <- drm(abs ~ conc,
    data = df, fct = LL.4(),
    robust = "median"
  )
  valid_points <- false_discovery_rate(residuals(model))
  model <- drm(abs ~ conc,
    data = df,
    subset = valid_points,
    start = model$coefficients,
    fct = LL.4(), robust = "mean",
  )
  res <- check_fit(
    model, min(df[, conc]),
    max(df[, conc]), min(df[, abs]), max(df[, abs]), title
  )
  p <- drawplot(
    df, abs, conc, model, valid_points, title, res$IC50_relative,
    res$IC50_relative_lower, res$IC50_relative_higher, df_outlier,
    islog_x, islog_y
  )
  return(list(res, p))
}

check_dr_df <- function(df, abs_col,
                        conc_col, substance_name_col) {
  if (!is.character(df[, substance_name_col]) &&
    !is.factor(df[, substance_name_col])) {
    return(errorClass$new("The substance names are not character"))
  }
  substances <- unique(df[, substance_name_col])
  if (length(substances) < 1) {
    return(errorClass$new("The data for compounds seems to be missing"))
  }
  if (!is.numeric(df[, abs_col])) {
    return(errorClass$new("The absorbance data is not numerical"))
  }
  return(NULL)
}

transform_conc_dr <- function(conc_col) {
  temp_conc <- as.numeric(conc_col)
  if (all(is.na(temp_conc))) {
    return(errorClass$new(
      "The concentration data cannot be converted to numerical"
    ))
  }
  if (!is.numeric(temp_conc)) {
    return(errorClass$new("The concentration data is not numerical"))
  }
  return(temp_conc)
}

#' @examples
#' path <- system.file("data", package = "MTT")
#' df <- read.csv(paste0(path, "/ExampleData.txt"))
#' ic50(df, "abs", "conc", "names", NULL, FALSE, FALSE)
ic50 <- function(df, abs_col, conc_col,
                 substance_name_col, outliers,
                 islog_x, islog_y) {
  # Checks
  err <- check_dr_df(df, abs_col, conc_col, substance_name_col)
  if (inherits(err, "errorClass")) {
    return(err)
  }
  substances <- unique(df[, substance_name_col])
  # Data preparation
  temp_conc <- transform_conc_dr(df[, conc_col])
  if (inherits(temp_conc, "errorClass")) {
    return(temp_conc)
  }
  df[, conc_col] <- temp_conc
  df <- data.frame(
    abs = df[, abs_col],
    conc = df[, conc_col],
    names = df[, substance_name_col]
  )
  res <- list()
  for (i in seq_along(substances)) {
    df_temp <- df[df$names == substances[i], ]
    df_outlier <- data.frame()
    outliers_temp <- outliers[[substances[i]]]
    if (!is.null(outliers_temp)) {
      df_outlier <- df_temp[outliers_temp, ]
      df_temp <- df_temp[-outliers_temp, ]
    }
    df_temp <- df_temp[!sapply(df_temp$conc, is.na), ]
    df_outlier <- df_outlier[!sapply(df_outlier$conc, is.na), ]

    m <- tryCatch(
      {
        m <- ic50_internal(
          df_temp,
          "abs", "conc",
          substances[i], df_outlier,
          islog_x, islog_y
        )
      },
      error = function(err) {
        retval <- errorClass$new(
          paste("A warning occurred: ", conditionMessage(err))
        )
        retval$object <- drawplotOnlyRawData(
          df_temp, "abs", "conc", substances[i]
        )
        return(retval)
      }
    )
    res[[i]] <- m
  }
  return(res)
}
