errorClass <- R6::R6Class("errorClass", # not exported via env_lc_V1_2 as engine tests whether the result inherits from errorClass
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

env_lc_V1_2 <- new.env(parent = getNamespace("OpenStats"))
env_lc_V1_2$shapenumber <- function(num) {
  if (is.finite(num)) {
    res <- signif(num)
  } else {
    res <- NA
  }
  return(res)
}

# calculates the robust 68th percentile of the residuals
# adapted from Motulsky HJ, Brown RE, BMC Bioinformatics 2006, 7:123
env_lc_V1_2$robust_68_percentile <- function(residuals) {
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
# robust_standard_deviation_residuals = env_lc_V1_2$rsdr
env_lc_V1_2$rsdr <- function(residuals, number_of_coefficients_fitted) {
  resids <- as.numeric(residuals)
  resids <- na.omit(residuals)
  N <- length(resids)
  env_lc_V1_2$robust_68_percentile(residuals) *
    N / (N - number_of_coefficients_fitted)
}

# false discovery rate (FDR) approach,
# returns a T/F vector for selection of valid data points
# adapted from Motulsky HJ, Brown RE, BMC Bioinformatics 2006, 7:123
env_lc_V1_2$false_discovery_rate <- function(res) {
  N <- length(res)
  # Q=1%
  Q <- 0.01
  # number of coefficients in the fitted LL.4 model
  K <- 4
  R <- env_lc_V1_2$rsdr(res, K)
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

env_lc_V1_2$check_fit <- function(model, min_conc, max_conc,
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
  Response_lowestdose_predicted <- env_lc_V1_2$shapenumber(Response_lowestdose_predicted)
  Response_highestdose_predicted <- env_lc_V1_2$shapenumber(Response_highestdose_predicted)
  HillCoefficient <- env_lc_V1_2$shapenumber(HillCoefficient)
  IC50_relative <- env_lc_V1_2$shapenumber(IC50_relative)
  IC50_relative_lower <- env_lc_V1_2$shapenumber(IC50_relative_lower)
  IC50_relative_higher <- env_lc_V1_2$shapenumber(IC50_relative_higher)
  pIC50 <- env_lc_V1_2$shapenumber(-log10(IC50_relative))
  p_value <- env_lc_V1_2$shapenumber(p_value)
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

env_lc_V1_2$drawplot_only_raw_data <- function(df, abs_col, conc_col, title) {
  conc <- function() stop("Should never be called") # Please R CMD check

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

env_lc_V1_2$drawplot <- function(df, abs_col, conc_col, model, valid_points, title,
                     IC50_relative, IC50_relative_lower, IC50_relative_higher,
                     islog_x, islog_y) {

  conc <- function() stop("Should never be called") # Please R CMD check

  min_conc <- min(df[, conc_col])
  max_conc <- max(df[, conc_col])
  grid <- seq(min_conc, max_conc, 0.1)
  plotFct <- (model$curve)[[1]]
  res <- plotFct(grid)
  data <- data.frame(
    abs = res,
    conc = grid
  )
  p <- env_lc_V1_2$drawplot_only_raw_data(df, abs_col, conc_col, title) +
    geom_line(data = data, aes(x = conc, y = abs))
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

env_lc_V1_2$ic50_internal <- function(df, abs, conc,
                          title, islog_x, islog_y) {
  model <- drm(abs ~ conc,
    data = df, fct = LL.4(),
    robust = "median"
  )
  valid_points <- env_lc_V1_2$false_discovery_rate(residuals(model))
  model <- drm(abs ~ conc,
    data = df,
    subset = valid_points,
    start = model$coefficients,
    fct = LL.4(), robust = "mean",
  )
  res <- env_lc_V1_2$check_fit(
    model, min(df[, conc]),
    max(df[, conc]), min(df[, abs]), max(df[, abs]), title
  )
  p <- env_lc_V1_2$drawplot(
    df, abs, conc, model, valid_points, title, res$IC50_relative,
    res$IC50_relative_lower, res$IC50_relative_higher,
    islog_x, islog_y
  )
  return(list(res, p))
}

env_lc_V1_2$check_dr_df <- function(df, abs_col,
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

env_lc_V1_2$transform_conc_dr <- function(conc_col) {
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
env_lc_V1_2$ic50 <- function(df, abs_col, conc_col,
                 substance_name_col,
                 islog_x, islog_y) {
  # Checks
  err <- env_lc_V1_2$check_dr_df(df, abs_col, conc_col, substance_name_col)
  if (inherits(err, "errorClass")) {
    return(err)
  }
  substances <- unique(df[, substance_name_col])
  # Data preparation
  temp_conc <- env_lc_V1_2$transform_conc_dr(df[, conc_col])
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
    df_temp <- df_temp[!sapply(df_temp$conc, is.na), ]

    m <- tryCatch(
      {
        m <- env_lc_V1_2$ic50_internal(
          df_temp,
          "abs", "conc",
          substances[i],
          islog_x, islog_y
        )
      },
      error = function(err) {
        retval <- errorClass$new(
          paste("A warning occurred: ", conditionMessage(err))
        )
        retval$object <- env_lc_V1_2$drawplot_only_raw_data(
          df_temp, "abs", "conc", substances[i]
        )
        return(retval)
      }
    )
    res[[i]] <- m
  }
  return(res)
}
