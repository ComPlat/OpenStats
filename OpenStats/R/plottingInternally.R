addFacet <- function(p, facetVar, facetMode, facetScales) {
  if (facetMode == "facet_wrap") {
    return(p + facet_wrap(. ~ .data[[facetVar]], scales = facetScales))
  } else if (facetMode == "facet_grid") {
    return(p + facet_grid(. ~ .data[[facetVar]], scales = facetScales))
  }
}

addInterval <- function(p, df, xCol, yCol, xMin, xMax, yMin, yMax) {
  x <- df[ , xCol]
  y <- df[ , yCol]
  if (is.numeric(x)) {
    p <- p + scale_x_continuous(limits = c(xMin, xMax))
  } else {
    choices <- unique(x)
    xStart <- which(xMin == choices)
    xEnd <- which(xMax == choices)
    p <- p + scale_x_discrete(limits = choices[xStart:xEnd])
  }
  if (is.numeric(y)) {
    p <- p + scale_y_continuous(limits = c(yMin, yMax))
  } else {
    choices <- unique(y)
    yStart <- which(yMin == choices)
    yEnd <- which(yMax == choices)
    p <- p + scale_y_discrete(limits = choices[yStart:yEnd])
  }
  return(p)
}

BoxplotFct <- function(df, x, y, xLabel, yLabel,
                       fillVar, legendTitleFill, fillTheme,
                       colourVar, legendTitleColour,
                       colourTheme, facetMode, facetVar, facetScales,
                       xMin, xMax, yMin, yMax) {
  aes <- aes(x = .data[[x]], y = .data[[y]])
  aesColour <- NULL
  aesFill <- NULL
  p <- NULL
  if (colourVar == "") {
    aesColour <- aes()
  } else {
    if (is.numeric(df[[colourVar]])) {
      aesColour <- aes(colour = factor(.data[[colourVar]]))
    } else {
      aesColour <- aes(colour = .data[[colourVar]])
    }
  }
  if (fillVar == "") {
    aesFill <- aes()
  } else {
    if (is.numeric(df[[fillVar]])) {
      aesFill <- aes(fill = factor(.data[[fillVar]]))
    } else {
      aesFill <- aes(fill = .data[[fillVar]])
    }
  }
  p <- ggplot() +
    geom_boxplot(
      data = df,
      aes(!!!aes, !!!aesColour, !!!aesFill,
        group = interaction(
          .data[[x]],
          !!!aesColour, !!!aesFill
        )
      )
    )
  p <- p + xlab(xLabel)
  p <- p + ylab(yLabel)
  p <- p + guides(fill = guide_legend(title = legendTitleFill))
  p <- p + guides(colour = guide_legend(title = legendTitleColour))
  p <- p + scale_fill_brewer(palette = fillTheme)
  p <- p + scale_color_brewer(palette = colourTheme)
  if (facetMode != "none") {
    p <- addFacet(p, facetVar, facetMode, facetScales)
  } else {
    p <- addInterval(p, df, x, y, xMin, xMax, yMin, yMax)
  }
  return(p + theme(text = element_text(size = 20)))
}

DotplotFct <- function(df, x, y, xLabel, yLabel,
                        colourVar, legendTitleColour,
                        colourTheme, facetMode, facetVar, facetScales,
                        xMin, xMax, yMin, yMax) {
  aes <- aes(x = .data[[x]], y = .data[[y]])
  aesColour <- NULL
  p <- NULL
  if (colourVar == "") {
    aesColour <- aes()
  } else {
    if (is.numeric(df[[colourVar]])) {
      aesColour <- aes(colour = factor(.data[[colourVar]]))
    } else {
      aesColour <- aes(colour = .data[[colourVar]])
    }
  }
  p <- ggplot() +
    geom_point(
      data = df,
      aes(!!!aes, !!!aesColour,
        group = interaction(
          .data[[x]],
          !!!aesColour
        )
      )
    )
  p <- p + xlab(xLabel)
  p <- p + ylab(yLabel)
  p <- p + guides(colour = guide_legend(title = legendTitleColour))
  p <- p + scale_color_brewer(palette = colourTheme)
  if (facetMode != "none") {
    p <- addFacet(p, facetVar, facetMode, facetScales)
  } else {
    p <- addInterval(p, df, x, y, xMin, xMax, yMin, yMax)
  }
  return(p + theme(text = element_text(size = 20)))
}

LineplotFct <- function(df, x, y, xLabel, yLabel,
                        colourVar, legendTitleColour,
                        colourTheme, facetMode, facetVar, facetScales,
                        xMin, xMax, yMin, yMax) {
  aes <- aes(x = .data[[x]], y = .data[[y]])
  aesColour <- NULL
  p <- NULL
  if (colourVar == "") {
    aesColour <- aes()
  } else {
    if (is.numeric(df[[colourVar]])) {
      aesColour <- aes(colour = factor(.data[[colourVar]]))
    } else {
      aesColour <- aes(colour = .data[[colourVar]])
    }
  }
  p <- ggplot() +
    geom_line(
      data = df,
      aes(!!!aes, !!!aesColour,
        group = interaction(
          .data[[x]],
          !!!aesColour
        )
      )
    )
  p <- p + xlab(xLabel)
  p <- p + ylab(yLabel)
  p <- p + guides(colour = guide_legend(title = legendTitleColour))
  p <- p + scale_color_brewer(palette = colourTheme)
  if (facetMode != "none") {
    p <- addFacet(p, facetVar, facetMode, facetScales)
  } else {
    p <- addInterval(p, df, x, y, xMin, xMax, yMin, yMax)
  }
  return(p + theme(text = element_text(size = 20)))
}
