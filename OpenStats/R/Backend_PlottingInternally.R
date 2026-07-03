env_plotting_V1_2 <- new.env(parent = getNamespace("OpenStats"))

parse_label <- function(lbl) {
  if (!nzchar(lbl)) return(lbl)
  tryCatch(parse(text = lbl)[[1]], error = function(e) lbl)
}
env_plotting_V1_2$parse_label <- parse_label

add_facet <- function(p, facetVar, facetMode, facetScales) {
  if (facetMode == "facet_wrap") {
    return(p + ggplot2::facet_wrap(. ~ .data[[facetVar]], scales = facetScales))
  } else if (facetMode == "facet_grid") {
    return(p + ggplot2::facet_grid(. ~ .data[[facetVar]], scales = facetScales))
  }
  return(p)
}
env_plotting_V1_2$add_facet <- add_facet

add_interval <- function(p, df, xCol, yCol, xMin, xMax, yMin, yMax) {
  x <- df[ , xCol]
  y <- df[ , yCol]
  if (is.numeric(x)) {
    p <- p + ggplot2::scale_x_continuous(limits = c(xMin, xMax))
  } else {
    choices <- unique(x)
    xStart <- which(xMin == choices)
    xEnd <- which(xMax == choices)
    p <- p + ggplot2::scale_x_discrete(limits = choices[xStart:xEnd])
  }
  if (is.numeric(y)) {
    p <- p + ggplot2::scale_y_continuous(limits = c(yMin, yMax))
  } else {
    choices <- unique(y)
    yStart <- which(yMin == choices)
    yEnd <- which(yMax == choices)
    p <- p + ggplot2::scale_y_discrete(limits = choices[yStart:yEnd])
  }
  return(p)
}
env_plotting_V1_2$add_interval <- add_interval

boxplot_fct <- function(df, x, y, xLabel, yLabel,
                       fillVar, legendTitleFill, fillTheme,
                       colourVar, legendTitleColour,
                       colourTheme, facetMode, facetVar, facetScales,
                       xMin, xMax, yMin, yMax) {
  base_aes <- ggplot2::aes(x = .data[[x]], y = .data[[y]])
  aesColour <- NULL
  aesFill <- NULL
  p <- NULL
  if (colourVar == "") {
    aesColour <- ggplot2::aes()
  } else {
    if (is.numeric(df[[colourVar]])) {
      aesColour <- ggplot2::aes(colour = factor(.data[[colourVar]]))
    } else {
      aesColour <- ggplot2::aes(colour = .data[[colourVar]])
    }
  }
  if (fillVar == "") {
    aesFill <- ggplot2::aes()
  } else {
    if (is.numeric(df[[fillVar]])) {
      aesFill <- ggplot2::aes(fill = factor(.data[[fillVar]]))
    } else {
      aesFill <- ggplot2::aes(fill = .data[[fillVar]])
    }
  }
  p <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = df,
      ggplot2::aes(!!!base_aes, !!!aesColour, !!!aesFill,
        group = interaction(
          .data[[x]],
          !!!aesColour, !!!aesFill
        )
      )
    )
  p <- p + ggplot2::xlab(env_plotting_V1_2$parse_label(xLabel))
  p <- p + ggplot2::ylab(env_plotting_V1_2$parse_label(yLabel))
  p <- p + ggplot2::guides(fill = ggplot2::guide_legend(title = env_plotting_V1_2$parse_label(legendTitleFill)))
  p <- p + ggplot2::guides(colour = ggplot2::guide_legend(title = env_plotting_V1_2$parse_label(legendTitleColour)))

  if (fillVar != "")  p <- p + ggplot2::scale_fill_brewer(palette = fillTheme)
  if (colourVar != "") p <- p + ggplot2::scale_color_brewer(palette = colourTheme)
  if (facetMode != "none") {
    p <- env_plotting_V1_2$add_facet(p, facetVar, facetMode, facetScales)
  } else {
    p <- env_plotting_V1_2$add_interval(p, df, x, y, xMin, xMax, yMin, yMax)
  }
  return(p + ggplot2::theme(text = ggplot2::element_text(size = 20)))
}
env_plotting_V1_2$boxplot_fct <- boxplot_fct

dotplot_fct <- function(df, x, y, xLabel, yLabel,
                        colourVar, legendTitleColour,
                        colourTheme, facetMode, facetVar, facetScales,
                        xMin, xMax, yMin, yMax) {
  base_aes <- ggplot2::aes(x = .data[[x]], y = .data[[y]])
  aesColour <- NULL
  p <- NULL
  if (colourVar == "") {
    aesColour <- ggplot2::aes()
  } else {
    if (is.numeric(df[[colourVar]])) {
      aesColour <- ggplot2::aes(colour = factor(.data[[colourVar]]))
    } else {
      aesColour <- ggplot2::aes(colour = .data[[colourVar]])
    }
  }
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(!!!base_aes, !!!aesColour,
        group = interaction(
          .data[[x]],
          !!!aesColour
        )
      )
    )
  p <- p + ggplot2::xlab(env_plotting_V1_2$parse_label(xLabel))
  p <- p + ggplot2::ylab(env_plotting_V1_2$parse_label(yLabel))
  p <- p + ggplot2::guides(colour = ggplot2::guide_legend(title = env_plotting_V1_2$parse_label(legendTitleColour)))
  if (colourVar != "") p <- p + ggplot2::scale_color_brewer(palette = colourTheme)
  if (facetMode != "none") {
    p <- env_plotting_V1_2$add_facet(p, facetVar, facetMode, facetScales)
  } else {
    p <- env_plotting_V1_2$add_interval(p, df, x, y, xMin, xMax, yMin, yMax)
  }
  return(p + ggplot2::theme(text = ggplot2::element_text(size = 20)))
}
env_plotting_V1_2$dotplot_fct <- dotplot_fct

lineplot_fct <- function(df, x, y, xLabel, yLabel,
                        colourVar, legendTitleColour,
                        colourTheme, facetMode, facetVar, facetScales,
                        xMin, xMax, yMin, yMax) {
  base_aes <- ggplot2::aes(x = .data[[x]], y = .data[[y]])
  aesColour <- NULL
  p <- NULL
  if (colourVar == "") {
    aesColour <- ggplot2::aes()
  } else {
    if (is.numeric(df[[colourVar]])) {
      aesColour <- ggplot2::aes(colour = factor(.data[[colourVar]]))
    } else {
      aesColour <- ggplot2::aes(colour = .data[[colourVar]])
    }
  }
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = df,
      ggplot2::aes(!!!base_aes, !!!aesColour,
        group = interaction(
          .data[[x]],
          !!!aesColour
        )
      )
    )
  p <- p + ggplot2::xlab(env_plotting_V1_2$parse_label(xLabel))
  p <- p + ggplot2::ylab(env_plotting_V1_2$parse_label(yLabel))
  p <- p + ggplot2::guides(colour = ggplot2::guide_legend(title = env_plotting_V1_2$parse_label(legendTitleColour)))
  if (colourVar != "") p <- p + ggplot2::scale_color_brewer(palette = colourTheme)
  if (facetMode != "none") {
    p <- env_plotting_V1_2$add_facet(p, facetVar, facetMode, facetScales)
  } else {
    p <- env_plotting_V1_2$add_interval(p, df, x, y, xMin, xMax, yMin, yMax)
  }
  return(p + ggplot2::theme(text = ggplot2::element_text(size = 20)))
}
env_plotting_V1_2$lineplot_fct <- lineplot_fct

histplot_fct <- function(df, y, yLabel,
                         fillVar, legendTitlefill, fillTheme,
                         facetVar, facetMode, facetScales,
                         frequency_or_density, bins) {
  base_aes <- ggplot2::aes(x = .data[[y]])
  is_density <- frequency_or_density == "density"
  aesfill <- NULL
  p <- NULL
  if (fillVar == "") {
    aesfill <- ggplot2::aes()
  } else if (is_density) {
    if (is.numeric(df[[fillVar]])) {
      aesfill <- ggplot2::aes(colour = factor(.data[[fillVar]]))
    } else {
      aesfill <- ggplot2::aes(colour = .data[[fillVar]])
    }
  } else {
    if (is.numeric(df[[fillVar]])) {
      aesfill <- ggplot2::aes(fill = factor(.data[[fillVar]]))
    } else {
      aesfill <- ggplot2::aes(fill = .data[[fillVar]])
    }
  }

  if (frequency_or_density == "frequency") {
    p <- ggplot2::ggplot() +
      ggplot2::stat_bin(
        data = df,
        ggplot2::aes(!!!base_aes, !!!aesfill),
        bins = bins
      )
  } else if (frequency_or_density == "density") {
    p <- ggplot2::ggplot() +
      ggplot2::geom_density(
        data = df,
        ggplot2::aes(!!!base_aes, !!!aesfill),
        linewidth = 1
      )
  }
  p <- p + ggplot2::ylab(env_plotting_V1_2$parse_label(yLabel))
  if (is_density) {
    p <- p + ggplot2::guides(colour = ggplot2::guide_legend(title = env_plotting_V1_2$parse_label(legendTitlefill)))
    if (fillVar != "") p <- p + ggplot2::scale_color_brewer(palette = fillTheme)
  } else {
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(title = env_plotting_V1_2$parse_label(legendTitlefill)))
    if (fillVar != "") p <- p + ggplot2::scale_fill_brewer(palette = fillTheme)
  }
  if (facetMode != "none") {
    p <- env_plotting_V1_2$add_facet(p, facetVar, facetMode, facetScales)
  }
  return(p + ggplot2::theme_bw() + ggplot2::theme(text = ggplot2::element_text(size = 20)))
}
env_plotting_V1_2$histplot_fct <- histplot_fct
