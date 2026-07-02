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

histplot_fct <- function(df, y, yLabel,
                         fillVar, legendTitlefill, fillTheme,
                         facetVar, facetMode, facetScales,
                         frequency_or_density, bins) {
  base_aes <- ggplot2::aes(x = .data[[y]])
  aesfill <- NULL
  p <- NULL
  if (fillVar == "") {
    aesfill <- ggplot2::aes()
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
        ggplot2::aes(!!!base_aes, !!!aesfill, group = !!!aesfill),
        bins = bins
      )
  } else if (frequency_or_density == "density") {
    p <- ggplot2::ggplot() +
      ggplot2::geom_density(
        data = df,
        ggplot2::aes(!!!base_aes, !!!aesfill, group = !!!aesfill)
      )
  }
  p <- p + ggplot2::xlab(env_plotting_V1_2$parse_label(xLabel))
  p <- p + ggplot2::ylab(env_plotting_V1_2$parse_label(yLabel))
  p <- p + ggplot2::guides(fill = ggplot2::guide_legend(title = env_plotting_V1_2$parse_label(legendTitlefill)))
  if (fillVar != "") p <- p + ggplot2::scale_color_brewer(palette = fillTheme)
  if (facetMode != "none") {
    p <- env_plotting_V1_2$add_facet(p, facetVar, facetMode, facetScales)
  }
  return(p + ggplot2::theme(text = ggplot2::element_text(size = 20)))
}

df <- data.frame(
  y = c(
    rnorm(50L, 10, 1), # A-C
    rnorm(50L, 50, 2), # A-D
    rnorm(50L, 20, 1), # B-C
    rnorm(50L, 75, 2) # B-D
  ),
  fill = rep(c("A", "B"), each = 100L),
  facet = c(rep(c("C", "D"), each = 50L), rep(c("C", "D"), each = 50L))
)
xLabel <- ""
yLabel <- ""
legendTitlefill <- ""
fillTheme <- "BuGn"
facetMode <- "facet_wrap"
facetScales <- "free"
frequency_or_density <- "frequency"
frequency_or_density <- "density"
bins <- 30L
histplot_fct(
  df,
  "y", yLabel,
  "fill", legendTitlefill, fillTheme,
  "facet", facetMode, facetScales,
  frequency_or_density, bins
)
# TODO:
# - add histplot_fct to Backend_PlottingInternally.R --> Done
# - define an own engine class in Backend_V1_2_Engine.R --> Done
#   * the visualization class is already heavily overloaded
# - add a histogram sidebar in UI_Visualization.R --> Done
# - update server Server_Visualization.R --> Done
#   * don't show XRangeUI for histograms --> Done
#   * don't show yRangeUI for histograms --> Done
#   * don't show xVarUI --> Done
#   * don't show colUI --> Done
#   * add the UI elements for frequency or densitry --> Done
#   * add the bins UI elements --> Done
#   * call the engine class --> Done
# - update eval_entry_V1_2 in Backend_History.R --> Done
#   So that the history entry for histograms is evaluated
# - update test_Backend_Visualisation.R
# - update test_Backend_TestEngine.R
# - update test_Backend_TestHistory.R
#   * create a dedicated history.json and run it
# - update test_Server_Visualisation.R
