library(tinytest)

test_layers <- function(p, expected_layer) {
  layers <- p$layers
  expect_equal(length(layers), 1L)
  expect_true(inherits(layers[[1]]$geom, expected_layer))
}

test_mapping <- function(p, expected) {
  built <- ggplot2::ggplot_build(p)
  layers <- built$plot$layers[[1]]
  mapping <- layers$computed_mapping
  within <- function(a, b) {
    vapply(a, function(ae) {
      grepl(ae, b)
    }, logical(1)) |> all()
  }
  checks <- Map(function(a, b) {
    str <- deparse(mapping[[b]])
    str <- paste(str, collapse = "; ") # removes line breaks
    within(a, str)
  }, expected, names(mapping)) |> unlist()
  expect_true(all(checks))
}

test_panel <- function(p) {
  built <- ggplot2::ggplot_build(p)
  bl <- built$layout
  expect_equal(as.character(bl$layout[1, 4]), "Quebec")
  expect_equal(as.character(bl$layout[2, 4]), "Mississippi")
}

test_boxplot_fct <- function() {
  base_p <- OpenStats:::boxplot_fct(
    df = CO2,
    x = "conc", y = "uptake", xLabel = "", yLabel = "",
    fillVar = "", legendTitleFill = "", fillTheme = "BuGn",
    colourVar = "", legendTitleColour = "", colourTheme = "Dark2",
    facetMode = "none", facetVar = "", facetScales = "",
    xMin = 50, xMax = 1200, yMin = 0, yMax = 50
  )
  filled_p <- OpenStats:::boxplot_fct(
    df = CO2,
    x = "conc", y = "uptake", xLabel = "", yLabel = "",
    fillVar = "Type", legendTitleFill = "", fillTheme = "BuGn",
    colourVar = "", legendTitleColour = "", colourTheme = "Dark2",
    facetMode = "none", facetVar = "", facetScales = "",
    xMin = 50, xMax = 1200, yMin = 0, yMax = 50
  )
  coloured_p <- OpenStats:::boxplot_fct(
    df = CO2,
    x = "conc", y = "uptake", xLabel = "", yLabel = "",
    fillVar = "", legendTitleFill = "", fillTheme = "BuGn",
    colourVar = "Treatment", legendTitleColour = "", colourTheme = "Dark2",
    facetMode = "none", facetVar = "", facetScales = "",
    xMin = 50, xMax = 1200, yMin = 0, yMax = 50
  )
  filled_and_coloured_p <- OpenStats:::boxplot_fct(
    df = CO2,
    x = "conc", y = "uptake", xLabel = "", yLabel = "",
    fillVar = "Type", legendTitleFill = "", fillTheme = "BuGn",
    colourVar = "Treatment", legendTitleColour = "", colourTheme = "Dark2",
    facetMode = "none", facetVar = "", facetScales = "",
    xMin = 50, xMax = 1200, yMin = 0, yMax = 50
  )
  all_p <- OpenStats:::boxplot_fct(
    df = CO2,
    x = "conc", y = "uptake", xLabel = "", yLabel = "",
    fillVar = "Type", legendTitleFill = "", fillTheme = "BuGn",
    colourVar = "Treatment", legendTitleColour = "", colourTheme = "Dark2",
    facetMode = "facet_wrap", facetVar = "Type", facetScales = "free",
    xMin = 50, xMax = 1200, yMin = 0, yMax = 50
  )

  test_layers(base_p, "GeomBoxplot")
  test_layers(filled_p, "GeomBoxplot")
  test_layers(coloured_p, "GeomBoxplot")
  test_layers(filled_and_coloured_p, "GeomBoxplot")
  test_layers(all_p, "GeomBoxplot")

  test_mapping(base_p,
    list(x = "conc", y = "uptake", group = "conc"))
  test_mapping(filled_p,
    list(x = "conc", y = "uptake", fill = "Type", group = c("conc", "Type")))
  test_mapping(coloured_p,
    list(x = "conc", y = "uptake", colour = "Treatment", group = c("conc", "Treatment")))
  test_mapping(filled_and_coloured_p,
    list(x = "conc", y = "uptake", colour = "Treatment", fill = "Type", group = c("conc", "Treatment", "Type")))
  test_mapping(all_p,
    list(x = "conc", y = "uptake", colour = "Treatment", fill = "Type", group = c("conc", "Treatment", "Type")))

  test_panel(all_p)
  invisible(NULL)
}
test_boxplot_fct()

test_dotplot_fct <- function() {
  base_p <- OpenStats:::dotplot_fct(
    df = CO2,
    x = "conc", y = "uptake", xLabel = "", yLabel = "",
    colourVar = "", legendTitleColour = "", colourTheme = "Dark2",
    facetMode = "none", facetVar = "", facetScales = "",
    xMin = 50, xMax = 1200, yMin = 0, yMax = 50
  )
  coloured_p <- OpenStats:::dotplot_fct(
    df = CO2,
    x = "conc", y = "uptake", xLabel = "", yLabel = "",
    colourVar = "Treatment", legendTitleColour = "", colourTheme = "Dark2",
    facetMode = "none", facetVar = "", facetScales = "",
    xMin = 50, xMax = 1200, yMin = 0, yMax = 50
  )
  all_p <- OpenStats:::dotplot_fct(
    df = CO2,
    x = "conc", y = "uptake", xLabel = "", yLabel = "",
    colourVar = "Treatment", legendTitleColour = "", colourTheme = "Dark2",
    facetMode = "facet_wrap", facetVar = "Type", facetScales = "free",
    xMin = 50, xMax = 1200, yMin = 0, yMax = 50
  )

  test_layers(base_p, "GeomPoint")
  test_layers(coloured_p, "GeomPoint")
  test_layers(all_p, "GeomPoint")

  test_mapping(base_p,
    list(x = "conc", y = "uptake", group = "conc"))
  test_mapping(coloured_p,
    list(x = "conc", y = "uptake", colour = "Treatment", group = c("conc", "Treatment")))
  test_mapping(all_p,
    list(x = "conc", y = "uptake", colour = "Treatment", group = c("conc", "Treatment")))

  test_panel(all_p)
  invisible(NULL)
}
test_dotplot_fct()

test_lineplot_fct <- function() {
  base_p <- OpenStats:::lineplot_fct(
    df = CO2,
    x = "conc", y = "uptake", xLabel = "", yLabel = "",
    colourVar = "", legendTitleColour = "", colourTheme = "Dark2",
    facetMode = "none", facetVar = "", facetScales = "",
    xMin = 50, xMax = 1200, yMin = 0, yMax = 50
  )
  coloured_p <- OpenStats:::lineplot_fct(
    df = CO2,
    x = "conc", y = "uptake", xLabel = "", yLabel = "",
    colourVar = "Treatment", legendTitleColour = "", colourTheme = "Dark2",
    facetMode = "none", facetVar = "", facetScales = "",
    xMin = 50, xMax = 1200, yMin = 0, yMax = 50
  )
  all_p <- OpenStats:::lineplot_fct(
    df = CO2,
    x = "conc", y = "uptake", xLabel = "", yLabel = "",
    colourVar = "Treatment", legendTitleColour = "", colourTheme = "Dark2",
    facetMode = "facet_wrap", facetVar = "Type", facetScales = "free",
    xMin = 50, xMax = 1200, yMin = 0, yMax = 50
  )

  test_layers(base_p, "GeomLine")
  test_layers(coloured_p, "GeomLine")
  test_layers(all_p, "GeomLine")

  test_mapping(base_p,
    list(x = "conc", y = "uptake", group = "conc"))
  test_mapping(coloured_p,
    list(x = "conc", y = "uptake", colour = "Treatment", group = c("conc", "Treatment")))
  test_mapping(all_p,
    list(x = "conc", y = "uptake", colour = "Treatment", group = c("conc", "Treatment")))

  test_panel(all_p)
  invisible(NULL)
}
test_lineplot_fct()
