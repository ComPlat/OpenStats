# Setup
# =============================================================================
coverage_test <- nzchar(Sys.getenv("R_COVR"))
run_test <- function(f) {
  if (coverage_test) f(app, srv, FALSE) else f(app, srv, TRUE)
}

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")
library(tinytest)

app <- OpenStats:::app()
srv <- app$server

# Tests boxplot
# =============================================================================
test_basic_boxplot <- function(app, srv, in_background) {
  options(OpenStats.background = in_background)
  ex <- NULL
  shiny::testServer(srv, {
    DataModelState$df <- CO2
    session$setInputs(`conditionedPanels` = 'Visualisation')
    session$setInputs(`VisConditionedPanels` = "Boxplot")

    session$setInputs(`VIS-yVar` = "uptake")
    session$setInputs(`VIS-xVar` = "conc")
    session$setInputs(`VIS-xaxisText` = "This is x")
    session$setInputs(`VIS-yaxisText` = "This is y")

    session$setInputs(`VIS-fill` = "")
    session$setInputs(`VIS-legendTitleFill` = "")
    session$setInputs(`VIS-themeFill` = "BuGn")

    session$setInputs(`VIS-col` = "")
    session$setInputs(`VIS-legendTitleCol` = "")
    session$setInputs(`VIS-theme` = "Accent")

    session$setInputs(`VIS-facetBy` = "")
    session$setInputs(`VIS-facetScales` = "free")
    session$setInputs(`VIS-xType` = "numeric")

    session$setInputs(`VIS-XRange` = c(1, 1000))
    session$setInputs(`VIS-YRange` = c(1, 100))

    session$setInputs(`VIS-widthPlot` = 10)
    session$setInputs(`VIS-heightPlot` = 10)
    session$setInputs(`VIS-resPlot` = 600)

    session$setInputs(`VIS-CreatePlotBox` = 1)

    t0 <- Sys.time()
    l0 <- length(ResultsState$all_data)
    repeat {
      ResultsState$bgp$tick(ResultsState, DataModelState, DataWranglingState)
      session$flushReact()

      if (l0 < length(session$userData$export)) break
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 30) break

      Sys.sleep(0.05)
    }
    ex <<- session$userData$export
  })

  built <- ggplot2::ggplot_build(ex[[1]]@p)
  labels <- built$plot$labels
  expect_equal(labels$y, "This is y")
  expect_equal(labels$x, "This is x")

  layers <- built$plot$layers[[1]]
  expect_true(inherits(layers$geom, "GeomBoxplot"))
  expect_equal(layers$data, CO2)

  mapping <- layers$mapping
  expect_equal("~.data[[\"conc\"]]", deparse(mapping$x))
  expect_equal("~.data[[\"uptake\"]]", deparse(mapping$y))
}
run_test(test_basic_boxplot)

test_boxplot <- function(app, srv, in_background) {
  options(OpenStats.background = in_background)
  ex <- NULL
  shiny::testServer(srv, {
    DataModelState$df <- CO2
    session$setInputs(`conditionedPanels` = 'Visualisation')
    session$setInputs(`VisConditionedPanels` = "Boxplot")

    session$setInputs(`VIS-yVar` = "uptake")
    session$setInputs(`VIS-xVar` = "conc")

    session$setInputs(`VIS-xaxisText` = "This is x")
    session$setInputs(`VIS-yaxisText` = "This is y")

    session$setInputs(`VIS-fill` = "Treatment")
    session$setInputs(`VIS-legendTitleFill` = "Filled by Treatment")
    session$setInputs(`VIS-themeFill` = "Reds")

    session$setInputs(`VIS-col` = "Type")
    session$setInputs(`VIS-legendTitleCol` = "Coloured by Type")
    session$setInputs(`VIS-theme` = "Set1")

    session$setInputs(`VIS-facetBy` = "Type")
    session$setInputs(`VIS-facetScales` = "fixed")
    session$setInputs(`VIS-xType` = "numeric")

    session$setInputs(`VIS-widthPlot` = 11)
    session$setInputs(`VIS-heightPlot` = 12)
    session$setInputs(`VIS-resPlot` = 610)

    session$setInputs(`conditionedPanels` = 'Visualisation')
    session$setInputs(`VisConditionedPanels` = "Boxplot")
    session$setInputs(`VIS-XRange` = list(150, 700)) # Is never called as it req(input$xVar)
    session$setInputs(`VIS-YRange` = c(17, 40))

    session$setInputs(`VIS-CreatePlotBox` = 1)

    t0 <- Sys.time()
    l0 <- length(ResultsState$all_data)
    repeat {
      ResultsState$bgp$tick(ResultsState, DataModelState, DataWranglingState)
      session$flushReact()

      if (l0 < length(session$userData$export)) break
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 30) break

      Sys.sleep(0.05)
    }
    ex <<- session$userData$export
  })

  built <- ggplot2::ggplot_build(ex[[1]]@p)
  labels <- built$plot$labels
  expect_equal(labels$y, "This is y")
  expect_equal(labels$x, "This is x")

  layers <- built$plot$layers[[1]]
  expect_true(inherits(layers$geom, "GeomBoxplot"))
  expect_equal(layers$data, CO2)

  mapping <- layers$mapping
  expect_equal("~.data[[\"conc\"]]", deparse(mapping$x))
  expect_equal("~.data[[\"uptake\"]]", deparse(mapping$y))
  expect_equal("~.data[[\"Type\"]]", deparse(mapping$colour))
  expect_equal("~.data[[\"Treatment\"]]", deparse(mapping$fill))

  bl <- built$layout
  expect_true(all(!unlist(bl$facet_params$free))) # As fixed is set

  scales <- built$plot$scales$get_scales("fill")
  fill_pal <- RColorBrewer::brewer.pal(3, "Reds")[1:2]
  expect_equal(scales$palette.cache, fill_pal)

  scales <- built$plot$scales$get_scales("colour")
  fill_pal <- RColorBrewer::brewer.pal(3, "Set1")[1:2]
  expect_equal(scales$palette.cache, fill_pal)
}
run_test(test_boxplot)


# Tests scatterplot
# =============================================================================
test_scatter <- function(app, srv, in_background) {
  options(OpenStats.background = in_background)
  ex <- NULL
  shiny::testServer(srv, {
    DataModelState$df <- CO2
    session$setInputs(`conditionedPanels` = 'Visualisation')
    session$setInputs(`VisConditionedPanels` = "ScatterPlot")

    session$setInputs(`VIS-yVar` = "uptake")
    session$setInputs(`VIS-xVar` = "conc")

    session$setInputs(`VIS-xaxisText` = "This is x")
    session$setInputs(`VIS-yaxisText` = "This is y")

    session$setInputs(`VIS-col` = "Type")
    session$setInputs(`VIS-legendTitleCol` = "Coloured by Type")
    session$setInputs(`VIS-theme` = "Dark2")

    session$setInputs(`VIS-facetBy` = "Type")
    session$setInputs(`VIS-facetScales` = "fixed")
    session$setInputs(`VIS-xType` = "numeric")

    session$setInputs(`VIS-widthPlot` = 11)
    session$setInputs(`VIS-heightPlot` = 12)
    session$setInputs(`VIS-resPlot` = 610)

    session$setInputs(`conditionedPanels` = 'Visualisation')
    session$setInputs(`VisConditionedPanels` = "ScatterPlot")
    session$setInputs(`VIS-XRange` = list(150, 700)) # Is never called as it req(input$xVar)
    session$setInputs(`VIS-YRange` = c(17, 40))

    session$setInputs(`VIS-CreatePlotScatter` = 1)

    t0 <- Sys.time()
    l0 <- length(ResultsState$all_data)
    repeat {
      ResultsState$bgp$tick(ResultsState, DataModelState, DataWranglingState)
      session$flushReact()

      if (l0 < length(session$userData$export)) break
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 30) break

      Sys.sleep(0.05)
    }
    ex <<- session$userData$export
  })

  built <- ggplot2::ggplot_build(ex[[1]]@p)
  labels <- built$plot$labels
  expect_equal(labels$y, "This is y")
  expect_equal(labels$x, "This is x")

  layers <- built$plot$layers[[1]]
  expect_true(inherits(layers$geom, "GeomPoint"))
  expect_equal(layers$data, CO2)

  mapping <- layers$mapping
  expect_equal("~.data[[\"conc\"]]", deparse(mapping$x))
  expect_equal("~.data[[\"uptake\"]]", deparse(mapping$y))
  expect_equal("~.data[[\"Type\"]]", deparse(mapping$colour))

  bl <- built$layout
  expect_true(all(!unlist(bl$facet_params$free)))# As fixed is set

  scales <- built$plot$scales$get_scales("colour")
  fill_pal <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]
  expect_equal(scales$palette.cache, fill_pal)

}
run_test(test_scatter)

# Tests lineplot
# =============================================================================
test_line <- function(app, srv, in_background) {
  options(OpenStats.background = in_background)
  ex <- NULL
  shiny::testServer(srv, {
    DataModelState$df <- CO2
    session$setInputs(`conditionedPanels` = 'Visualisation')
    session$setInputs(`VisConditionedPanels` = "Lineplot")

    session$setInputs(`VIS-yVar` = "uptake")
    session$setInputs(`VIS-xVar` = "conc")

    session$setInputs(`VIS-xaxisText` = "This is x")
    session$setInputs(`VIS-yaxisText` = "This is y")

    session$setInputs(`VIS-col` = "Type")
    session$setInputs(`VIS-legendTitleCol` = "Coloured by Type")
    session$setInputs(`VIS-theme` = "Dark2")

    session$setInputs(`VIS-facetBy` = "Type")
    session$setInputs(`VIS-facetScales` = "fixed")
    session$setInputs(`VIS-xType` = "numeric")

    session$setInputs(`VIS-widthPlot` = 11)
    session$setInputs(`VIS-heightPlot` = 12)
    session$setInputs(`VIS-resPlot` = 610)

    session$setInputs(`conditionedPanels` = 'Visualisation')
    session$setInputs(`VisConditionedPanels` = "LinePlot")
    session$setInputs(`VIS-XRange` = list(150, 700)) # Is never called as it req(input$xVar)
    session$setInputs(`VIS-YRange` = c(17, 40))

    session$setInputs(`VIS-CreatePlotLine` = 1)

    t0 <- Sys.time()
    l0 <- length(ResultsState$all_data)
    repeat {
      ResultsState$bgp$tick(ResultsState, DataModelState, DataWranglingState)
      session$flushReact()

      if (l0 < length(session$userData$export)) break
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 30) break

      Sys.sleep(0.05)
    }
    ex <<- session$userData$export
  })

  built <- ggplot2::ggplot_build(ex[[1]]@p)
  labels <- built$plot$labels
  expect_equal(labels$y, "This is y")
  expect_equal(labels$x, "This is x")

  layers <- built$plot$layers[[1]]
  expect_true(inherits(layers$geom, "GeomLine"))
  expect_equal(layers$data, CO2)

  mapping <- layers$mapping
  expect_equal("~.data[[\"conc\"]]", deparse(mapping$x))
  expect_equal("~.data[[\"uptake\"]]", deparse(mapping$y))
  expect_equal("~.data[[\"Type\"]]", deparse(mapping$colour))

  bl <- built$layout
  expect_true(all(!unlist(bl$facet_params$free)))# As fixed is set

  scales <- built$plot$scales$get_scales("colour")
  fill_pal <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]
  expect_equal(scales$palette.cache, fill_pal)

}
run_test(test_line)

# TODO: test plot model
