library(ggplot2)
library(shinytest2)
library(tinytest)
wait <- function(app) {
  try(app$wait_for_idle(), silent = TRUE)
}
app <- OpenStats:::app()
app <- shiny::shinyApp(app$ui, app$server)
app <- AppDriver$new(app)
wait(app)
app$upload_file(
  file = system.file("/test_data/CO2.csv", package = "OpenStats")
)
wait(app)

# Parameters for expected plot
# =============================================================================
x <- "conc"
y <- "uptake"
df <- CO2
x_min <- min(df[[x]], na.rm = TRUE)
padded_min_x <- x_min * 0.5
x_max <- max(df[[x]], na.rm = TRUE)
padded_max_x <- x_max * 1.25
y_min <- min(df[[y]], na.rm = TRUE)
padded_min_y <- y_min * 0.95
y_max <- max(df[[y]], na.rm = TRUE)
padded_max_y <- y_max * 1.05
fillTheme <- "BuGn"
fillVar <- "Treatment"
legendTitleFill <- "Treatment"
colourTheme <- "Accent"
colVar <- "Treatment"
legendTitleColour <- "Treatment"
facetVar <- "Type"
aesFill <- aes(fill = .data[[fillVar]])
CO2$Treatment <- as.character(CO2$Treatment)
CO2$Type <- as.character(CO2$Type)
aesColour <- aes(colour = .data[[colVar]])
aesFill <- aes(fill = .data[[fillVar]])

# Test basic plot
# =============================================================================
create_basic_plot <- function(app) {
  app$set_inputs(conditionedPanels = "Visualisation")
  wait(app)
  app$set_inputs(`VIS-yVar` = "uptake")
  wait(app)
  app$set_inputs(`VIS-xVar` = "conc")
  wait(app)
  app$click("VIS-CreatePlotBox")
  Sys.sleep(10)
  res <- app$get_values()$export
  res <- res$result_list
  res[[2]]@p
}
create_expected_plot <- function() {
  ggplot() +
    geom_boxplot(
      data = CO2,
      aes(x = .data[[x]], y = .data[[y]], group = interaction(.data[[x]]))
    ) +
    scale_x_continuous(limits = c(padded_min_x, padded_max_x)) +
    scale_y_continuous(limits = c(padded_min_y, padded_max_y)) +
    labs(x = "x label", y = "y label")
}
p <- create_basic_plot(app)
ep <- create_expected_plot()
p_data_extracted <- ggplot_build(p)$data[[1]]
ep_data_extracted <- ggplot_build(ep)$data[[1]]
expect_equal(p_data_extracted, ep_data_extracted)
pl <- ggplot_build(p)$plot$layers[[1]]
epl <- ggplot_build(ep)$plot$layers[[1]]
Map(function(a, b) {
  a <- deparse(a)
  b <- deparse(b)
  expect_equal(a, b)
}, pl$mapping, epl$mapping)

# plot with fill
# =============================================================================
create_fill_plot <- function(app) {
  app$set_inputs(`VIS-fill` = "Treatment")
  wait(app)
  app$click("VIS-CreatePlotBox")
  Sys.sleep(10)
  res <- app$get_values()$export
  res <- res$result_list
  res[[3]]@p
}
create_expected_plot <- function() {
  legendTitleColour <- ""
  aesColour <- aes()
  ggplot() +
    geom_boxplot(
      data = CO2,
      aes(
        x = .data[[x]], y = .data[[y]],
        !!!aesColour, !!!aesFill,
        group = interaction(.data[[x]], !!!aesColour, !!!aesFill),
      )
    ) +
    scale_x_continuous(limits = c(padded_min_x, padded_max_x)) +
    scale_y_continuous(limits = c(padded_min_y, padded_max_y)) +
    labs(x = "x label", y = "y label") +
    guides(fill = guide_legend(title = legendTitleFill)) +
    guides(colour = guide_legend(title = legendTitleColour)) +
    scale_fill_brewer(palette = fillTheme) +
    scale_color_brewer(palette = colourTheme)
}
p <- create_fill_plot(app)
ep <- create_expected_plot()
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)
pl <- ggplot_build(p)$plot$layers[[1]]
epl <- ggplot_build(ep)$plot$layers[[1]]
Map(function(a, b) {
  a <- deparse(a)
  b <- deparse(b)
  expect_equal(a, b)
}, pl$mapping, epl$mapping)

# Adding colour to plot
# =============================================================================
create_colour_plot <- function(app) {
  app$set_inputs(`VIS-col` = "Treatment")
  app$set_inputs(`VIS-fill` = "Treatment")
  wait(app)
  app$click("VIS-CreatePlotBox")
  Sys.sleep(10)
  res <- app$get_values()$export
  res <- res$result_list
  res[[4]]@p
}
create_expected_plot <- function() {
  CO2$Treatment <- as.character(CO2$Treatment)
  ggplot() +
    geom_boxplot(
      data = CO2,
      aes(
        x = .data[[x]], y = .data[[y]],
        !!!aesColour, !!!aesFill,
        group = interaction(.data[[x]], !!!aesColour, !!!aesFill),
      )
    ) +
    scale_x_continuous(limits = c(padded_min_x, padded_max_x)) +
    scale_y_continuous(limits = c(padded_min_y, padded_max_y)) +
    labs(x = "x label", y = "y label") +
    guides(fill = guide_legend(title = legendTitleFill)) +
    guides(colour = guide_legend(title = legendTitleColour)) +
    scale_fill_brewer(palette = fillTheme) +
    scale_color_brewer(palette = colourTheme)
}
p <- create_colour_plot(app)
ep <- create_expected_plot()
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)
pl <- ggplot_build(p)$plot$layers[[1]]
epl <- ggplot_build(ep)$plot$layers[[1]]
Map(function(a, b) {
  a <- deparse(a)
  b <- deparse(b)
  expect_equal(a, b)
}, pl$mapping, epl$mapping)


# Set scales for y
# =============================================================================
create_specified_yaxis <- function(app) {
  app$set_inputs(`VIS-YRange` = c(7.315, 26.3))
  wait(app)
  app$set_inputs(`VIS-col` = "Treatment")
  wait(app)
  app$set_inputs(`VIS-fill` = "Treatment")
  wait(app)
  app$click("VIS-CreatePlotBox")
  Sys.sleep(10)
  res <- app$get_values()$export
  res <- res$result_list
  res[[5]]@p
}
create_expected_plot <- function() {
  padded_min_y <- 7.315
  padded_max_y <- 26.3
  aesColour <- aes(colour = .data[[colVar]])
  aesFill <- aes(fill = .data[[fillVar]])
  ggplot() +
    geom_boxplot(
      data = CO2,
      aes(
        x = .data[[x]], y = .data[[y]],
        !!!aesColour, !!!aesFill,
        group = interaction(.data[[x]], !!!aesColour, !!!aesFill),
      )
    ) +
    scale_x_continuous(limits = c(padded_min_x, padded_max_x)) +
    scale_y_continuous(limits = c(padded_min_y, padded_max_y)) +
    labs(x = "x label", y = "y label") +
    guides(fill = guide_legend(title = legendTitleFill)) +
    guides(colour = guide_legend(title = legendTitleColour)) +
    scale_fill_brewer(palette = fillTheme) +
    scale_color_brewer(palette = colourTheme)
}
p <- create_specified_yaxis(app)
ep <- create_expected_plot()
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)
pl <- ggplot_build(p)$plot$layers[[1]]
epl <- ggplot_build(ep)$plot$layers[[1]]
Map(function(a, b) {
  a <- deparse(a)
  b <- deparse(b)
  expect_equal(a, b)
}, pl$mapping, epl$mapping)


# Set additional the x axis
# =============================================================================
create_specified_xaxis <- function(app) {
  app$set_inputs(`VIS-XRange` = c(47.5, 637))
  wait(app)
  app$click("VIS-CreatePlotBox")
  Sys.sleep(10)
  res <- app$get_values()$export
  res <- res$result_list
  res[[6]]@p
}
create_expected_plot <- function() {
  padded_min_y <- 7.315
  padded_max_y <- 26.3
  padded_min_x <- 47.5
  padded_max_x <- 637
  aesColour <- aes(colour = .data[[colVar]])
  aesFill <- aes(fill = .data[[fillVar]])
  ggplot() +
    geom_boxplot(
      data = CO2,
      aes(
        x = .data[[x]], y = .data[[y]],
        !!!aesColour, !!!aesFill,
        group = interaction(.data[[x]], !!!aesColour, !!!aesFill),
      )
    ) +
    scale_x_continuous(limits = c(padded_min_x, padded_max_x)) +
    scale_y_continuous(limits = c(padded_min_y, padded_max_y)) +
    labs(x = "x label", y = "y label") +
    guides(fill = guide_legend(title = legendTitleFill)) +
    guides(colour = guide_legend(title = legendTitleColour)) +
    scale_fill_brewer(palette = fillTheme) +
    scale_color_brewer(palette = colourTheme)
}
p <- create_specified_xaxis(app)
ep <- create_expected_plot()
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)
pl <- ggplot_build(p)$plot$layers[[1]]
epl <- ggplot_build(ep)$plot$layers[[1]]
Map(function(a, b) {
  a <- deparse(a)
  b <- deparse(b)
  expect_equal(a, b)
}, pl$mapping, epl$mapping)

# Test facet
# =============================================================================
create_facet_plot <- function(app) {
  app$set_inputs(`VIS-XRange` = c(47.5, 1250))
  wait(app)
  app$set_inputs(`VIS-YRange` = c(7.315, 47.775))
  wait(app)
  app$set_inputs(`VIS-facetBy` = "Type")
  wait(app)
  app$click("VIS-CreatePlotBox")
  Sys.sleep(10)
  res <- app$get_values()$export
  res <- res$result_list
  res[[7]]@p
}
create_expected_plot <- function() {
  padded_min_y <- 7.315
  padded_max_y <- 47.775
  padded_min_x <- 47.5
  padded_max_x <- 1250
  ep <- ggplot() +
    geom_boxplot(
      data = CO2,
      aes(
        x = .data[[x]], y = .data[[y]],
        !!!aesColour, !!!aesFill,
        group = interaction(.data[[x]], !!!aesColour, !!!aesFill),
      )
    ) +
    labs(x = "x label", y = "y label") +
    guides(fill = guide_legend(title = legendTitleFill)) +
    guides(colour = guide_legend(title = legendTitleColour)) +
    scale_fill_brewer(palette = fillTheme) +
    scale_color_brewer(palette = colourTheme) +
    facet_wrap(~ .data[[facetVar]], scales = "free")
}
p <- create_facet_plot(app)
ep <- create_expected_plot()
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)
pl <- ggplot_build(p)$plot$layers[[1]]
epl <- ggplot_build(ep)$plot$layers[[1]]
Map(function(a, b) {
  a <- deparse(a)
  b <- deparse(b)
  expect_equal(a, b)
}, pl$mapping, epl$mapping)


# Change themes
# =============================================================================
create_different_theme_plot <- function(app) {
  app$set_inputs(`VIS-themeFill` = "Greys")
  wait(app)
  app$set_inputs(`VIS-theme` = "Dark2")
  wait(app)
  app$click("VIS-CreatePlotBox")
  Sys.sleep(10)
  res <- app$get_values()$export
  res <- res$result_list
  res[[8]]@p
}
create_expected_plot <- function() {
  padded_min_y <- 7.315
  padded_max_y <- 47.775
  padded_min_x <- 47.5
  padded_max_x <- 1250
  fillTheme <- "Greys"
  fillVar <- "Treatment"
  colourTheme <- "Dark2"
  colVar <- "Treatment"
  ggplot() +
    geom_boxplot(
      data = CO2,
      aes(
        x = .data[[x]], y = .data[[y]],
        !!!aesColour, !!!aesFill,
        group = interaction(.data[[x]], !!!aesColour, !!!aesFill),
      )
    ) +
    labs(x = "x label", y = "y label") +
    guides(fill = guide_legend(title = legendTitleFill)) +
    guides(colour = guide_legend(title = legendTitleColour)) +
    scale_fill_brewer(palette = fillTheme) +
    scale_color_brewer(palette = colourTheme) +
    facet_wrap(~ .data[[facetVar]], scales = "free")
}
p <- create_different_theme_plot(app)
ep <- create_expected_plot()
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)
pl <- ggplot_build(p)$plot$layers[[1]]
epl <- ggplot_build(ep)$plot$layers[[1]]
Map(function(a, b) {
  a <- deparse(a)
  b <- deparse(b)
  expect_equal(a, b)
}, pl$mapping, epl$mapping)

# Test scatterplots
# =============================================================================
app$set_inputs(VisConditionedPanels = "Scatterplot")
wait(app)
app$click("VIS-CreatePlotScatter")
Sys.sleep(10)
res <- app$get_values()$export
res <- res$result_list
p <- res[[9]]@p
colourTheme <- "Dark2"
colVar <- "Treatment"
legendTitleColour <- "Title colour"
CO2$Treatment <- as.character(CO2$Treatment)
CO2$Type <- as.character(CO2$Type)
ep <- ggplot() +
  geom_point(
    data = CO2,
    aes(
      x = .data[[x]], y = .data[[y]],
      !!!aesColour,
      group = interaction(.data[[x]], !!!aesColour)
    )
  ) +
  labs(x = "x label", y = "y label") +
  guides(colour = guide_legend(title = legendTitleColour)) +
  scale_color_brewer(palette = colourTheme) +
  facet_wrap(~ .data[[facetVar]], scales = "free")
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)

# Test line plots
# =============================================================================
app$set_inputs(VisConditionedPanels = "Lineplot")
wait(app)
app$click("VIS-CreatePlotLine")
Sys.sleep(10)
res <- app$get_values()$export
res <- res$result_list
p <- res[[10]]@p
colourTheme <- "Dark2"
colVar <- "Treatment"
legendTitleColour <- "Title colour"
aesColour <- aes(colour = .data[[colVar]])
CO2$Treatment <- as.character(CO2$Treatment)
CO2$Type <- as.character(CO2$Type)
facetVar <- "Type"
ep <- ggplot() +
  geom_line(
    data = CO2,
    aes(
      x = .data[[x]], y = .data[[y]],
      !!!aesColour,
      group = interaction(.data[[x]], !!!aesColour)
    )
  ) +
  labs(x = "x label", y = "y label") +
  guides(colour = guide_legend(title = legendTitleColour)) +
  scale_color_brewer(palette = colourTheme) +
  facet_wrap(~ .data[[facetVar]], scales = "free")
pd <- ggplot_build(p)$data[[1]]
epd <- ggplot_build(ep)$data[[1]]
tinytest::expect_equal(pd, epd)

app$stop()
