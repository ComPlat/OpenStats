options(shiny.launch.browser = TRUE)
library(shiny)
library(ggplot2)

CO2$conc <- as.factor(CO2$conc)

conc_levels      <- levels(CO2$conc)
treatment_levels <- levels(factor(CO2$Treatment))

ui <- fluidPage(
  plotOutput("plot", hover = hoverOpts("plot_hover", delay = 50)),
  uiOutput("tooltip")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(CO2,
      aes(x = conc, y = uptake, group = interaction(conc, Treatment), fill = Treatment, colour = Treatment)
    ) +
      geom_boxplot() +
      facet_wrap(~Type)
  })

  output$tooltip <- renderUI({
    hover <- input$plot_hover
    req(hover)

    x_var <- hover$mapping$x
    y_var <- hover$mapping$y
    group_var <- hover$mapping$group
    fill_var <- hover$mapping$fill
    colour_var <- hover$mapping$colour
    cat("x: ", x_var, hover$x, "\n")
    cat("y: ", y_var, hover$y, "\n")
    cat("fill: ", fill_var, hover$fill, "\n") # Unfortunatly NULL; Thus, we cannot differentiate the groups
    cat("colour: ", colour_var, "\n")
    cat("panel 1: ", hover$panelvar1, "\n")

    # Factor x: ggplot maps levels to integers 1, 2, ...
    conc_idx <- round(hover$x)
    if (conc_idx < 1 || conc_idx > length(conc_levels)) return(NULL)
    conc_val <- conc_levels[conc_idx]

    # Dodge width 0.75, 2 groups: boundary between boxes sits at the integer
    # left of integer = first factor level, right = second
    treatment_val <- if (hover$x < conc_idx) treatment_levels[1] else treatment_levels[2]

    type_val <- hover$panelvar1

    sub <- CO2[CO2$conc == conc_val & CO2$Treatment == treatment_val & CO2$Type == type_val, ]
    if (nrow(sub) == 0) return(NULL)

    s <- boxplot.stats(sub$uptake)$stats  # min whisker, Q1, median, Q3, max whisker

    htmltools::div(
      style = paste0(
        "position: fixed;",
        "left: ", hover$coords_css$x + 10, "px;",
        "top: ",  hover$coords_css$y - 30, "px;",
        "background: white; border: 1px solid #aaa;",
        "padding: 4px 8px; border-radius: 4px; pointer-events: none;"
      ),
      htmltools::tags$b(paste0(treatment_val, " / conc ", conc_val, " / ", type_val)),
      htmltools::br(),
      paste0("median: ", round(s[3], 1)),
      htmltools::br(),
      paste0("Q1–Q3: ", round(s[2], 1), " – ", round(s[4], 1)),
      htmltools::br(),
      paste0("whiskers: ", round(s[1], 1), " – ", round(s[5], 1))
    )
  })
}

shinyApp(ui, server)
