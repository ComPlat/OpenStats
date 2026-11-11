predictorsSidebarUI <- function(id) {
  shiny::tagList(
    shiny::h4("Add predictor"),
    shiny::textInput("PREDICTORS-predictor_name", "Name"),
    shiny::textInput("PREDICTORS-predictor_levels", "Levels (comma separated; continuous_grid: start, end, step; continuous: min, max)"),
    shiny::selectInput("PREDICTORS-predictor_type", "Type", choices = PREDICTOR_TYPES, selected = "categorical"),
    shiny::actionButton("PREDICTORS-add_predictor", "+"),

    shiny::tags$hr(),

    shiny::h4("Update predictor"),
    shiny::selectInput("PREDICTORS-update_predictor_select", "Predictor", choices = NULL),
    shiny::textInput("PREDICTORS-update_predictor_levels", "New levels (comma separated; continuous_grid: start, end, step; continuous: min, max)"),
    shiny::selectInput("PREDICTORS-update_predictor_type", "Type", choices = PREDICTOR_TYPES, selected = "categorical"),
    shiny::actionButton("PREDICTORS-update_predictor", "Update"),

    shiny::tags$hr(),

    shiny::h4("Remove predictor"),
    shiny::selectInput("PREDICTORS-remove_predictor_select", "Predictor", choices = NULL),
    shiny::actionButton("PREDICTORS-remove_predictor", "Remove")
  )
}

predictorsMainUI <- function(id) {
  shiny::tagList(
    shiny::h4("Predictors (preview)"),
    shiny::uiOutput("PREDICTORS-predictors_box")
  )
}
