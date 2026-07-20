predictorsSidebarUI <- function(id) {
  shiny::tagList(
    shiny::h4("Add predictor"),
    shiny::textInput("PREDICTORS-predictor_name", "Name"),
    shiny::textInput("PREDICTORS-predictor_levels", "Levels (comma separated)"),
    shiny::actionButton("PREDICTORS-add_predictor", "+"),

    shiny::tags$hr(),

    shiny::h4("Update predictor"),
    shiny::selectInput("PREDICTORS-update_predictor_select", "Predictor", choices = NULL),
    shiny::textInput("PREDICTORS-update_predictor_levels", "New levels (comma separated)"),
    shiny::actionButton("PREDICTORS-update_predictor", "Update"),

    shiny::tags$hr(),

    shiny::h4("Remove predictor"),
    shiny::selectInput("PREDICTORS-remove_predictor_select", "Predictor", choices = NULL),
    shiny::actionButton("PREDICTORS-remove_predictor", "Remove"),

    shiny::tags$hr(),

    shiny::actionButton("PREDICTORS-add_to_results", "Add predictor table to results")
  )
}

predictorsMainUI <- function(id) {
  shiny::tagList(
    shiny::h4("Predictors (preview)"),
    shiny::uiOutput("PREDICTORS-predictors_box")
  )
}
