check_dose_response <- function(DataModelState) {
  if (is.null(DataModelState$df)) return("No data is available")
  if (!is.data.frame(DataModelState$df)) return("Dataset seems to be malformed")

  formula <- DataModelState$formula
  if (is.null(formula)) return("You have to define a model in the formula editor")
  if (!inherits(formula, "LinearFormula")) return("Only linear models are supported.")

  f <- formula@formula
  response <- all.vars(f[[2]])
  predictor <- all.vars(f[[3]])

  if (length(response) != 1 || length(predictor) != 1) {
    return("The formula must have one predictor and one response.")
  }

  df <- DataModelState$df
  if (!(response %in% names(df))) {
    return(sprintf("The response variable '%s' is not a column of the active dataset", response))
  }
  if (!(predictor %in% names(df))) {
    return(sprintf("The predictor variable '%s' is not a column of the active dataset", predictor))
  }
  if (!is.numeric(df[[response]])) {
    return(sprintf("The response variable '%s' must be numeric.", response))
  }

  if (!is.numeric(df[[predictor]])) {
    return(sprintf("The predictor variable '%s' must be numeric.", predictor))
  }

  return(NULL)
}
check_correlation <- check_dose_response

check_assumptions <- function(DataModelState) {
  if (is.null(DataModelState$df)) return("No data is available")
  if (!is.data.frame(DataModelState$df)) return("Dataset seems to be malformed")
  formula <- DataModelState$formula
  if (is.null(formula)) return("You have to define a model in the formula editor")
  if (inherits(DataModelState$formula, "OptimFormula")) {
    return("There are no meaningful tests for an optimization")
  }

  f <- formula@formula
  response <- all.vars(f[[2]])
  predictors <- all.vars(f[[3]])
  df <- DataModelState$df
  if (!(response %in% names(df))) {
    return(sprintf("The response variable '%s' is not a column of the active dataset", response))
  }
  for (predictor in predictors) {
    if (!(predictor %in% names(df))) {
      return(sprintf("The predictor variable '%s' is not a column of the active dataset", predictor))
    }
  }

  return(NULL)
}
check_statistical_tests <- check_assumptions

check_data_wrangling <- function(DataModelState) {
  if (is.null(DataModelState$df)) return("No data is available")
  if (!is.data.frame(DataModelState$df)) return("Dataset seems to be malformed")
  return(NULL)
}
check_visualization1 <- check_data_wrangling
check_visualization2 <- function(DataModelState) {
  if (is.null(DataModelState$df)) return("No data is available")
  if (!is.data.frame(DataModelState$df)) return("Dataset seems to be malformed")

  f <- DataModelState$formula@formula
  response <- all.vars(f[[2]])
  predictors <- all.vars(f[[3]])
  df <- DataModelState$df
  if (!(response %in% names(df))) {
    return(sprintf("The response variable '%s' is not a column of the active dataset", response))
  }
  for (predictor in predictors) {
    if (!(predictor %in% names(df))) {
      return(sprintf("The predictor variable '%s' is not a column of the active dataset", predictor))
    }
  }

  return(NULL)
}

check_history <- check_data_wrangling
