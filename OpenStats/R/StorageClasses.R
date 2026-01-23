setClass("MethodDoseResponse",
  slots = c(
    id = "character",
    request_id = "character"
  )
)

setClass("LinearFormula",
  slots = c(
    formula = "formula"
  )
)
setClass("GeneralisedLinearFormula",
  slots = c(
    formula = "formula",
    family = "character",
    link_fct = "character"
  )
)
setClass("OptimFormula",
  slots = c(
    formula = "formula",
    parameter = "character",
    lhs = "character",
    rhs = "call",
    method = "character",
    lower = "numeric",
    upper = "numeric",
    seed = "numeric"
  )
)

setClass("doseResponse",
  slots = c(
    input_df = "data.frame",
    df = "data.frame",
    p = "ANY",
    name_col = "character",
    formula = "formula",
    xTransform = "logical",
    yTransform = "logical",
    current_page = "integer"
  )
)

setClass("plot",
  slots = c(
    p = "ANY",
    width = "numeric",
    height = "numeric",
    resolution = "numeric"
  )
)
setClass("summaryModel",
  slots = c(
    p = "plot",
    summary = "data.frame",
    information_criterions = "data.frame"
  )
)
setClass("optimResult",
  slots = c(
    parameter = "numeric",
    error = "numeric",
    convergence = "logical",
    message = "character",
    predicted_df = "data.frame",
    x_vars = "character"
  )
)
