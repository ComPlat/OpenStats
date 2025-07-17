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
    lower = "numeric",
    upper = "numeric",
    seed = "numeric"
  )
)

setClass("doseResponse",
  slots = c(
    df = "data.frame",
    p = "ANY",
    outlier_info = "character"
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
setClass("diagnosticPlot",
  slots = c(
    p = "character"
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
