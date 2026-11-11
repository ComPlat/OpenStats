setClass("sampleSizeResult",
  slots = c(
    n = "numeric"
  )
)

setClass("assignmentResult",
  slots = c(
    df = "data.frame"
  )
)

setClass("designResult",
  slots = c(
    df = "data.frame"
  )
)

setClass("finiteAssignmentResult",
  slots = c(
    df = "data.frame",
    loss = "numeric"
  )
)

setClass("importedData",
  slots = c(
    df = "data.frame"
  )
)

setClass("boSuggestionResult",
  slots = c(
    df = "data.frame"
  )
)

df_result_classes <- c(
  "assignmentResult", "designResult", "finiteAssignmentResult", "importedData",
  "boSuggestionResult"
)
