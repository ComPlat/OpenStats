# power: power, power_se, power_by_effect of the Monte Carlo runs; empty for closed form.
setClass("sampleSizeResult",
  slots = c(
    n = "numeric",
    power = "list"
  ),
  prototype = list(power = list())
)

sample_size_power <- function(res) {
  power <- res[intersect(c("power", "power_se", "power_by_effect"), names(res))]
  if (!is.null(power$power_by_effect)) power$power_by_effect <- as.list(power$power_by_effect)
  power
}

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
