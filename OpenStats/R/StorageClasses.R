setClass("MethodDoseResponse",
  slots = c(
    id = "character",
    request_id = "character",
    element_info = "list"
  )
)
setClass("MethodVariationStatistics",
  slots = c(
    id = "character",
    request_id = "character",
    element_info = "list"
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
setClass("LinearMixedFormula",
  slots = c(
    formula = "formula"
  )
)

setClass("doseResponse",
  slots = c(
    input_df = "data.frame",
    df = "data.frame",
    p = "ANY",
    name_col = "character",
    unit_col = "character",
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

setClass("summaryDataFrame",
  slots = c(
    summary = "data.frame"
  )
)
setClass("summaryPlotDataFrame",
  slots = c(
    p = "ANY"
  )
)
setClass("diagnosticPlots",
  slots = c(
    p = "ANY",
    current_page = "integer"
  )
)

# Text representation of a result for the LLM.
# Mirrors the dispatch of build_result_box in Server_ResultsList.R
methods::setGeneric("llm_text", function(res) standardGeneric("llm_text"))

# Long tables would overflow the small context windows of local models
llm_table_text_V1_2 <- function(df, max_rows = 50L) {
  df <- as.data.frame(df)
  more <- ""
  if (nrow(df) > max_rows) {
    more <- sprintf("\n... (%d of %d rows shown)", max_rows, nrow(df))
    df <- utils::head(df, max_rows)
  }
  paste0(paste(utils::capture.output(print(df)), collapse = "\n"), more)
}

llm_format_V1_2 <- function(x) {
  format(signif(x, 4))
}

llm_plot_text_V1_2 <- function(p) {
  if (!inherits(p, "ggplot")) {
    return(paste("Plot of class", class(p)[1]))
  }
  labs <- ggplot2::get_labs(p)
  aes_names <- intersect(names(labs), c("x", "y", "colour", "fill", "shape", "linetype", "group"))
  aes_text <- paste(aes_names, unlist(labs[aes_names]), sep = ": ", collapse = ", ")
  geoms <- vapply(p$layers, function(l) sub("^Geom", "", class(l$geom)[1]), character(1))
  paste0(
    "Plot", if (!is.null(labs$title)) paste0(": ", labs$title), "\n",
    "  Layers: ", paste(unique(geoms), collapse = ", "), "\n",
    "  Mapping: ", aes_text, "\n",
    "  Data points: ", if (is.data.frame(p$data)) nrow(p$data) else "unknown"
  )
}

llm_plots_text_V1_2 <- function(plots) {
  if (inherits(plots, "ggplot")) {
    plots <- list(plots)
  }
  texts <- vapply(seq_along(plots), function(i) {
    paste0("Page ", i, ": ", llm_plot_text_V1_2(plots[[i]]))
  }, character(1))
  paste(texts, collapse = "\n")
}

llm_text_any <- function(res) {
  if (is.data.frame(res)) {
    return(llm_table_text_V1_2(res))
  }
  paste(utils::capture.output(print(res)), collapse = "\n")
}
llm_text_summary_model <- function(res) {
  paste0(
    llm_plot_text_V1_2(res@p@p), "\n\n",
    "Coefficients:\n", llm_table_text_V1_2(res@summary), "\n\n",
    "Information criteria:\n", llm_table_text_V1_2(res@information_criterions)
  )
}
llm_text_summary_data_frame <- function(res) {
  llm_table_text_V1_2(res@summary)
}
llm_text_dose_response <- function(res) {
  paste0(
    "Fitted parameters:\n", llm_table_text_V1_2(res@df), "\n\n",
    llm_plots_text_V1_2(res@p)
  )
}
llm_text_diagnostic_plots <- function(res) {
  llm_plots_text_V1_2(res@p)
}
llm_text_plot <- function(res) {
  llm_plot_text_V1_2(res@p)
}
llm_text_summary_plot_data_frame <- function(res) {
  llm_plot_text_V1_2(res@p)
}

methods::setMethod("llm_text", "ANY", llm_text_any)
methods::setMethod("llm_text", "summaryModel", llm_text_summary_model)
methods::setMethod("llm_text", "summaryDataFrame", llm_text_summary_data_frame)
methods::setMethod("llm_text", "doseResponse", llm_text_dose_response)
methods::setMethod("llm_text", "diagnosticPlots", llm_text_diagnostic_plots)
methods::setMethod("llm_text", "plot", llm_text_plot)
methods::setMethod("llm_text", "summaryPlotDataFrame", llm_text_summary_plot_data_frame)
