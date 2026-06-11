library(ggplot2)
df <- CO2
column_by <- c("Treatment", "Type", "conc")
for_which <- "uptake"

sp <- function(df, column_by, for_which) {

  summary_plot <- function(df, for_which, column_by) {
    df$INTERACTION_INTERNALLY_ <- interaction(df[, column_by])
    ggplot2::ggplot(
      df, ggplot2::aes(y = .data[[for_which]], x = INTERACTION_INTERNALLY_)
    ) +
      ggplot2::geom_boxplot() +
      ggplot2::xlab("") +
      ggplot2::theme(text = ggplot2::element_text(size = 12, angle = 90))
  }
  res <- lapply(for_which, function(i) {
    summary_plot(df, i, column_by)
  })
  p <- cowplot::plot_grid(plotlist = res, nrow = length(res))
  p
}
sp(df, column_by, for_which)
