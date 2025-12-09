\name{random_assign}
\alias{random_assign}
\title{Creates a Random Assignment to a Dataframe}
\usage{
random_assign(
  df,
  groups,
  ratios,
  col,
  block_col,
  strata_cols,
  randomization_method,
  seed
)
}
\arguments{
  \item{df}{A data.frame to which the groups should be randomly aissgned}
  \item{groups}{A character vector containing the groups which should be assigned.}
  \item{ratios}{A numeric vector defining the ratios of the groups}
  \item{col}{A character defining the column(name) to which the groups should be assigned}
  \item{block_col}{In case the randomization_method is "block", or "balanced", you have to pass a character vector containing the column defining the blocks.}
  \item{strata_cols}{In case the randomization_method is "block_stratified", you have to pass a character vector containing the column(s) for which the assignment should be stratified}
  \item{randomization_method}{one of these options: "simple", "block", "block_stratified", or "balanced"}
  \item{seed}{A seed number to initialize the Random Number Generator}
}
\description{
  The desired groups are assigned to df using different randomization methods.
}
\examples{
\donttest{
design <- agricolae::design.rcbd(
  trt = c("ctrl", "A", "B", "C"), r = 5
)$book
groups <- c("Day1", "Day2")
ratios <- c(1, 1)
col <- "Group"
Randomization::random_assign(
  design,
  groups,
  "infinite",
  ratios,
  col,
  randomization_method = "simple",
  seed = 1234
)
}
}
