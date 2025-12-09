\name{randomization_one_way_anova}
\alias{randomization_one_way_anova}
\title{Creates Design Matrix and Conduct Random Assignments}
\usage{
randomization_one_way_anova(
  predictors,
  primary_factor,
  groups,
  ratios,
  cohens_f,
  sig_level,
  desired_power,
  randomization_method,
  strata_cols,
  seed
)
}
\arguments{
  \item{predictors}{A named list which contains all predictors.}
  \item{primary_factor}{a character vector defining the name of the predictor which is used in the power analysis.}
  \item{groups}{A character vector containing the groups which should be assigned.}
  \item{ratios}{A numeric vector defining the ratios of the groups}
  \item{cohens_f}{cohens_f}
  \item{sig_level}{The significance level which should be used in the power analysis}
  \item{desired_power}{desired_power}
  \item{randomization_method}{one of these options: "simple", "block", "block_stratified", or "balanced"}
  \item{strata_cols}{In case the randomization_method is "block_stratified" one has to pass a character vector containing the predictors for which the assignment should be stratified}
  \item{seed}{A seed number to initialize the Random Number Generator}
}
\description{
  A power analysis (for a one way anova) is conducted for the defined predictors. Thereby, the number of repetions per predictor level is calculated.
  Afterwards, the design matrix is created and the desired groups are assigned to it.
}
\examples{
\donttest{
predictors <- list(
  cellLines = c("HeLa", "Hek"),
  Treatment = LETTERS[1:4]
)
res <- randomization_one_way_anova(
  predictors, "Treatment",
  groups = c("Day1", "Day2", "Day3"),
  "infinite",
  ratios = c(1.1, 1, 2.32),
  0.25, 0.05, 0.8,
  randomization_method = "simple",
  seed = 42
)
res <- randomization_one_way_anova(
  predictors, "Treatment",
  groups = c("Day1", "Day2", "Day3"), ratios = c(1, 1, 2),
  "infinite",
  0.25, 0.05, 0.8,
  randomization_method = "block",
  seed = 42
)
res <- randomization_one_way_anova(
  predictors,
  primary_factor = "Treatment",
  groups = c("Day1", "Day2", "Day3"),
  "infinite",
  ratios = c(1, 1, 1),
  cohens_f = 0.25,
  sig_level = 0.05,
  desired_power = 0.8,
  randomization_method = "block_stratified",
  strata_cols = c("cellLines", "Treatment"),
  seed = 42
)
}
}
