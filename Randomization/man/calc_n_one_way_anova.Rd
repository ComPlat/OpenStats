\name{calc_n_one_way_anova}
\alias{calc_n_one_way_anova}
\title{Calculates the sample size using power analysis}
\usage{
randomization_one_way_anova(
  predictors,
  primary_factor,
  cohens_f,
  sig_level,
  desired_power
)
}
\arguments{
  \item{predictors}{A named list which contains all predictors.}
  \item{primary_factor}{a character vector defining the name of the predictor which is used in the power analysis.}
  \item{cohens_f}{cohens_f}
  \item{sig_level}{The significance level which should be used in the power analysis}
  \item{desired_power}{desired_power}
}
\description{
  A power analysis (for a one way anova) is conducted for the defined predictors. Thereby, the number of repetions per predictor level is calculated.
}
\examples{
\donttest{

predictors <- list(
    cellLines = c("HeLa", "Hek"),
    Treatment = LETTERS[1:4]
    )
res <- Randomization::calc_n_one_way_anova(
  predictors = predictors,
  primary_factor = "Treatment",
  cohens_f = 0.25,
  sig_level = 0.05,
  desired_power = 0.8
)
}
}
