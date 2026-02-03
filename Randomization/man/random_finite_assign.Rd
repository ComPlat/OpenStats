\name{random_assign}
\alias{random_assign}
\title{Creates a Random Assignment to a Dataframe}
\usage{
random_assign(
  seed,
  groups,
  design,
  max_iter
)
}
\arguments{
  \item{seed}{A seed number to initialize the Random Number Generator}
  \item{groups}{A data.frame which contains the properties based on which the assignment should be done.}
  \item{design}{A data.frame to which the groups should be randomly aissgned}
  \item{max_iter}{An integer which determines how long the optimization should run. Default value is 50.}
}
\description{
  The desired groups are assigned to df. This is done via an optimization where one swap is conducted in each iteration.
}
\examples{
\donttest{
  n_blocks <- data.frame(treatment = rep(c("control", "one", "two"), each = 27L))
  groups <- data.frame(blood = c(rep(4000, 27L), rep(3500, 27L), rep(300, 27L)))
  random_finite_assign(42, groups, n_blocks)
}
}
