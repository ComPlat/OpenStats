\name{random_finite_assign}
\alias{random_finite_assign}
\title{Random Assignment of a Finite Population to a Design via Optimization}

\usage{
random_finite_assign(
  seed,
  groups,
  design,
  max_iter = 50L,
  ridge = 1e-8,
  loss_function = "Default",
  verbose = TRUE,
  ids = NULL
)
}

\arguments{
  \item{seed}{
    A single numeric value used to initialize the random number generator.
  }

  \item{groups}{
    A \code{data.frame} containing the finite population to be assigned.
    The columns represent covariates used for balancing during optimization.
  }

  \item{design}{
    A \code{data.frame} defining the experimental design (e.g. treatment
    structure or block structure). Units from \code{groups} are assigned
    to the rows of this design.
  }

  \item{max_iter}{
    Integer. Maximum number of swap-iterations used in the greedy
    optimization procedure. Default is \code{50L}.
  }

  \item{ridge}{
    Numeric value used as ridge regularization (e.g. in Mahalanobis-based
    loss functions). Default is \code{1e-8}.
  }

  \item{loss_function}{
    Character string specifying the loss function used for balancing.
    One of \code{"Default"} or \code{"Mahalanobis"}.
  }

  \item{verbose}{
    Logical. If \code{TRUE}, prints progress information during optimization.
    Default is \code{TRUE}.
  }

  \item{ids}{
    Optional vector of unique identifiers for the units in \code{groups}.
    Must have length \code{nrow(groups)}. If provided, the returned object
    contains these identifiers in permuted order.
  }
}

\description{
  Performs an optimization-based random assignment of a finite set of
  experimental units (given in \code{groups}) to a predefined experimental
  design (\code{design}).

  The assignment is initialized randomly and iteratively improved using
  a greedy swap-based optimization procedure. In each iteration, potential
  swaps are evaluated according to the chosen loss function.

  If \code{ids} are supplied, they are carried through the permutation
  and returned alongside the assignment.
}

\value{
  A list containing the optimized assignment, including:
  \itemize{
    \item \code{assigned}: The design data frame with assigned units.
    \item \code{perm}: The final permutation (row indices of \code{groups}).
    \item \code{unit_index}: Row indices of \code{groups} in assigned order.
    \item \code{unit_id}: The corresponding identifiers (if \code{ids} was provided).
    \item \code{blocks}: The block structure used during optimization.
    \item \code{UNUSED}: Units not assigned (if the population size exceeds
      the number of required design rows).
  }
}

\examples{
\donttest{
  # Define experimental design
  design <- data.frame(
    treatment = rep(c("control", "one", "two"), each = 10L)
  )

  # Finite population with covariate and explicit ID
  groups <- data.frame(
    weight = rnorm(30, mean = 300, sd = 25)
  )

  animal_ids <- paste0("Animal_", seq_len(nrow(groups)))

  res <- random_finite_assign(
    seed = 42,
    groups = groups,
    design = design,
    ids = animal_ids,
    loss_function = "Default"
  )

  head(res$assigned)
  head(res$unit_id)
}
}
