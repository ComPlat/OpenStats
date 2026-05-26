env_operations_V1_2 <- new.env(parent = getNamespace("OpenStats"))

get_elem <- function(df, ...) {
  stopifnot("Expected dataframe or vector" = is.data.frame(df) || is.vector(df))
  s <- substitute(list(...))
  args <- as.list(s[-1])
  l <- length(args)
  if (l <= 0 || l > 2) {
    stop("Wrong number of arguments")
  }
  if (is.data.frame(df) && l == 1) {
    stop("To get one element from a dataframe two index arguments are required")
  }
  if (is.vector(df) && l != 1) {
    stop("To get one element from a list one indec argument is required")
  }
  if (is.data.frame(df)) {
    if (!is.numeric(args[[1]]) || !is.numeric(args[[2]])) {
      stop("The index arguments have to be of type numeric")
    }
    res <- df[args[[1]], args[[2]]]
    if (is.null(res)) stop("Cannot access the element")
    return(res)
  }
  if (is.vector(df)) {
    if (!is.numeric(args[[1]])) {
      stop("The index arguments have to be of type numeric")
    }
    res <- df[args[[1]]]
    if (is.na(res)) stop("Cannot access the element")
    return(res)
  }
}
env_operations_V1_2$get_elem <- get_elem

get_cols <- function(df, ...) {
  stopifnot("Expected dataframe" = is.data.frame(df))
  s <- substitute(list(...))
  args <- as.list(s[-1])
  stopifnot("No columns are specified" = length(args) >= 1)
  lapply(args, function(x) {
    name <- deparse(x)
    stopifnot("Column not found" = name %in% names(df))
  })
  args <- as.character(args)
  df[, args]
}
env_operations_V1_2$get_cols <- get_cols

get_rows <- function(df, expr) {
  stopifnot("Expected dataframe" = is.data.frame(df))
  subset(df, expr)
}
env_operations_V1_2$get_rows <- get_rows

# nocov start own helper
env_operations_V1_2$Mean <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  mean(x, na.rm = TRUE)
}

env_operations_V1_2$Median <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  median(x, na.rm = TRUE)
}

env_operations_V1_2$SD <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  sd(x, na.rm = TRUE)
}

env_operations_V1_2$Sum <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  sum(x, na.rm = TRUE)
}

env_operations_V1_2$Min <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  min(x, na.rm = TRUE)
}

env_operations_V1_2$Max <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  max(x, na.rm = TRUE)
}
# nocov end own helper

# TODO: for a later update keep the type of the original cols
DataFrame <- function(...) {
  columns <- list(...)
  s <- substitute(list(...))
  args <- as.list(s[-1])
  args <- lapply(args, function(x) {
    make.names(deparse(x))
  })
  sapply(columns, function(x) {
    if (length(x) == 0) stop("Found empty column")
  })
  rows <- max(sapply(columns, length))
  total_bytes <- sum(sapply(columns, function(col) {
    type <- typeof(col)
    element_size <- if (type %in% c("double", "integer", "numeric")) 8 else nchar(type) # Approximate for other types
    rows * element_size
  }))
  if (total_bytes > 10^8) {
    stop("The total size of the data frame is too large")
  }
  columns <- lapply(columns, function(col) {
    env_utils_V1_2$elongate_col(col, rows)
  })
  df <- do.call(cbind, columns) |> as.data.frame()
  names(df) <- args
  return(df)
}
env_operations_V1_2$DataFrame <- DataFrame

Seq <- function(...) {
  args <- list(...)
  start <- args[[1]]
  end <- args[[2]]
  by <- args[[3]]
  number_of_elems <- floor(abs(end - start) / by) + 1
  n_bytes <- number_of_elems * 8 # Assume that each element is a double
  if (n_bytes > 10^8) {
    stop("The size of the sequence is too large")
  }
  return(seq(start, end, by))
}
env_operations_V1_2$Seq <- Seq

# nocov start own helper
env_operations_V1_2$C <- function(...) {
  c(...)
}
env_operations_V1_2$Dnorm <- function(...) {
  dnorm(...)
}
env_operations_V1_2$Pnorm <- function(...) {
  pnorm(...)
}
env_operations_V1_2$Qnorm <- function(...) {
  qnorm(...)
}
# nocov end own helper

Rnorm <- function(...) {
  args <- list(...)
  n <- args[[1]]
  if (length(n) > 1) stop("Length of size input to Rnorm > 1")
  if (!is.numeric(n) && !is.integer(n)) {
    n <- length(n)
  }
  if (is.numeric(n) && floor(n) != n) {
    n <- floor(n)
  }
  n_bytes <- n * 8
  if (n_bytes > 10^8) {
    stop("The size of the sequence is too large")
  }
  rnorm(...)
}
env_operations_V1_2$Rnorm <- Rnorm

# nocov start own helper
env_operations_V1_2$Dbinom <- function(...) {
  dbinom(...)
}
env_operations_V1_2$Pbinom <- function(...) {
  pbinom(...)
}
env_operations_V1_2$Qbinom <- function(...) {
  qbinom(...)
}
# nocov end own helper

Rbinom <- function(...) {
  args <- list(...)
  n <- args[[1]]
  if (length(n) > 1) stop("Length of size input to Rbinom > 1")
  if (!is.numeric(n) && !is.integer(n)) {
    n <- length(n)
  }
  if (is.numeric(n) && floor(n) != n) {
    n <- floor(n)
  }
  n_bytes <- n * 8
  if (n_bytes > 10^8) {
    stop("The size of the sequence is too large")
  }
  rbinom(...)
}
env_operations_V1_2$Rbinom <- Rbinom

# nocov start own helper
env_operations_V1_2$Dpois <- function(...) {
  dpois(...)
}
env_operations_V1_2$Ppois <- function(...) {
  ppois(...)
}
# nocov end own helper

Rpois <- function(...) {
  args <- list(...)
  n <- args[[1]]
  if (length(n) > 1) stop("Length of size input to Rpois > 1")
  if (!is.numeric(n) && !is.integer(n)) {
    n <- length(n)
  }
  if (is.numeric(n) && floor(n) != n) {
    n <- floor(n)
  }
  n_bytes <- n * 8
  if (n_bytes > 10^8) {
    stop("The size of the sequence is too large")
  }
  rpois(...)
}
env_operations_V1_2$Rpois <- Rpois

# nocov start own helper
env_operations_V1_2$Dunif <- function(...) {
  dunif(...)
}
env_operations_V1_2$Punif <- function(...) {
  punif(...)
}
env_operations_V1_2$Qunif <- function(...) {
  qunif(...)
}
# nocov end own helper

Runif <- function(...) {
  args <- list(...)
  n <- args[[1]]
  if (length(n) > 1) stop("Length of size input to Runif > 1")
  if (!is.numeric(n) && !is.integer(n)) {
    n <- length(n)
  }
  if (is.numeric(n) && floor(n) != n) {
    n <- floor(n)
  }
  n_bytes <- n * 8
  if (n_bytes > 10^8) {
    stop("The size of the sequence is too large")
  }
  runif(...)
}
env_operations_V1_2$Runif <- Runif

# nocov start own helper
create_run_env <- function() {
  env <- new.env(parent = baseenv())
  env$get_elem <- env_operations_V1_2$get_elem
  env$get_rows <- env_operations_V1_2$get_rows
  env$get_cols <- env_operations_V1_2$get_cols
  env$Mean <- env_operations_V1_2$Mean
  env$SD <- env_operations_V1_2$SD
  env$Median <- env_operations_V1_2$Median
  env$Sum <- env_operations_V1_2$Sum
  env$Min <- env_operations_V1_2$Min
  env$Max <- env_operations_V1_2$Max
  env$C <- env_operations_V1_2$C
  env$Seq <- env_operations_V1_2$Seq
  env$DataFrame <- env_operations_V1_2$DataFrame
  env$as.char <- as.character
  env$as.int <- as.integer
  env$as.real <- as.numeric
  env$as.fact <- as.factor
  env$Dnorm <- env_operations_V1_2$Dnorm
  env$Pnorm <- env_operations_V1_2$Pnorm
  env$Qnorm <- env_operations_V1_2$Qnorm
  env$Rnorm <- env_operations_V1_2$Rnorm
  env$Dbinom <- env_operations_V1_2$Dbinom
  env$Pbinom <- env_operations_V1_2$Pbinom
  env$Qbinom <- env_operations_V1_2$Qbinom
  env$Rbinom <- env_operations_V1_2$Rbinom
  env$Dpois <- env_operations_V1_2$Dpois
  env$Ppois <- env_operations_V1_2$Ppois
  env$Rpois <- env_operations_V1_2$Rpois
  env$Dunif <- env_operations_V1_2$Dunif
  env$Punif <- env_operations_V1_2$Punif
  env$Qunif <- env_operations_V1_2$Qunif
  env$Runif <- env_operations_V1_2$Runif
  env
}
# nocov end own helper
