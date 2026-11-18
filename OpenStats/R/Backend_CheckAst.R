env_check_ast_V1_2 <- new.env(parent = getNamespace("OpenStats"))

env_check_ast_V1_2$allowed_fcts <- function() {
  c(
    "-", "+", "*", "/", "(",
    "log", "log10", "sqrt", "exp", "^",
    "sin", "cos", "tan", "tanh", "sinh", "cosh", "acos", "asin", "atan",
    "as.numeric", "as.character", "as.logical", "as.factor", "as.integer",
    ">", "<", "<=", ">=", "==", "!=",
    "abs", "ceiling", "floor", "trunc", "round",
    "paste", "paste0", "tolower", "toupper",
    "Dnorm", "Pnorm", "Qnorm", "Rnorm", "Dbinom",
    "Pbinom", "Qbinom", "Rbinom", "Dpois",
    "Ppois", "Rpois", "Dunif", "Punif", "Qunif", "Runif",
    "Mean", "SD", "Median", "quantile", "range",
    "Sum", "diff", "Min", "Max", "scale",
    "C", "Seq", "DataFrame", "vector", "length", "matrix", "~",
    "get_rows", "get_cols", "get_elem",
    "as.char", "as.int", "as.real", "as.fact", "|", ":",
    "I", "poly"
  )
}

# Stricter than check_ast, which is shared with the data wrangling:
# - x^2 outside of I() means interaction expansion and collapses silently to x
# - functions which merge values (round, abs, ...) are not allowed, see formula_functions()
# Only for the right side, the left side is evaluated normally
env_check_ast_V1_2$check_formula_rhs <- function(rhs, inside_I = FALSE) {
  if (!is.call(rhs)) {
    return(invisible(TRUE))
  }
  fct <- deparse(rhs[[1]])
  if (fct == "^" && !inside_I) {
    stop("In a formula '^' does not compute a power. Use I(x^2) for a quadratic term.")
  }
  formula_ops <- c("+", "-", "*", ":", "|", "(")
  arithmetic_ops <- c("+", "-", "*", "^", "(")
  allowed <- c(formula_functions(), if (inside_I) arithmetic_ops else formula_ops)
  if (!fct %in% allowed) {
    stop(
      "The function '", fct, "' is not allowed in a formula. ",
      "Create the transformed column in the DataWrangling tab instead."
    )
  }
  for (x in as.list(rhs)[-1]) {
    env_check_ast_V1_2$check_formula_rhs(x, inside_I || fct == "I")
  }
  invisible(TRUE)
}

env_check_ast_V1_2$check_ast <- function(inp, allowed_variables) {
  if (!is.call(inp)) {
    return(inp)
  }
  inp <- as.list(inp)
  # check fct
  fct <- inp[[1]]
  check <- deparse(fct)
  if ((check %in% env_check_ast_V1_2$allowed_fcts()) == FALSE) {
    stop(paste("Found unallowed function: ", check))
  }
  # check variables
  lapply(inp, function(x) {
    if (!is.list(x) && is.symbol(x) && !(deparse(x) %in% env_check_ast_V1_2$allowed_fcts())) {
      if (!(deparse(x) %in% allowed_variables)) {
        stop(paste0("Found unknown variable:", deparse(x)))
      }
    }
  })
  lapply(inp, function(x) {
    env_check_ast_V1_2$check_ast(x, allowed_variables)
  })
}
