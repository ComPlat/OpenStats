OperatorEditorSidebar <- function(id) {
  ui <- fluidPage(
    div(
      h3("Variables"),
      uiOutput(NS(id, "colnames_list")),
      class = "boxed-output"
    ),
    br(),
    div(
      h3("Arithmetic Operators"),
      actionButton(NS(id, "add"), "+", class = "add-button"),
      actionButton(NS(id, "sub"), "-", class = "add-button"),
      actionButton(NS(id, "mul"), "*", class = "add-button"),
      actionButton(NS(id, "div"), "/", class = "add-button"),
      actionButton(NS(id, "bracket_open"), "(", class = "add-button"),
      actionButton(NS(id, "bracket_close"), ")", class = "add-button"),
      actionButton(NS(id, "comma"), ",", class = "add-button"),
      class = "boxed-output"
    ),
    div(
      h3("Math Functions"),
      actionButton(NS(id, "log"), "log", class = "add-button", title = "logarithm to the base e"),
      actionButton(NS(id, "log10"), "log10", class = "add-button", title = "logarithm to the base 10"),
      actionButton(NS(id, "sqrt"), "sqrt", class = "add-button", title = "Square root"),
      actionButton(NS(id, "exp"), "exp", class = "add-button", title = "Exponential function (e^x) equate to exp(x)"),
      actionButton(NS(id, "exponent"), "^", class = "add-button"),
      actionButton(NS(id, "sin"), "sin", class = "add-button"),
      actionButton(NS(id, "cos"), "cos", class = "add-button"),
      actionButton(NS(id, "tan"), "tan", class = "add-button"),
      actionButton(NS(id, "sinh"), "sinh", class = "add-button"),
      actionButton(NS(id, "cosh"), "cosh", class = "add-button"),
      actionButton(NS(id, "tanh"), "tanh", class = "add-button"),
      actionButton(NS(id, "asin"), "asin", class = "add-button"),
      actionButton(NS(id, "acos"), "acos", class = "add-button"),
      actionButton(NS(id, "atan"), "atan", class = "add-button"),
      actionButton(NS(id, "abs"), "abs", class = "add-button", title = "absolute value |x| equivalent to abs(x)"),
      actionButton(NS(id, "ceil"), "ceiling", class = "add-button", title = "Round up to the nearest integer."),
      actionButton(NS(id, "floor"), "floor", class = "add-button", title = "Round down to the nearest integer"),
      actionButton(NS(id, "trunc"), "trunc", class = "add-button", title = "Truncate decimal part, keeping only the integer part"),
      actionButton(NS(id, "round"), "round", class = "add-button", title = "Round to the nearest integer or specified precision"),
      class = "boxed-output"
    ),
    div(
      h3("Comparison Operators"),
      actionButton(NS(id, "larger"), ">", class = "add-button", title = "Greater than (e.g., 5 > 3 results in TRUE, '5 is greater than 3')"),
      actionButton(NS(id, "smaller"), "<", class = "add-button", title = "Less than (e.g., 3 < 5 results in TRUE, '3 is less than 5')"),
      actionButton(NS(id, "larger_eq"), ">=", class = "add-button", title = "Greater than or equal to (e.g., 5 >= 5 results in TRUE, '5 is greater than or equal to 5')"),
      actionButton(NS(id, "smaller_eq"), "<=", class = "add-button", title = "Less than or equal to (e.g., 3 <= 5 results in TRUE, '3 is less than or equal to 5')"),
      actionButton(NS(id, "eq"), "==", class = "add-button", title = "Equal to (e.g., 5 == 5 results in TRUE, '5 is equal to 5')"),
      actionButton(NS(id, "not_eq"), "!=", class = "add-button", title = "Not equal to (e.g., 5 != 3 results in TRUE, '5 is not equal to 3')"),
      class = "boxed-output"
    ),
    div(
      h3("Statistical & Utils Functions"),
      actionButton(NS(id, "mean"), "Mean", class = "add-button", title = "Calculate the average of numbers (e.g., Mean(ColName))"),
      actionButton(NS(id, "sd"), "standard deviation", class = "add-button", title = "SD(ColName) gives the standard deviation"),
      actionButton(NS(id, "median"), "Median", class = "add-button", title = "Median(ColName) calculates the median)"),
      actionButton(NS(id, "sum"), "Sum", class = "add-button", title = "Add up all the numbers Sum(ColName)"),
      actionButton(NS(id, "min"), "Min", class = "add-button", title = "Find the smallest number (e.g., Min(c(1, 2, 3)) gives 1)"),
      actionButton(NS(id, "max"), "Max", class = "add-button", title = "Find the largest number (e.g., Max(c(1, 2, 3)) gives 3)"),
      actionButton(NS(id, "c"), "concatenate", class = "add-button", title = "Combine values into a list (e.g., C(1, 2, 3) gives [1, 2, 3])"),
      actionButton(NS(id, "seq"), "sequence", class = "add-button", title = "Create a sequence of elements (e.g. Seq(1, 10, 0.1) which creates a sequence starting from 1 to 10 in steps of 0.1)."),
      actionButton(NS(id, "df"), "DataFrame", class = "add-button", title = "Create a table (e.g. DataFrame(Variable1, Variable2))"),
      actionButton(NS(id, "get_elem"), "get one element", class = "add-button",
      title = "Extract one element from a variable. This can either be ColName or a tabular dataset. In case it is a ColName the syntax is get_elem(ColName, idx) where idx is an integer number e.g. 1. In case one specific element of a dataset should be retrieved the syntax is get_elem(df, idx_row, idx_col). Again idx_row and idx_col have to be integers. The first one specifies the row number and the second one the column number."),
      actionButton(NS(id, "get_rows"), "get_rows", class = "add-button",
        title = 'Filter by row. For example get_rows(df, ColName == "Control") or get_rows(df, colName == 10)'),
      actionButton(NS(id, "get_cols"), "get_cols", class = "add-button",
        title = 'Extract column from a data frame (a table).
        For example get_cols(df, ColName) or get_cols(df, ColName1, ColName2)'),
      class = "boxed-output"
    ),
    div(
      h3("String Functions"),
      actionButton(NS(id, "paste"), "paste", class = "add-button", title = "Join pieces of text (e.g., paste('Hello', 'World') gives 'Hello World')."),
      actionButton(NS(id, "paste0"), "paste0", class = "add-button", title = "Join pieces of text without spaces (e.g., paste0('Hello', 'World') gives 'HelloWorld'). This is very practical if you want to join two columns e.g. paste0(ColName1, ColName2)"),
      actionButton(NS(id, "tolower"), "tolower", class = "add-button", title = "Convert text to lowercase (e.g., tolower('Hello') gives 'hello')"),
      actionButton(NS(id, "toupper"), "toupper", class = "add-button", title = "Convert text to uppercase (e.g., toupper('hello') gives 'HELLO')"),
      class = "boxed-output"
    ),
    div(
      h3("Convert types of columns"),
      actionButton(NS(id, "as_char"), "convert to character",
        title = "Convert a column of the dataset or an intermediate variable to character. For example as.char(ColName)",
        class = "add-button"),
      actionButton(NS(id, "as_int"), "convert to integer",
        title = "Convert a column of the dataset or an intermediate variable to integer. For example as.int(ColName)",
        class = "add-button"),
      actionButton(NS(id, "as_real"), "convert to real number",
        title = "Convert a column of the dataset or an intermediate variable to a real number. For example as.real(ColName)",
        class = "add-button"),
      actionButton(NS(id, "as_fact"), "convert to factors",
        title = "Convert a column of the dataset or an intermediate variable to a factor. For example as.fact(ColName)",
        class = "add-button"),
      class = "boxed-output"
    ),
    div(
      h3("Random number functions"),
      actionButton(NS(id, "dnorm"), "Dnorm", class = "add-button",
        title = "Probability density function for normal distribution (e.g., Dnorm(0) gives the height of the normal curve at 0)"),
      actionButton(NS(id, "pnorm"), "Pnorm", class = "add-button",
        title = "Cumulative distribution function for normal distribution (e.g., Pnorm(1) gives the probability that a random variable is less than 1)"),
      actionButton(NS(id, "qnorm"), "Qnorm", class = "add-button",
        title = "Quantile function for normal distribution (e.g., Qnorm(0.95) gives the value corresponding to the 95th percentile)"),
      actionButton(NS(id, "rnorm"), "Rnorm", class = "add-button",
        title = "Generate random numbers from a normal distribution (e.g., Rnorm(5) gives 5 random numbers from a normal distribution)"),
      actionButton(NS(id, "dbinom"), "Dbinom", class = "add-button",
        title = "Probability mass function for binomial distribution (e.g., Dbinom(2, 5, 0.5) gives the probability of getting exactly 2 successes in 5 trials with probability 0.5)"),
      actionButton(NS(id, "pbinom"), "Pbinom", class = "add-button",
        title = "Cumulative distribution function for binomial distribution (e.g., Pbinom(2, 5, 0.5) gives the probability of getting 2 or fewer successes in 5 trials)"),
      actionButton(NS(id, "qbinom"), "Qbinom", class = "add-button",
        title = "Quantile function for binomial distribution (e.g., Qbinom(0.95, 5, 0.5) gives the number of successes corresponding to the 95th percentile)"),
      actionButton(NS(id, "rbinom"), "Rbinom", class = "add-button",
        title = "Generate random numbers from a binomial distribution (e.g., Rbinom(5, 10, 0.5) gives 5 random binomial values with 10 trials and probability 0.5)"),
      actionButton(NS(id, "dpois"), "Dpois", class = "add-button",
        title = "Probability mass function for Poisson distribution (e.g., Dpois(3, 2) gives the probability of getting exactly 3 events when the average is 2)"),
      actionButton(NS(id, "ppois"), "Ppois", class = "add-button",
        title = "Cumulative distribution function for Poisson distribution (e.g., Ppois(3, 2) gives the probability of getting 3 or fewer events when the average is 2)"),
      actionButton(NS(id, "rpois"), "Rpois", class = "add-button",
        title = "Generate random numbers from a Poisson distribution (e.g., Rpois(5, 2) gives 5 random Poisson values with mean 2)"),
      actionButton(NS(id, "dunif"), "Dunif", class = "add-button",
        title = "Probability density function for uniform distribution (e.g., Dunif(0.5, min = 0, max = 1) gives the height of the uniform distribution at 0.5)"),
      actionButton(NS(id, "punif"), "Punif", class = "add-button",
        title = "Cumulative distribution function for uniform distribution (e.g., Punif(0.5, min = 0, max = 1) gives the probability of getting a value less than or equal to 0.5)"),
      actionButton(NS(id, "qunif"), "Qunif", class = "add-button",
        title = "Quantile function for uniform distribution (e.g., Qunif(0.95, min = 0, max = 1) gives the value corresponding to the 95th percentile)"),
      actionButton(NS(id, "runif"), "Runif", class = "add-button",
        title = "Generate random numbers from a uniform distribution (e.g., Runif(5, min = 0, max = 1) gives 5 random values between 0 and 1)"),

      class = "boxed-output"
    )
  )
}

OperatorEditorUI <- function(id) {
  ui <- fluidPage(
    div(
      textAreaInput(NS(id, "editable_code"), "Operation:", value = "", rows = 12),
      class = "boxed-output"
    ),
    br(),
    div(
      class = "boxed-output",
      fluidRow(
        column(
          7,
          actionButton(NS(id, "run_op_intermediate"), "Run operation and store intermediate results"),

        ),
        column(
          4,
          textInput(NS(id, "iv"), "Intermediate variable name:", value = "")
        )
      )
    ),
    br(),
    div(
      class = "boxed-output",
      fluidRow(

        column(
          7,
          actionButton(NS(id, "run_op"), "Run operation and append to dataset")
        ),
        column(
          4,
          textInput(NS(id, "nc"), "New column name:", value = "")
        )
      )
    ),
    uiOutput(NS(id, "head")),
    uiOutput(NS(id, "intermediate_results"))
  )
}
