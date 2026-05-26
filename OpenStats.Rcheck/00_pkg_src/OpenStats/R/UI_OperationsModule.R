OperatorEditorSidebar <- function(id) {
  ui <- shiny::fluidPage(
    htmltools::div(
      htmltools::h3("Variables"),
      shiny::uiOutput(shiny::NS(id, "colnames_list")),
      shiny::uiOutput(shiny::NS(id, "column_apply")),
      class = "boxed-output"
    ),
    htmltools::div(
      htmltools::h3("Apply by groups"),
      shiny::uiOutput(shiny::NS(id, "column_apply")),
      class = "boxed-output"
    ),
    htmltools::br(),
    htmltools::div(
      htmltools::h3("Arithmetic Operators"),
      shiny::actionButton(shiny::NS(id, "add"), "+", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "sub"), "-", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "mul"), "*", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "div"), "/", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "bracket_open"), "(", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "bracket_close"), ")", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "comma"), ",", class = "add-button"),
      class = "boxed-output"
    ),
    htmltools::div(
      htmltools::h3("Math Functions"),
      shiny::actionButton(shiny::NS(id, "log"), "log", class = "add-button", title = "logarithm to the base e"),
      shiny::actionButton(shiny::NS(id, "log10"), "log10", class = "add-button", title = "logarithm to the base 10"),
      shiny::actionButton(shiny::NS(id, "sqrt"), "sqrt", class = "add-button", title = "Square root"),
      shiny::actionButton(shiny::NS(id, "exp"), "exp", class = "add-button", title = "Exponential function (e^x) equate to exp(x)"),
      shiny::actionButton(shiny::NS(id, "exponent"), "^", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "sin"), "sin", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "cos"), "cos", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "tan"), "tan", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "sinh"), "sinh", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "cosh"), "cosh", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "tanh"), "tanh", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "asin"), "asin", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "acos"), "acos", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "atan"), "atan", class = "add-button"),
      shiny::actionButton(shiny::NS(id, "abs"), "abs", class = "add-button", title = "absolute value |x| equivalent to abs(x)"),
      shiny::actionButton(shiny::NS(id, "ceil"), "ceiling", class = "add-button", title = "Round up to the nearest integer."),
      shiny::actionButton(shiny::NS(id, "floor"), "floor", class = "add-button", title = "Round down to the nearest integer"),
      shiny::actionButton(shiny::NS(id, "trunc"), "trunc", class = "add-button", title = "Truncate decimal part, keeping only the integer part"),
      shiny::actionButton(shiny::NS(id, "round"), "round", class = "add-button", title = "Round to the nearest integer or specified precision"),
      class = "boxed-output"
    ),
    htmltools::div(
      htmltools::h3("Comparison Operators"),
      shiny::actionButton(shiny::NS(id, "larger"), ">", class = "add-button", title = "Greater than (e.g., 5 > 3 results in TRUE, '5 is greater than 3')"),
      shiny::actionButton(shiny::NS(id, "smaller"), "<", class = "add-button", title = "Less than (e.g., 3 < 5 results in TRUE, '3 is less than 5')"),
      shiny::actionButton(shiny::NS(id, "larger_eq"), ">=", class = "add-button", title = "Greater than or equal to (e.g., 5 >= 5 results in TRUE, '5 is greater than or equal to 5')"),
      shiny::actionButton(shiny::NS(id, "smaller_eq"), "<=", class = "add-button", title = "Less than or equal to (e.g., 3 <= 5 results in TRUE, '3 is less than or equal to 5')"),
      shiny::actionButton(shiny::NS(id, "eq"), "==", class = "add-button", title = "Equal to (e.g., 5 == 5 results in TRUE, '5 is equal to 5')"),
      shiny::actionButton(shiny::NS(id, "not_eq"), "!=", class = "add-button", title = "Not equal to (e.g., 5 != 3 results in TRUE, '5 is not equal to 3')"),
      class = "boxed-output"
    ),
    htmltools::div(
      htmltools::h3("Statistical & Utils Functions"),
      shiny::actionButton(shiny::NS(id, "mean"), "Mean", class = "add-button", title = "Calculate the average of numbers (e.g., Mean(ColName))"),
      shiny::actionButton(shiny::NS(id, "sd"), "standard deviation", class = "add-button", title = "SD(ColName) gives the standard deviation"),
      shiny::actionButton(shiny::NS(id, "median"), "Median", class = "add-button", title = "Median(ColName) calculates the median)"),
      shiny::actionButton(shiny::NS(id, "sum"), "Sum", class = "add-button", title = "Add up all the numbers Sum(ColName)"),
      shiny::actionButton(shiny::NS(id, "min"), "Min", class = "add-button", title = "Find the smallest number (e.g., Min(c(1, 2, 3)) gives 1)"),
      shiny::actionButton(shiny::NS(id, "max"), "Max", class = "add-button", title = "Find the largest number (e.g., Max(c(1, 2, 3)) gives 3)"),
      shiny::actionButton(shiny::NS(id, "c"), "concatenate", class = "add-button", title = "Combine values into a list (e.g., C(1, 2, 3) gives [1, 2, 3])"),
      shiny::actionButton(shiny::NS(id, "seq"), "sequence", class = "add-button", title = "Create a sequence of elements (e.g. Seq(1, 10, 0.1) which creates a sequence starting from 1 to 10 in steps of 0.1)."),
      shiny::actionButton(shiny::NS(id, "df"), "DataFrame", class = "add-button", title = "Create a table (e.g. DataFrame(Variable1, Variable2))"),
      shiny::actionButton(shiny::NS(id, "get_elem"), "get one element", class = "add-button",
      title = "Extract one element from a variable. This can either be ColName or a tabular dataset. In case it is a ColName the syntax is get_elem(ColName, idx) where idx is an integer number e.g. 1. In case one specific element of a dataset should be retrieved the syntax is get_elem(df, idx_row, idx_col). Again idx_row and idx_col have to be integers. The first one specifies the row number and the second one the column number."),
      shiny::actionButton(shiny::NS(id, "get_rows"), "get_rows", class = "add-button",
        title = 'Filter by row. For example get_rows(df, ColName == "Control") or get_rows(df, colName == 10)'),
      shiny::actionButton(shiny::NS(id, "get_cols"), "get_cols", class = "add-button",
        title = 'Extract column from a data frame (a table).
        For example get_cols(df, ColName) or get_cols(df, ColName1, ColName2)'),
      class = "boxed-output"
    ),
    htmltools::div(
      htmltools::h3("String Functions"),
      shiny::actionButton(shiny::NS(id, "paste"), "paste", class = "add-button", title = "Join pieces of text (e.g., paste('Hello', 'World') gives 'Hello World')."),
      shiny::actionButton(shiny::NS(id, "paste0"), "paste0", class = "add-button", title = "Join pieces of text without spaces (e.g., paste0('Hello', 'World') gives 'HelloWorld'). This is very practical if you want to join two columns e.g. paste0(ColName1, ColName2)"),
      shiny::actionButton(shiny::NS(id, "tolower"), "tolower", class = "add-button", title = "Convert text to lowercase (e.g., tolower('Hello') gives 'hello')"),
      shiny::actionButton(shiny::NS(id, "toupper"), "toupper", class = "add-button", title = "Convert text to uppercase (e.g., toupper('hello') gives 'HELLO')"),
      class = "boxed-output"
    ),
    htmltools::div(
      htmltools::h3("Convert types of columns"),
      shiny::actionButton(shiny::NS(id, "as_char"), "convert to character",
        title = "Convert a column of the dataset or an intermediate variable to character. For example as.char(ColName)",
        class = "add-button"),
      shiny::actionButton(shiny::NS(id, "as_int"), "convert to integer",
        title = "Convert a column of the dataset or an intermediate variable to integer. For example as.int(ColName)",
        class = "add-button"),
      shiny::actionButton(shiny::NS(id, "as_real"), "convert to real number",
        title = "Convert a column of the dataset or an intermediate variable to a real number. For example as.real(ColName)",
        class = "add-button"),
      shiny::actionButton(shiny::NS(id, "as_fact"), "convert to factors",
        title = "Convert a column of the dataset or an intermediate variable to a factor. For example as.fact(ColName)",
        class = "add-button"),
      class = "boxed-output"
    ),
    htmltools::div(
      htmltools::h3("Random number functions"),
      shiny::actionButton(shiny::NS(id, "dnorm"), "Dnorm", class = "add-button",
        title = "Probability density function for normal distribution (e.g., Dnorm(0) gives the height of the normal curve at 0)"),
      shiny::actionButton(shiny::NS(id, "pnorm"), "Pnorm", class = "add-button",
        title = "Cumulative distribution function for normal distribution (e.g., Pnorm(1) gives the probability that a random variable is less than 1)"),
      shiny::actionButton(shiny::NS(id, "qnorm"), "Qnorm", class = "add-button",
        title = "Quantile function for normal distribution (e.g., Qnorm(0.95) gives the value corresponding to the 95th percentile)"),
      shiny::actionButton(shiny::NS(id, "rnorm"), "Rnorm", class = "add-button",
        title = "Generate random numbers from a normal distribution (e.g., Rnorm(5) gives 5 random numbers from a normal distribution)"),
      shiny::actionButton(shiny::NS(id, "dbinom"), "Dbinom", class = "add-button",
        title = "Probability mass function for binomial distribution (e.g., Dbinom(2, 5, 0.5) gives the probability of getting exactly 2 successes in 5 trials with probability 0.5)"),
      shiny::actionButton(shiny::NS(id, "pbinom"), "Pbinom", class = "add-button",
        title = "Cumulative distribution function for binomial distribution (e.g., Pbinom(2, 5, 0.5) gives the probability of getting 2 or fewer successes in 5 trials)"),
      shiny::actionButton(shiny::NS(id, "qbinom"), "Qbinom", class = "add-button",
        title = "Quantile function for binomial distribution (e.g., Qbinom(0.95, 5, 0.5) gives the number of successes corresponding to the 95th percentile)"),
      shiny::actionButton(shiny::NS(id, "rbinom"), "Rbinom", class = "add-button",
        title = "Generate random numbers from a binomial distribution (e.g., Rbinom(5, 10, 0.5) gives 5 random binomial values with 10 trials and probability 0.5)"),
      shiny::actionButton(shiny::NS(id, "dpois"), "Dpois", class = "add-button",
        title = "Probability mass function for Poisson distribution (e.g., Dpois(3, 2) gives the probability of getting exactly 3 events when the average is 2)"),
      shiny::actionButton(shiny::NS(id, "ppois"), "Ppois", class = "add-button",
        title = "Cumulative distribution function for Poisson distribution (e.g., Ppois(3, 2) gives the probability of getting 3 or fewer events when the average is 2)"),
      shiny::actionButton(shiny::NS(id, "rpois"), "Rpois", class = "add-button",
        title = "Generate random numbers from a Poisson distribution (e.g., Rpois(5, 2) gives 5 random Poisson values with mean 2)"),
      shiny::actionButton(shiny::NS(id, "dunif"), "Dunif", class = "add-button",
        title = "Probability density function for uniform distribution (e.g., Dunif(0.5, min = 0, max = 1) gives the height of the uniform distribution at 0.5)"),
      shiny::actionButton(shiny::NS(id, "punif"), "Punif", class = "add-button",
        title = "Cumulative distribution function for uniform distribution (e.g., Punif(0.5, min = 0, max = 1) gives the probability of getting a value less than or equal to 0.5)"),
      shiny::actionButton(shiny::NS(id, "qunif"), "Qunif", class = "add-button",
        title = "Quantile function for uniform distribution (e.g., Qunif(0.95, min = 0, max = 1) gives the value corresponding to the 95th percentile)"),
      shiny::actionButton(shiny::NS(id, "runif"), "Runif", class = "add-button",
        title = "Generate random numbers from a uniform distribution (e.g., Runif(5, min = 0, max = 1) gives 5 random values between 0 and 1)"),

      class = "boxed-output"
    )
  )
}

OperatorEditorUI <- function(id) {
  ui <- shiny::fluidPage(
    htmltools::div(
      shiny::textAreaInput(shiny::NS(id, "editable_code"), "Operation:", value = "", rows = 12),
      class = "boxed-output"
    ),
    htmltools::br(),
    htmltools::div(
      class = "boxed-output",
      shiny::fluidRow(
        shiny::column(
          7,
          shiny::actionButton(shiny::NS(id, "run_op_intermediate"), "Run operation and store intermediate results"),

        ),
        shiny::column(
          4,
          shiny::textInput(shiny::NS(id, "iv"), "Intermediate variable name:", value = "")
        )
      )
    ),
    htmltools::br(),
    htmltools::div(
      class = "boxed-output",
      shiny::fluidRow(

        shiny::column(
          7,
          shiny::actionButton(shiny::NS(id, "run_op"), "Run operation and append to dataset")
        ),
        shiny::column(
          4,
          shiny::textInput(shiny::NS(id, "nc"), "New column name:", value = "")
        )
      )
    ),
    shiny::uiOutput(shiny::NS(id, "head")),
    shiny::uiOutput(shiny::NS(id, "intermediate_results"))
  )
}
