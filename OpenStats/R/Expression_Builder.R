data_wrangling_docs <- function() {
  list(
    "+" = "",
    "-" = "",
    "*" = "",
    "/" = "",
    "^" = "",
    log = "logarithm to the base e",
    log10 = "logarithm to the base 10",
    sqrt = "Square root",
    exp = "Exponential function (e^x) equate to exp(x)",
    sin = "", cos = "", tan = "", tanh = "",
    sinh = "", cosh = "", acos = "", asin = "", atan = "",
    as.numeric = "", as.character = "", as.logical = "",
    as.factor = "", as.integer = "",
    abs = "absolute value |x| equivalent to abs(x)",
    ceiling = "Round up to the nearest integer.",
    floor = "Round down to the nearest integer",
    trunc = "Truncate decimal part, keeping only the integer part",
    round = "Round to the nearest integer or specified precision",
    paste = "Join pieces of text (e.g., paste('Hello', 'World') gives 'Hello World').",
    paste0 = "Join pieces of text without spaces (e.g., paste0('Hello', 'World') gives 'HelloWorld'). This is very practical if you want to join two columns e.g. paste0(ColName1, ColName2)",
    tolower = "Convert text to lowercase (e.g., tolower('Hello') gives 'hello')",
    toupper = "Convert text to uppercase (e.g., toupper('hello') gives 'HELLO')",
    Mean = "Calculate the average of numbers (e.g., Mean(ColName))",
    SD = "SD(ColName) gives the standard deviation",
    Median = "Median(ColName) calculates the median",
    range = "",
    Sum = "Add up all the numbers Sum(ColName)",
    diff = "",
    Min = "Find the smallest number (e.g., Min(c(1, 2, 3)) gives 1)",
    Max = "Find the largest number (e.g., Max(c(1, 2, 3)) gives 3)",
    length = "Returns the number of elements",
    DataFrame = paste0(
      "Create a table (data frame) from one or more columns, e.g. DataFrame(Variable1, Variable2).\n",
      "By default each column keeps the name of the variable you pass in.\n",
      "Use a named argument to rename a column: DataFrame(height = Variable1, weight = Variable2) ",
      "creates columns named 'height' and 'weight'.\n",
      "Named and unnamed columns can be mixed, e.g. DataFrame(ratio = Variable1 / Variable2, Variable3)."
    ),
    C = paste0(
      "Combine values into a vector, e.g. C(1, 2, 3) gives a vector with the elements 1, 2 and 3.\n",
      "Name the elements with named arguments: C(low = 1, high = 10) gives a vector whose elements ",
      "are labelled 'low' and 'high'."
    ),
    Seq = "Create a sequence of elements (e.g. Seq(1, 10, 0.1) which creates a sequence starting from 1 to 10 in steps of 0.1).",
    get_rows = 'Filter by row. For example get_rows(df, ColName == "Control") or get_rows(df, colName == 10)',
    get_cols = "Extract column from a data frame (a table). For example get_cols(df, ColName) or get_cols(df, ColName1, ColName2)",
    get_elem = "Extract one element from a variable. This can either be ColName or a tabular dataset. In case it is a ColName the syntax is get_elem(ColName, idx) where idx is an integer number e.g. 1. In case one specific element of a dataset should be retrieved the syntax is get_elem(df, idx_row, idx_col). Again idx_row and idx_col have to be integers. The first one specifies the row number and the second one the column number.",
    as.char = "Convert a column of the dataset or an intermediate variable to character. For example as.char(ColName)",
    as.int = "Convert a column of the dataset or an intermediate variable to integer. For example as.int(ColName)",
    as.real = "Convert a column of the dataset or an intermediate variable to a real number. For example as.real(ColName)",
    as.fact = "Convert a column of the dataset or an intermediate variable to a factor. For example as.fact(ColName)",
    Dnorm = "Probability density function for normal distribution (e.g., Dnorm(0) gives the height of the normal curve at 0)",
    Pnorm = "Cumulative distribution function for normal distribution (e.g., Pnorm(1) gives the probability that a random variable is less than 1)",
    Qnorm = "Quantile function for normal distribution (e.g., Qnorm(0.95) gives the value corresponding to the 95th percentile)",
    Rnorm = "Generate random numbers from a normal distribution (e.g., Rnorm(5) gives 5 random numbers from a normal distribution)",
    Dbinom = "Probability mass function for binomial distribution (e.g., Dbinom(2, 5, 0.5) gives the probability of getting exactly 2 successes in 5 trials with probability 0.5)",
    Pbinom = "Cumulative distribution function for binomial distribution (e.g., Pbinom(2, 5, 0.5) gives the probability of getting 2 or fewer successes in 5 trials)",
    Qbinom = "Quantile function for binomial distribution (e.g., Qbinom(0.95, 5, 0.5) gives the number of successes corresponding to the 95th percentile)",
    Rbinom = "Generate random numbers from a binomial distribution (e.g., Rbinom(5, 10, 0.5) gives 5 random binomial values with 10 trials and probability 0.5)",
    Dpois = "Probability mass function for Poisson distribution (e.g., Dpois(3, 2) gives the probability of getting exactly 3 events when the average is 2)",
    Ppois = "Cumulative distribution function for Poisson distribution (e.g., Ppois(3, 2) gives the probability of getting 3 or fewer events when the average is 2)",
    Rpois = "Generate random numbers from a Poisson distribution (e.g., Rpois(5, 2) gives 5 random Poisson values with mean 2)",
    Dunif = "Probability density function for uniform distribution (e.g., Dunif(0.5, min = 0, max = 1) gives the height of the uniform distribution at 0.5)",
    Punif = "Cumulative distribution function for uniform distribution (e.g., Punif(0.5, min = 0, max = 1) gives the probability of getting a value less than or equal to 0.5)",
    Qunif = "Quantile function for uniform distribution (e.g., Qunif(0.95, min = 0, max = 1) gives the value corresponding to the 95th percentile)",
    Runif = "Generate random numbers from a uniform distribution (e.g., Runif(5, min = 0, max = 1) gives 5 random values between 0 and 1)"
  )
}
data_wrangling_specs <- function() {
  list(
    log = list(min_args = 1, max_args = 2),
    log10 = list(min_args = 1, max_args = 1),
    sqrt = list(min_args = 1, max_args = 1),
    exp = list(min_args = 1, max_args = 1),
    sin = list(min_args = 1, max_args = 1),
    cos = list(min_args = 1, max_args = 1),
    tan = list(min_args = 1, max_args = 1),
    tanh = list(min_args = 1, max_args = 1),
    sinh = list(min_args = 1, max_args = 1),
    cosh = list(min_args = 1, max_args = 1),
    acos = list(min_args = 1, max_args = 1),
    asin = list(min_args = 1, max_args = 1),
    atan = list(min_args = 1, max_args = 1),
    as.numeric = list(min_args = 1, max_args = 1),
    as.character = list(min_args = 1, max_args = 1),
    as.logical = list(min_args = 1, max_args = 1),
    as.factor = list(min_args = 1, max_args = 1),
    as.integer = list(min_args = 1, max_args = 1),
    abs = list(min_args = 1, max_args = 1),
    ceiling = list(min_args = 1, max_args = 1),
    floor = list(min_args = 1, max_args = 1),
    trunc = list(min_args = 1, max_args = 1),
    round = list(min_args = 1, max_args = 2),
    paste = list(min_args = 1),
    paste0 = list(min_args = 1),
    tolower = list(min_args = 1, max_args = 1),
    toupper = list(min_args = 1, max_args = 1),
    Mean = list(min_args = 1, max_args = 1),
    SD = list(min_args = 1, max_args = 1),
    Median = list(min_args = 1, max_args = 1),
    range = list(min_args = 1, max_args = 1),
    Sum = list(min_args = 1, max_args = 1),
    diff = list(min_args = 1, max_args = 1),
    Min = list(min_args = 1, max_args = 1),
    Max = list(min_args = 1, max_args = 1),
    length = list(min_args = 1, max_args = 1),
    DataFrame = list(min_args = 1),
    C = list(min_args = 1),
    Seq = list(min_args = 2, max_args = 3),
    get_rows = list(min_args = 2, max_args = 2),
    get_cols = list(min_args = 2, max_args = 1000),
    get_elem = list(min_args = 2, max_args = 1000)
  )
}
data_wrangling_operator_set <- function() {
  functions <- c(
    "log", "log10", "sqrt", "exp",
    "sin", "cos", "tan", "tanh", "sinh", "cosh", "acos", "asin", "atan",
    "as.numeric", "as.character", "as.logical", "as.factor", "as.integer",
    "abs", "ceiling", "floor", "trunc", "round",
    "paste", "paste0", "tolower", "toupper",
    "Dnorm", "Pnorm", "Qnorm", "Rnorm", "Dbinom",
    "Pbinom", "Qbinom", "Rbinom", "Dpois",
    "Ppois", "Rpois", "Dunif", "Punif", "Qunif", "Runif",
    "Mean", "SD", "Median", "range",
    "Sum", "diff", "Min", "Max",
    "C", "Seq", "DataFrame", "length",
    "get_rows", "get_cols", "get_elem",
    "as.char", "as.int", "as.real", "as.fact"
  )
  comparison <- c(">", "<", "<=", ">=", "==", "!=")
  list(
    operations_all = c("+", "-", "*", "/", "^", "(", ")", comparison, functions),
    expression_ops = c("+", "-"),
    term_ops = c("*", "/"),
    comparison_ops = comparison,
    function_ops = functions
  )
}
data_wrangling_palette <- function() {
  list(
    "Arithmetic" = list("+", "-", "*", "/", "^", "(", ")", ","),
    "Comparison" = list(">", "<", "<=", ">=", "==", "!="),
    "Math" = list("log", "log10", "sqrt", "exp", "sin", "cos", "tan", "tanh",
                  "sinh", "cosh", "acos", "asin", "atan", "abs", "ceiling",
                  "floor", "trunc", "round"),
    "Statistics" = list("Mean", "SD", "Median", "range", "Sum",
                        "diff", "Min", "Max"),
    "Strings" = list("paste", "paste0", "tolower", "toupper"),
    "Type conversion" = list("as.char", "as.int", "as.real", "as.fact"),
    "Construct" = list("C", "Seq", "DataFrame", "length"),
    "Getter" = list("get_elem", "get_rows", "get_cols"),
    "Distributions" = list("Dnorm", "Pnorm", "Qnorm", "Rnorm", "Dbinom",
                           "Pbinom", "Qbinom", "Rbinom", "Dpois", "Ppois",
                           "Rpois", "Dunif", "Punif", "Qunif", "Runif")
  )
}


linear_formula_docs <- function() {
  list(
    "+" = "Include an additional predictor variable in the model",
    "-" = "Removes an additional predictor variable in the model",
    "*" = "Multiply variables to assess interactions in the model",
    ":" = "Includes the interaction between two variables in the model"
  )
}
generalized_linear_formula_docs <- linear_formula_docs
linear_formula_specs <- function() {
  list()
}
generalized_linear_formula_specs <- linear_formula_specs 
linear_formula_operator_set <- function() {
  functions <- character(0)
  comparison <- character(0)
  list(
    operations_all = c("+", "-", "*", ":"),
    expression_ops = c("+", "-", ":"),
    term_ops = c("*"),
    comparison_ops = comparison,
    function_ops = functions
  )
}
generalized_linear_formula_operator_set <- linear_formula_operator_set 
linear_formula_palette <- function() {
  list(
    "Functions" = list("+", "-", "*", ":")
  )
}
generalized_linear_formula_palette <- linear_formula_palette 

linear_mixed_formula_docs <- function() {
  list(
    "+" = "Include an additional predictor variable in the model",
    "-" = "Removes an additional predictor variable in the model",
    "*" = "Multiply variables to assess interactions in the model",
    "|" = "Nesting"
  )
}
linear_mixed_formula_specs <- function() {
  list()
}
linear_mixed_formula_operator_set <- function() {
  functions <- character(0)
  comparison <- character(0)
  list(
    operations_all = c("+", "-", "*", "|"),
    expression_ops = c("+", "-", "|"),
    term_ops = c("*"),
    comparison_ops = comparison,
    function_ops = functions
  )
}
linear_mixed_formula_palette <- function() {
  list(
    "Functions" = list("+", "-", "*", "|")
  )
}

optim_formula_math_functions <- function() {
  c(
    "log", "log10", "sqrt", "exp",
    "sin", "cos", "tan", "tanh", "sinh", "cosh", "acos", "asin", "atan",
    "abs", "ceiling", "floor", "trunc", "round"
  )
}
optim_formula_docs <- function() {
  data_wrangling_docs()[c("+", "-", "*", "/", "^", optim_formula_math_functions())]
}
optim_formula_specs <- function() {
  data_wrangling_specs()[optim_formula_math_functions()]
}
optim_formula_operator_set <- function() {
  functions <- optim_formula_math_functions()
  list(
    operations_all = c("+", "-", "*", "/", "^", "(", ")", functions),
    expression_ops = c("+", "-"),
    term_ops = c("*", "/"),
    comparison_ops = character(0),
    function_ops = functions
  )
}
optim_formula_palette <- function() {
  list(
    "Arithmetic" = list("+", "-", "*", "/", "^", "(", ")", ","),
    "Math" = as.list(optim_formula_math_functions())
  )
}

expr_builder_ui <- function(id, operators, colnames) {
  target <- paste0(id, "-expr")
  op_buttons <- lapply(seq_len(length(operators)), function(section_id) {
    section <- operators[[section_id]]
    section_buttons <- lapply(section, function(op) {
      shiny::actionButton(
        paste0(id, "-", op), op,
        draggable = "true",
        `data-token` = op,
        `data-target` = target,
        ondragstart = "dragStart(event)",
        onclick = "addToken(event)",
        onmouseenter = "showDoc(event)",
        onmouseleave = "hideDoc(event)"
      )
    })
    htmltools::div(
      htmltools::h5(names(operators)[[section_id]]),
      do.call(htmltools::tagList, section_buttons),
      class = "boxed-output"
    )
  })

  col_buttons <- lapply(colnames, function(col) {
    shiny::actionButton(
      paste0(id, "-", col), col,
      draggable = "true",
      `data-token` = col,
      `data-target` = target,
      ondragstart = "dragStart(event)",
      onclick = "addToken(event)",
      onmouseenter = "showDoc(event)",
      onmouseleave = "hideDoc(event)"
    )
  })
  col_buttons <- htmltools::div(
    htmltools::h5("Variables"),
    do.call(htmltools::tagList, col_buttons),
    class = "boxed-output"
  )

  htmltools::div(
    class = "expr-layout-stacked",
    htmltools::div(
      id = paste0(id, "-expr_buttons"),
      class = "expr-buttons",
      do.call(htmltools::tagList, op_buttons),
      col_buttons
    ),
    htmltools::div(
      id = paste0(id, "-expr"),
      class = "expr-builder",
      `data-placeholder` = "Drop buttons here to build an expression…",
      ondragover = "allowDrop(event)",
      ondragleave = "dragLeave(event)",
      ondrop = "dropToken(event)"
    ),
    htmltools::div(
      class = "expr-suggest-wrap",
      htmltools::tags$input(
        id = paste0(id, "-expr-text"),
        type = "text",
        class = "expr-text",
        placeholder = "Type to build an expression…",
        autocomplete = "off"
      ),
      htmltools::div(
        id = paste0(id, "-expr-suggest"),
        class = "expr-suggest"
      )
    ),
    htmltools::div(
      id = paste0(id, "-expr-status"),
      class = "expr-status status-empty",
      "Status: "
    ),
    htmltools::div(
      id = paste0(id, "-expr-docs"),
      class = "expr-docs",
      htmltools::div(
        class = "expr-docs-header",
        htmltools::span("Documentation", class = "expr-docs-heading"),
        shiny::tags$button(
          "–",
          type = "button",
          class = "expr-docs-toggle",
          onclick = "toggleDocs(event)",
          `aria-label` = "Minimize documentation"
        )
      ),
      htmltools::div(
        id = paste0(id, "-expr-docs-content"),
        class = "expr-docs-content",
        "Hover over a button to see its documentation."
      )
    )
  )
}

data_wrangling_expr_builder_ui <- function(id, operators, colnames, df_name = NULL, intermediate = NULL) {
  target <- paste0(id, "-expr")
  op_buttons <- lapply(seq_len(length(operators)), function(section_id) {
    section <- operators[[section_id]]
    section_buttons <- lapply(section, function(op) {
      shiny::actionButton(
        paste0(id, "-", op), op,
        draggable = "true",
        `data-token` = op,
        `data-target` = target,
        ondragstart = "dragStart(event)",
        onclick = "addToken(event)",
        onmouseenter = "showDoc(event)",
        onmouseleave = "hideDoc(event)"
      )
    })
    htmltools::div(
      htmltools::h5(names(operators)[[section_id]]),
      do.call(htmltools::tagList, section_buttons),
      class = "boxed-output"
    )
  })

  col_buttons <- lapply(colnames, function(col) {
    shiny::actionButton(
      paste0(id, "-", col), col,
      class = if (identical(col, df_name)) "df-button"
              else if (col %in% intermediate) "iv-button"
              else "colnames-button",
      draggable = "true",
      `data-token` = col,
      `data-target` = target,
      ondragstart = "dragStart(event)",
      onclick = "addToken(event)",
      onmouseenter = "showDoc(event)",
      onmouseleave = "hideDoc(event)"
    )
  })
  col_buttons <- htmltools::div(
    htmltools::h5("Variables"),
    do.call(htmltools::tagList, col_buttons),
    class = "boxed-output"
  )

  htmltools::div(
    class = "expr-layout-dw",
    htmltools::div(
      id = paste0(id, "-expr_buttons"),
      class = "expr-buttons",
      col_buttons,
      htmltools::div(
        class = "expr-op-grid",
        do.call(htmltools::tagList, op_buttons)
      )
    ),
    htmltools::div(
      class = "expr-right",
      htmltools::div(
        id = paste0(id, "-expr"),
        class = "expr-builder",
        `data-placeholder` = "Drop buttons here to build an expression…",
        ondragover = "allowDrop(event)",
        ondragleave = "dragLeave(event)",
        ondrop = "dropToken(event)"
      ),
      htmltools::div(
        class = "expr-suggest-wrap",
        htmltools::tags$input(
          id = paste0(id, "-expr-text"),
          type = "text",
          class = "expr-text",
          placeholder = "Type to build an expression…",
          autocomplete = "off"
        ),
        htmltools::div(
          id = paste0(id, "-expr-suggest"),
          class = "expr-suggest"
        )
      ),
      htmltools::div(
        id = paste0(id, "-expr-status"),
        class = "expr-status status-empty",
        "Status: "
      ),
      htmltools::div(
        id = paste0(id, "-expr-docs"),
        class = "expr-docs",
        htmltools::div(
          class = "expr-docs-header",
          htmltools::span("Documentation", class = "expr-docs-heading"),
          shiny::tags$button(
            "–",
            type = "button",
            class = "expr-docs-toggle",
            onclick = "toggleDocs(event)",
            `aria-label` = "Minimize documentation"
          )
        ),
        htmltools::div(
          id = paste0(id, "-expr-docs-content"),
          class = "expr-docs-content",
          "Hover over a button to see its documentation."
        )
      )
    )
  )
}

# Push the builder payload to the "expr" input of the current module session.
# Standalone so callers with model-type-dependent config can send it from inside
# their own moduleServer (where the model_type input lives).
expr_send_payload <- function(session, state, check_variables, operator_set, docs, specs,
                              variables = names(state$df), df_doc = NULL) {
  types <- lapply(state$df[variables], function(col) {
    if (!is.numeric(col)) return(paste(class(col), collapse = " "))
    qs <- tryCatch(quantile(col, na.rm = TRUE), error = function(e) NULL)
    if (is.null(qs)) return(paste(class(col), collapse = " "))
    paste0(
      "numeric;", "\n",
      "min: ", min(col, na.rm = TRUE), " max: ", max(col, na.rm = TRUE), "\n",
      "median: ", median(col, na.rm = TRUE), " mean: ", round(mean(col, na.rm = TRUE), 3), "\n",
      "sd: ", round(sd(col, na.rm = TRUE), 3), "\n",
      "1.Quartile: ", qs[[2L]], " 3.Quartile: ", qs[[4L]]
    )
  }) |> unname()
  send_docs <- docs
  if (!is.null(df_doc) && !is.null(state$df_name)) {
    send_docs[[state$df_name]] <- df_doc
  }
  session$sendInputMessage("expr", list(
    variables = variables,
    checkVariables = check_variables,
    docs = send_docs,
    columnTypes = types,
    operatorSet = operator_set,
    specs = specs
  ))
}

expr_builder_server <- function(id, state, refresh = shiny::reactive(NULL), check_variables, operator_set, docs, specs, df_doc = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    send <- function() {
      expr_send_payload(session, state, check_variables, operator_set, docs, specs, df_doc = df_doc)
    }
    # Send on data/refresh changes, and whenever the binding (re)mounts and
    # announces itself (handles modal reopen / tab switch).
    shiny::observeEvent(list(state$df, refresh()), send(), ignoreNULL = FALSE)
    shiny::observeEvent(input[["expr-ready"]], send(), ignoreNULL = TRUE)
  })
}
