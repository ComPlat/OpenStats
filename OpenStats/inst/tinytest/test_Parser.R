library(tinytest)
if (!requireNamespace("V8", quietly = TRUE)) exit_file("needs V8")

ctx <- V8::v8()
ctx$eval("var Shiny = { InputBinding: function(){}, inputBindings: { register: function(){} } };")
ctx$eval("var $ = { extend: function(t, s){ for (var k in s) { t[k] = s[k]; } return t; } };")
ctx$source(system.file("www/parser.js", package = "OpenStats"))
ctx$source(system.file("www/expression.js", package = "OpenStats"))

ops <- OpenStats:::data_wrangling_operator_set()
sp  <- OpenStats:::data_wrangling_specs()

ctx$eval("function pflat(str, vars, o, s, c) { return parse(tokenize(str), vars, o, s, c).to_flat_string(); }")
tok  <- function(s) unlist(ctx$call("tokenize", s))
flat <- function(s, vars = list(), check = TRUE) ctx$call("pflat", s, vars, ops, sp, check)

# tokenize
# =================================================================
expect_equal(tok("Mean(uptake)"), c("Mean", "(", "uptake", ")"), info = "tokenize call")
expect_equal(tok("a <= b"), c("a", "<=", "b"), info = "tokenize multi-char op")
expect_equal(tok("x==y"), c("x", "==", "y"), info = "tokenize == without spaces")
expect_equal(tok("a:b"), c("a", ":", "b"), info = "tokenize colon")
expect_equal(tok("as.numeric(conc)"), c("as.numeric", "(", "conc", ")"), info = "tokenize dotted name")

# parse: valid expressions round-trip through to_flat_string
# =================================================================
expect_equal(flat("Mean(uptake)", list("uptake")), "Mean(uptake)", info = "function call")
expect_equal(flat("a + b * c", list("a", "b", "c")), "a + b * c", info = "precedence")
expect_equal(flat("conc > 700", list("conc")), "conc > 700", info = "comparison")
expect_equal(flat("DataFrame(x = conc)", list("conc")), "DataFrame(x = conc)", info = "named argument")
expect_equal(flat("( a + b )", list("a", "b")), "(a + b)", info = "parentheses")
expect_equal(flat("get_cols(df, conc, uptake)", list("df", "conc", "uptake")),
  "get_cols(df, conc, uptake)", info = "multi-arg getter")

# parse: errors surface in the flattened string
# =================================================================
expect_true(grepl("Too many arguments to: Mean", flat("Mean(a, b)", list("a", "b"))), info = "arity too many")
expect_true(grepl("Too less arguments to: Seq", flat("Seq(a)", list("a"))), info = "arity too few")
expect_true(grepl("Invalid variable: foo", flat("foo", list("a"))), info = "invalid variable")
expect_true(grepl("ParseError", flat("Mean(a", list("a"))), info = "unbalanced paren")

# check_variables toggles variable validation
# =================================================================
expect_equal(flat("foo + 1", list(), FALSE), "foo + 1", info = "variable check disabled")
expect_true(grepl("Invalid variable: foo", flat("foo + 1", list("a"), TRUE)), info = "variable check enabled")
