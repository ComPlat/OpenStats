// Operator sets and function arity specs are supplied by R (per mode) and
// reach the parser through parse(); nothing operator-specific is hardcoded here.

function within(token, arr) {
  for (let i = 0; i < arr.length; i++) {
    if (token === arr[i]) {
      return true;
    }
  }
  return false;
}

function is_identifier(token) { // stuff like DataFrame(a = 1)
  return typeof token === "string" && /^[A-Za-z.][A-Za-z0-9._]*$/.test(token);
}

class ParseError {
  constructor(message) {
    this.type = "ParseError";
    this.message = message;
    this.wrapped_in_paranthesis = false;
  }
  to_string(indent = "") {
    return indent + `ParseError(${this.message})`;
  }
  to_flat_string() {
    if (this.wrapped_in_paranthesis) return "(" + this.message + ")"
    return ` ==> ParseError(${this.message})`;
  }
}

class Variable {
  constructor(variable) {
    this.type = "Variable";
    this.variable = variable;
    this.wrapped_in_paranthesis = false;
    this.error_message = null;
  }
  to_string(indent = "") {
    return indent + `Variable(${this.variable})`;
  }
  to_flat_string() {
    let expr = this.variable;
    if (this.error_message !== null) {
      expr = new ParseError(this.variable + " " + this.error_message);
      expr = expr.to_flat_string();
    }
    if (this.wrapped_in_paranthesis) return "(" + expr + ")"
    return expr;
  }
}

const is_numeric = (string) => Number.isFinite(+string)
class ConstantNumber {
  constructor(number) {
    this.type = "ConstantNumber";
    this.number = number;
    this.wrapped_in_paranthesis = false;
  }
  to_string(indent = "") {
    return indent + `ConstantNumber(${this.number})`;
  }
  to_flat_string() {
    if (this.wrapped_in_paranthesis) return "(" + this.number + ")"
    return this.number;
  }
}

class Binary {
  constructor(operation, left, right) {
    this.type = "Binary";
    this.operation = operation;
    this.left = left;
    this.right = right;
    this.wrapped_in_paranthesis = false;
  }
  to_string(indent = "") {
    return `${indent}Binary(${this.operation})\n` +
      this.left.to_string(indent + "  ") + "\n" +
      this.right.to_string(indent + "  ");
  }
  to_flat_string() {
    const l = this.left.to_flat_string();
    const r = this.right.to_flat_string();
    const expr = l + " " + this.operation + " " + r;
    if (this.wrapped_in_paranthesis) return "(" + expr + ")"
    return expr;
  }
}

class Unary {
  constructor(operation, obj) {
    this.type = "Unary";
    this.operation = operation;
    this.obj = obj;
    this.wrapped_in_paranthesis = false;
  }
  to_string(indent = "") {
    return `${indent}Unary(${this.operation})\n` +
      this.obj.to_string(indent + "  ");
  }
  to_flat_string() {
    const o = this.obj.to_flat_string();
    let expr = this.operation + o;
    if (this.operation === "-") {
      expr = "-" + o;
    }
    if (this.wrapped_in_paranthesis) return "(" + expr + ")"
    return expr;
  }
}

class FunctionCall {
  constructor(name, args) {
    this.type = "FunctionCall";
    this.name = name;
    this.args = args;
    this.wrapped_in_paranthesis = false;
    this.error_message = null;
  }
  to_string(indent = "") {
    return `${indent}FunctionCall(${this.name})\n` +
      this.args.map(arg => arg.to_string(indent + "  ")).join("\n");
  }
  to_flat_string() {
    const args = this.args.map(arg => arg.to_flat_string()).join(", ");
    let expr = `${this.name}(${args})`;
    if (this.error_message !== null) {
      expr = new ParseError(expr + " " + this.error_message);
      expr = expr.to_flat_string();
    }
    if (this.wrapped_in_paranthesis) return "(" + expr + ")";
    return expr;
  }
}

class NamedArgument {
  constructor(name, value) {
    this.type = "NamedArgument";
    this.name = name;
    this.value = value;
    this.wrapped_in_paranthesis = false;
  }
  to_string(indent = "") {
    return `${indent}NamedArgument(${this.name})\n` +
      this.value.to_string(indent + "  ");
  }
  to_flat_string() {
    return this.name + " = " + this.value.to_flat_string();
  }
}

class Parser {
  constructor(tokens, operator_set) {
    this.tokens = tokens;
    this.pos = 0;
    this.operations = operator_set.operations_all;
    this.terms = operator_set.term_ops;
    this.expressions = operator_set.expression_ops;
    this.comparisons = operator_set.comparison_ops;
    this.functions = operator_set.function_ops;
  }
  current() {
    return this.tokens[this.pos];
  }
  consume() {
    return this.tokens[this.pos++];
  }
  peek(offset = 1) {
    return this.tokens[this.pos + offset];
  }
  parse_factor() {
    const token = this.current();
    if (token === "(") {
      this.consume(); // (
      const expr = this.parse_comparison();
      if (this.current() !== ")") {
        const expr_flat_string = expr.to_flat_string();
        const res = new ParseError("(" + expr_flat_string + " Expected ')'");
        return res;
      }
      this.consume(); // )
      expr.wrapped_in_paranthesis = true;
      return expr;
    }
    if (within(token, this.functions)) {
      return this.parse_function_call();
    }
    if (!within(token, this.operations) && typeof token !== 'undefined') {
      this.consume();
      if (is_numeric(token)) {
        return new ConstantNumber(token);
      } else {
        return new Variable(token);
      }
    }
    return new ParseError("Expected variable");
  }
  parse_argument() {
    if (is_identifier(this.current()) && this.peek(1) === "=") {
      const name = this.consume(); // label
      this.consume(); // =
      const value = this.parse_comparison();
      return new NamedArgument(name, value);
    }
    return this.parse_comparison();
  }
  parse_function_call() {
    const name = this.consume();
    if (this.current() !== "(") {
      return new ParseError(`Expected '(' after function ${name}`);
    }
    this.consume(); // (
    const args = [];
    if (this.current() !== ")") {
      args.push(this.parse_argument());
      while (this.current() === ",") {
        this.consume(); // ,
        args.push(this.parse_argument());
      }
    }
    if (this.current() !== ")") {
      const temp = args.map(arg => arg.to_flat_string()).join(", ");
      const expr = `${name}(${temp}`;
      const res = new ParseError(`Expected ')' after '${expr}'`);
      return res;
    }
    this.consume(); // )
    return new FunctionCall(name, args);
  }
  parse_power() {
    let left = this.parse_factor();
    if (this.current() === "^") {
      const op = this.consume();
      const right = this.parse_power(); // recursion as it is right associative
      left = new Binary(op, left, right);
    }
    return left;
  }
  parse_unary() {
    if (this.current() === "-") {
      const op = this.consume();
      const expr = this.parse_power();
      return new Unary(op, expr);
    }
    return this.parse_power();
  }
  parse_term() {
    let left = this.parse_unary();
    while(within(this.current(), this.terms)) {
      const op = this.consume();
      const right = this.parse_unary();
      left = new Binary(op, left, right);
    }
    return left;
  }
  parse_expression() {
    let left = this.parse_term();
    while(within(this.current(), this.expressions)) {
      const op = this.consume();
      const right = this.parse_term();
      left = new Binary(op, left, right);
    }
    return left;
  }
  parse_comparison() {
    let left = this.parse_expression();
    while (within(this.current(), this.comparisons)) {
      const op = this.consume();
      const right = this.parse_expression();
      left = new Binary(op, left, right);
    }
    return left;
  }
  parse() {
    const ast = this.parse_comparison();
    if (this.pos < this.tokens.length) {
      const token = this.current();
      return new ParseError(
        `Unexpected token '${token}' after complete expression. ` +
          `Maybe an operator is missing before '${token}'?`
      );
    }
    return ast;
  }

}

function traverse_arity(AST, specs) {
  if (AST.type === "Unary") {
    traverse_arity(AST.obj, specs);
  }
  else if (AST.type === "Binary") {
    traverse_arity(AST.left, specs);
    traverse_arity(AST.right, specs);
  }
  else if (AST.type === "NamedArgument") {
    traverse_arity(AST.value, specs);
  }
  else if (AST.type === "FunctionCall") {
    const arity = specs[AST.name];
    const arity_got = AST.args.length;
    if (arity) {
      if (arity_got < arity.min_args) {
        AST.error_message = "Too less arguments to: " + AST.name;
      }
      if (arity_got > arity.max_args) {
        AST.error_message = "Too many arguments to: " + AST.name;
      }
    }
    AST.args.map((arg) => traverse_arity(arg, specs));
  }
}

function traverse_variables(AST, variables) {
  if (AST.type === "Variable") {
    if (!within(AST.variable, variables)) {
      AST.error_message = "Invalid variable: " + AST.variable;
    }
  }
  else if (AST.type === "Unary") {
    traverse_variables(AST.obj, variables);
  }
  else if (AST.type === "Binary") {
    traverse_variables(AST.left, variables);
    traverse_variables(AST.right, variables);
  }
  else if (AST.type === "NamedArgument") {
    traverse_variables(AST.value, variables); // name is a label, not a variable
  }
  else if (AST.type === "FunctionCall") {
    AST.args.map((arg) => traverse_variables(arg, variables));
  }
}

function parse(tokens, variables, operator_set, specs, check_variables = true) {
  const p = new Parser(tokens, operator_set);
  const result = p.parse();
  traverse_arity(result, specs);
  // Optimization parameters and named args in DataFrame etc. are not necessarily contained in variables
  if (check_variables) {
    traverse_variables(result, variables);
  }
  return result;
}
